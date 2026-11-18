unit Commands.Backup;

interface
uses
  Classes, SysUtils, Generics.Defaults, Generics.Collections, UCommandLine;

procedure RegisterBackupCommand;


implementation
uses IOUtils, Commands.CommonOptions, Commands.Logging, Commands.GlobalConfig,
     Snapshot.Writer, Zip, UTmsBuildSystemUtils, Status.Manager, UMultiLogger, Deget.Version,
     Downloads.FileNameManager, Actions.Fetch, Fetching.ProductVersion, Fetching.FetchExtraction,
     UProjectLoader, UProjectAnalyzer, Deget.CoreTypes, Deget.IDETypes, UProjectDefinition, Downloads.VersionManager;

var OptionFileName: string;
var OptionAsInstalled: boolean;
var OptionInclude: TArray<string>;
var OptionExclude: TArray<string>;
var OptionBundlesOnly: boolean;

procedure EnsureProducts(const Products: TList<TProductStatus>);
begin
  ExecuteFetchAction(OptionInclude, TFetchMode.DownloadNew, TFetchExtraction.Create(true, OptionBundlesOnly,
    procedure (const ProductId, BundleFileName, Server: string; const Version: TLenientVersion)
    begin
      Products.Add(TProductStatus.Create);
      var Product := Products.Last;
      Product.Id := ProductId;
      Product.Version := Version;
      Product.InstalledVersion := Version;
      Product.Server := Server;
      Product.Pinned := false;
      Product.BundleFileName := BundleFileName;

      //This would be needed to get the url in the snapshot. But it is a lot of work just to show a url in the snapshot.
      //Product.Project := TProjectLoader.LoadProjectDefinitionFromCompressedFile(BundleFileName);
    end), false
  );
end;

procedure AddPlatFolders(const PackagePath: string; const List: THashSet<string>);
begin
  for var IDEName := Low(TIDEName) to High(TIDEName) do
  begin
    for var Plat := Low(TPlatform) to High(TPlatform) do List.Add(TPath.Combine(PackagePath, PlatformMacroString(IDEName, Plat)));
  end;
end;

procedure AddIDEFolders(const PackagePath: string; const List: THashSet<string>);
begin
  for var IDEName := Low(TIDEName) to High(TIDEName) do  //even if we need delphi11 to support + folders, we can have a XE8\16.0 if there are both dXE and D11+ folders.
  begin
    List.Add(TPath.Combine(PackagePath, DelphiProductVersion[IDEName]));
  end;
end;


function GetAllPackageBinFolders(const PackagePaths: TList<string>): THashSet<string>;
begin
  Result := THashSet<string>.Create(TIStringComparer.Ordinal);
  try
    for var PackagePath in PackagePaths do
    begin
      AddIDEFolders(PackagePath, Result);
      AddPlatFolders(PackagePath, Result);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function ZipContents(const Product: TProductStatus): string;
begin
  Logger.Info('Compressing existing product: ' + Product.Id);
  // We need to skip binaries. Still, just not backing up dcus wouldn't work for binary
  // distributions like webcore. Luckily, those dcus are not in the packages folder, so
  // we can safely ignore whatever is in the packages folder.
  Result := '';
  var Zip := TZipFile.Create;
  try
    var ZipFileName := TDownloadFileName.GenerateFileName(Product.Id, Product.InstalledVersion) + '.zip';
    var FullZipFileName := TPath.Combine(Config.Folders.TempBackupFolder, 'as-installed', ZipFileName);
    var ListOfPackageFolders := TList<string>.Create;
    try
      TProjectAnalyzer.LocatePackagesFolder(Config, Product.Project, ListOfPackageFolders);
      var AllPackageBinFolders := GetAllPackageBinFolders(ListOfPackageFolders);
      try
        TDirectory_CreateDirectory(TPath.GetDirectoryName(FullZipFileName));
        Zip.Open(FullZipFileName, TZipMode.zmWrite);
        ScanFiles(Product.InstallFolder, ['*'], [], ['*'], [],
          procedure(FileName, RelPath: string)
          begin
            Zip.Add(FileName, RelPath);
          end, true, false,
          function(Folder: string): boolean
          begin
            Result := AllPackageBinFolders.Contains(TPath.GetFullPath(Folder));
          end);
      finally
        AllPackageBinFolders.Free;
      end;
    finally
      ListOfPackageFolders.Free;
    end;
    Result := FullZipFileName;
  finally
    Zip.Free;
  end;
end;

procedure BackupAsInstalled(const NewProducts: TList<TProductStatus>; const ExistingProducts: TList<TProductStatus>);
begin
  var ProductIds := ParseVersions(OptionInclude);
  if Length(ProductIds) > 0 then
  begin
    Config.ReplaceIncludedComponents(Length(ProductIds), function(index: integer): string begin Result := ProductIds[index].IdMask; end);
  end;

  for var i := ExistingProducts.Count - 1 downto 0 do
  begin
    var Msg := Config.ExcludedMsg(ExistingProducts[i].Id, 'Ignoring %s');
    if Msg <> '' then Logger.Trace(Msg);
    if not Config.IsIncluded(ExistingProducts[i].Id) then continue;
    //only backup products in \products, as those are are the only ones tms restore can restore.
    if not SameFileName(
      IncludeTrailingPathDelimiter(TPath.GetDirectoryName(ExistingProducts[i].InstallFolder)),
      IncludeTrailingPathDelimiter(Config.Folders.ProductsFolder))
      then continue;

    NewProducts.Add(ExistingProducts.ExtractAt(i));
    NewProducts.Last.BundleFileName := ZipContents(NewProducts.Last);

  end;
end;


function GetProducts: TList<TProductStatus>;
begin
  Config.SetCmdExcludedProjects(OptionExclude);

  Result := TObjectList<TProductStatus>.Create;
  try
    if not OptionAsInstalled then
    begin
      EnsureProducts(Result);
    end
    else
    begin
      var ExistingProductsManager := TStatusManager.Create(Config);
      try
        ExistingProductsManager.Update;
        BackupAsInstalled(Result, ExistingProductsManager.Products);
      finally
        ExistingProductsManager.Free;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure SaveProducts(const Products: TList<TProductStatus>; const Zip: TZipFile);
begin
  for var Product in Products do
  begin
    if Product.BundleFileName = '' then
    begin
      Logger.Info('Skipping product ' + Product.Id + ':' + Product.Version.ToString + '. It will be added to the snapshot, but not to the backup file.');
    end
    else
    begin
      Logger.Info('Adding product ' + Product.Id + ':' + Product.Version.ToString);
      Zip.Add(Product.BundleFileName, TPath.GetFileName(Product.BundleFileName), TZipCompression.zcStored);
    end;
  end;
end;

procedure RunBackupCommand;
begin
  InitFolderBasedCommand(true);
  if OptionAsInstalled and OptionBundlesOnly then raise Exception.Create('Option "-bundles-only" can only be specified if "-as-installed" is not.');
  if (not OptionAsInstalled) and (Length(OptionInclude) = 0) then raise Exception.Create('You must specify the products to backup with "-include", or use "-as-installed" to backup what you have installed.');

  var DestFolder := TPath.GetDirectoryName(OptionFileName); //save in same disk, so we can rename
  var TempFileName := TPath.GetFullPath(TPath.Combine(DestFolder, GUIDToStringN(TGuid.NewGuid) + '.zip' + TempExtension));
  var BackupFileName := TPath.GetFullPath(TPath.Combine(DestFolder, GUIDToStringN(TGuid.NewGuid) + '.zip' + TempExtension));
  Logger.Info('Creating backup file: "' + OptionFileName + '"...');
  TDirectory_CreateDirectory(TPath.GetDirectoryName(TempFileName));
  try
    var Zip := TZipFile.Create;
    try
      Zip.Open(TempFileName, TZipMode.zmWrite);
      var Products := GetProducts;
      try
        Zip.Add(GetSnapshot(Products), 'tms.snapshot.yaml');
        SaveProducts(Products, Zip);
      finally
        Products.Free;
      end;
    finally
      Zip.Free;
    end;
    AtomicMove(TempFileName, OptionFileName, BackupFileName);
  finally
    SysUtils.DeleteFile(TempFileName);
    SysUtils.DeleteFile(BackupFileName);
  end;
  RotateDownloads(Config.MaxVersionsPerProduct);
  Logger.Info('Backup file: "' + OptionFileName + '" created');
end;

procedure RegisterBackupCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('backup', '','save products into a backup file, that can be recovered with tms restore.',
    'More information: https://doc.tmssoftware.com/smartsetup/reference/tms-backup.html',
    'backup <filename> [options]');

  cmd.Examples.Add('backup c:\test\backup.zip -include:tms.* -exclude:tms.fnc.uipack');
  cmd.Examples.Add('backup c:\test\backup.zip -include:tms.flexcel.vcl:7.27 tms.biz.xdata');
  cmd.Examples.Add('backup c:\test\backup.zip -as-installed');

  var option := cmd.RegisterUnnamedOption<string>('File where to save the backup', 'filename',
    procedure(const Value : string)
    begin
      OptionFileName := Value;
    end);
  option.Required := True;

  option := cmd.RegisterOption<string>('include', '', 'specify what components to put in the backup. If you specify a component not installed and "-as-installed" is not specified, it will be downloaded and put in the backup. Dependencies will be added if not specified. If "-as-installed" is specified, only the installed components you specify will be added, without dependencies, and components that are not installed will be ignored.',
    procedure(const Value : string)
    begin
      OptionInclude := OptionInclude + [Value.Trim];
    end);
  option.AllowMultiple := true;

  option := cmd.RegisterOption<string>('exclude', '', 'specify what components to avoid putting in the backup. If not specified, no component will be excluded.',
    procedure(const Value : string)
    begin
      OptionExclude := OptionExclude + [Value.Trim];
    end);
  option.AllowMultiple := true;

  option := cmd.RegisterOption<Boolean>('as-installed', '', 'backup from what you have installed in your disk. If not specified, we will backup the official releases, and not include modifications you might have made to the components.',
    procedure(const Value: Boolean)
    begin
      OptionAsInstalled := Value;
    end);
  option.HasValue := False;

  option := cmd.RegisterOption<Boolean>('bundles-only', '', 'If true products from Git or SVN won''t be added to the backup file. Those products will still be added to the snapshot so they will be restored, but from github or the server, not from the backup file. This option cannot be set if "-as-installed" is also set.',
    procedure(const Value: Boolean)
    begin
      OptionBundlesOnly := Value;
    end);
  option.HasValue := False;

  AddCommand(cmd.Name, CommandGroups.Install, RunBackupCommand);
end;

end.

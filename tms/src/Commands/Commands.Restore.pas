unit Commands.Restore;
interface
uses
  Classes, System.Generics.Collections, System.SysUtils, UCommandLine;

procedure RegisterRestoreCommand;

implementation
uses
  Commands.CommonOptions, Commands.Logging, Commands.GlobalConfig, Status.Manager, Deget.CoreTypes,
  Deget.Version, Snapshot.Reader, Actions.Fetch, Actions.Build, Commands.Pin, Masks,
  UGenericDecompressor, System.Zip, Downloads.FileNameManager, IOUtils, UMultiLogger,
  Fetching.ParallelDownloader, UTmsBuildSystemUtils;

type
  TBackupData = class
  public
    HasSnapshot: boolean;
    ZipFile: string;
    Products: TObjectList<TProductStatus>;
    NameToIndexMap: TDictionary<string, integer>;
    NameToProductMap: TDictionary<string, TProductStatus>;
    constructor Create;
    destructor Destroy; override;

    function ProductInBackupList(const ProductId: string; const ProductVersion: TLenientVersion; out BackupIndex: integer): boolean;
  end;

  TBackupSingleFile = record
  public
    Index: integer;
    ProductId: string;
    Version: TLenientVersion;
    Server: string;

    constructor Create(const aIndex: integer; aProductId: string; aVersion: TLenientVersion; const aServer: string);
  end;

procedure RestoreFromBackup(const BackupFile: string; const ProductsToGetFromBackup: TArray<TBackupSingleFile>);
begin
  var zip := TZipFile.Create;
  try
    zip.Open(BackupFile, TZipMode.zmRead);

    for var SingleFile in ProductsToGetFromBackup do
    begin
      if Config.IsExcluded(SingleFile.ProductId) then
      begin
        Logger.Info('Skipped restoring "' + zip.FileName[SingleFile.Index] + '" from "' + TPath.GetFileName(BackupFile) + '" because it is in excluded products...');
        continue;
      end;
      Logger.Info('Restoring "' + zip.FileName[SingleFile.Index] + '" from "' + TPath.GetFileName(BackupFile) + '"...');
      var TempFileName := TPath.Combine(Config.Folders.ProductsTempFolder, TPath.GetGUIDFileName());
      try
        TDirectory_CreateDirectory(TPath.GetDirectoryName(TempFileName));
        //TZipFile.Extract follows symlinks and ends up being more clumsy to use.
        //zip.Extract(SingleFile.Index, TempFolder, false);
        var LocalHeader: TZipHeader;
        var zStream: TStream := nil;
        try
          zip.Read(SingleFile.Index, zStream, LocalHeader);
          var outStream := TFileStream.Create(TempFileName, fmCreate);
          try
            outStream.CopyFrom(zStream);
          finally
            outStream.Free;
          end;
        finally
          zStream.Free;
        end;


        TParallelDownloader.ExtractFile(Config.Folders, TempFileName, TDownloadFormat.Unknown, SingleFile.ProductId, SingleFile.Version, SingleFile.Server);
      finally
        DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, TempFileName);
      end;
    end;
  finally
    zip.Free;
  end;
end;

procedure Install(const ProductsToFetch: TArray<string>;
    const BackupFile: string; const ProductsToGetFromBackup: TArray<TBackupSingleFile>;
    const ProductsToPin, ProductsToUnpin, ProductsToBuild: TArray<string>);
begin
  if ProductsToGetFromBackup <> nil then
  begin
    RestoreFromBackup(BackupFile, ProductsToGetFromBackup);
  end;

  if ProductsToFetch <> nil then
  begin
    ExecuteFetchAction(ProductsToFetch, TFetchMode.DownloadNew);
  end;

  if ProductsToPin <> nil then
  begin
    DoPin(ProductsToPin, true);
  end;

  if ProductsToUnpin <> nil then
  begin
    DoPin(ProductsToUnpin, false);
  end;

  if ProductsToBuild <> nil then
  begin
    ExecuteBuildAction(ProductsToBuild, False);
  end;
end;

function Ignore(const ProductId: string; const IncludeMasks, ExcludeMasks: TArray<string>): boolean;
begin
  for var Mask in ExcludeMasks do
  begin
    if MatchesMask(ProductId, Mask) then exit(true);
  end;

  if IncludeMasks = nil then exit(false);
  for var Mask in IncludeMasks do
  begin
    if MatchesMask(ProductId, Mask) then exit(false);
  end;
  Result := true;

end;

procedure LoadSnapshotFromZip(const zip: TZipFile; const idx: integer; const Products: TList<TProductStatus>; const DisplayFileName: string);
begin
  var SnapshotBytes: TBytes;
  zip.Read(idx, SnapshotBytes);
  if SnapshotBytes <> nil then
  begin
    var SnapshotStream := TMemoryStream.Create;
    try
      SnapshotStream.WriteBuffer(SnapshotBytes[0], Length(SnapshotBytes));
      SnapshotStream.Position := 0;
      var SnapshotStreamReader := TStreamReader.Create(SnapshotStream, TEncoding.UTF8);
      try
        LoadSnapshot(SnapshotStreamReader, DisplayFileName, Products);
      finally
        SnapshotStreamReader.Free;
      end;
    finally
      SnapshotStream.Free;
    end;
  end;

end;

procedure LoadProductsFromZip(const zip: TZipFile; const Backup: TBackupData);
begin
  for var i := 0 to zip.FileCount - 1 do
  begin
    var NameOnly: string;
    if TDownloadFileName.TryGetFileNameWithoutExtension(TPath.GetFileName(zip.FileName[i]), NameOnly) then
    begin
      var ProductId: string;
      var ProductVersion:TLenientVersion;
      if TDownloadFileName.TryParse(NameOnly + '.zip', ProductId, ProductVersion) and not (ProductId.StartsWith('tms.smartsetup.', true)) then
      begin
        var BackupFileName := TDownloadFileName.GenerateFileName(ProductId, ProductVersion);
        if (not Backup.NameToIndexMap.ContainsKey(BackupFileName)) then
        begin
          Backup.NameToIndexMap.Add(BackupFileName, i);
        end;

        if not Backup.HasSnapshot then //autocreate a snapshot from the filenames.
        begin
          var ExistingProduct: TProductStatus;
          if not Backup.NameToProductMap.TryGetValue(ProductId, ExistingProduct) then
          begin
            Backup.Products.Add(TProductStatus.Create());
            var NewProduct := Backup.Products.Last;
            NewProduct.Id := ProductId;
            NewProduct.Version := ProductVersion;
            NewProduct.Server := '*backup*';
            Backup.NameToProductMap.Add(ProductId, NewProduct);
          end
          else
          begin
            if ExistingProduct.Version < ProductVersion then  //if there are many versions in the same zip file, without a snapshot, get the latest.
            begin
              ExistingProduct.Version := ProductVersion;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure LoadProducts(const FileName: string; const Backup: TBackupData);
const
  DefaultSnapshotFile = 'tms.snapshot.yaml';
begin
  if TBundleDecompressor.GetFileFormat(FileName) = TDownloadFormat.Zip then
  begin
    Backup.ZipFile := FileName;
    var zip := TZipFile.Create;
    try
      zip.Open(FileName, TZipMode.zmRead);
      var idx := zip.IndexOf(DefaultSnapshotFile);
      if idx >= 0 then
      begin
        Backup.HasSnapshot := true;
        LoadSnapshotFromZip(zip, idx, Backup.Products, FileName + ':' + DefaultSnapshotFile);
      end else
      begin
        Backup.HasSnapshot := false;
      end;
      LoadProductsFromZip(zip, Backup);
    finally
      zip.Free;
    end;
  end
  else
  begin
    LoadSnapshot(FileName, Backup.Products);
  end;

end;

function GetDisplayInfo(const NoBuild, SkipRegister: boolean): string;
begin
  Result := '';
  if NoBuild then exit(Result + ' Only restoring. Not building or registering.');
  if SkipRegister then exit(Result + ' Only building. Not registering.');
end;

procedure RestoreComponents(FileName: string; const LatestVersions, NoBuild, IncludeLocalProducts: boolean; const IncludeMasks, ExcludeMasks: TArray<string>; const SkipRegister: boolean);
begin
  Config.ForceSkipRegistering := SkipRegister;
  Logger.Info('Restoring from "' + TPath.GetFileName(FileName) + '".' + GetDisplayInfo(NoBuild, SkipRegister));
  var Backup := TBackupData.Create;
  try
    if (FileName.Trim = '') and (Config.AutoSnapshotFileNames.Count = 1) then FileName := Config.FullAutoSnapshotFileName(0);
    if FileName.Trim = '' then raise Exception.Create('Please specify the filename of the snapshot to restore. You can only call "tms restore" without filename if a single "automatic snapshot filename" is provided.');
    FileName := TPath.GetFullPath(FileName);

    LoadProducts(FileName, Backup);

    var ProductsToFetch: TList<string> := nil;
    var ProductsToGetFromBackup: TList<TBackupSingleFile> := nil;
    var ProductsToPin: TList<string> := nil;
    var ProductsToUnpin: TList<string> := nil;
    var ProductsToBuild: TList<string> := nil;
    try
      ProductsToFetch := TList<string>.Create;
      ProductsToGetFromBackup := TList<TBackupSingleFile>.Create;
      ProductsToPin := TList<string>.Create;
      ProductsToUnpin := TList<string>.Create;
      ProductsToBuild := TList<string>.Create;

      for var Product in Backup.Products do
      begin
        //We could use Config.IsIncluded here but we would get no message that the component was excluded because it was in excluded components in the config file
        //If we add everything, we will still not include the excluded components in the file, but get a less confusing message.
        if Ignore(Product.Id, IncludeMasks, ExcludeMasks) then continue;
        if (Product.Server = '') and (not IncludeLocalProducts) then continue;


        var Version := '';
        if not LatestVersions then Version := ':' + String(Product.Version);

        var BackupIndex: integer;
        if Backup.ProductInBackupList(Product.Id, Product.Version, BackupIndex)
          then ProductsToGetFromBackup.Add(TBackupSingleFile.Create(BackupIndex, Product.Id, Product.Version, Product.Server))
          else ProductsToFetch.Add(Product.Id + Version);

        if not LatestVersions then
        begin
          if Product.Pinned then ProductsToPin.Add(Product.Id)
          else ProductsToUnpin.Add(Product.Id);
        end;

        if not NoBuild then ProductsToBuild.Add(Product.Id);

      end;

      Install(ProductsToFetch.ToArray, Backup.ZipFile, ProductsToGetFromBackup.ToArray, ProductsToPin.ToArray, ProductsToUnpin.ToArray, ProductsToBuild.ToArray);
    finally
      ProductsToFetch.Free;
      ProductsToGetFromBackup.Free;
      ProductsToPin.Free;
      ProductsToUnpin.Free;
      ProductsToBuild.Free;
    end;
  finally
    Backup.Free;
  end;
end;


var OptionFileName: string;
var OptionLatestVersions: boolean;
var OptionNoBuild: boolean;
var OptionLocal: boolean;
var OptionSkipRegister: boolean;
var OptionRegister: boolean;
var OptionInclude: TArray<string>;
var OptionExclude: TArray<string>;

procedure RunRestoreCommand;
begin
  InitFolderBasedCommand(true);
  if OptionSkipRegister and OptionRegister then raise Exception.Create('Please specify only one of -auto-register or -skip-register.');
  if not OptionSkipRegister and not OptionRegister then raise Exception.Create('Please specify -auto-register or -skip-register.');


  RestoreComponents(OptionFileName, OptionLatestVersions, OptionNoBuild, OptionLocal, OptionInclude, OptionExclude, OptionSkipRegister);
end;


procedure RegisterRestoreCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('restore', '', 'restore a list of components from a snapshot file.',
    'More information: https://doc.tmssoftware.com/smartsetup/reference/tms-restore.html',
    'restore [filename] <-auto-register/-skip-register> [-latest] [-nobuild] [-include:...] [-exclude:...]');

  cmd.Examples.Add('restore -auto-register');
  cmd.Examples.Add('restore c:\test\tms.snapshot.yaml -skip-register -latest');
  cmd.Examples.Add('restore -skip-register -exclude:tms.biz.* -exclude:example.test.*');

  var option := cmd.RegisterUnnamedOption<string>('File from where to load the snapshot. If not specified and there is a single "auto snapshot filenames" entry in the config file, it will load it.', 'filename',
    procedure(const Value : string)
    begin
      OptionFileName := Value;
    end);
  option.Required := false;

  option := cmd.RegisterOption<boolean>('auto-register', '', 'register the components in the IDE according to the configuration.',
    procedure(const Value : boolean)
    begin
      OptionRegister := true;
    end);
  option.HasValue := False;

  option := cmd.RegisterOption<boolean>('skip-register', '', 'do NOT register the components in the IDE. only build them.',
    procedure(const Value : boolean)
    begin
      OptionSkipRegister := true;
    end);
  option.HasValue := False;

  option := cmd.RegisterOption<boolean>('latest', '', 'if you specify this option, components will be restored to their latest versions, not what is in the snapshot.',
    procedure(const Value : boolean)
    begin
      OptionLatestVersions := true;
    end);
  option.HasValue := False;

  option := cmd.RegisterOption<boolean>('include-local', '', 'if you specify this parameter, restore will try to also install local projects in the snapshot, and probably fail.',
    procedure(const Value : boolean)
    begin
      OptionLocal := true;
    end);
  option.HasValue := False;

  option := cmd.RegisterOption<string>('include', '', 'specify what components to restore from the snapshot. If not specified, all components will be restored.',
    procedure(const Value : string)
    begin
      OptionInclude := OptionInclude + [Value.Trim];
    end);
  option.AllowMultiple := true;

  option := cmd.RegisterOption<string>('exclude', '', 'specify what components to restore from the snapshot. If not specified, all components will be restored.',
    procedure(const Value : string)
    begin
      OptionExclude := OptionExclude + [Value.Trim];
    end);
  option.AllowMultiple := true;

  
  RegisterNoBuildOption(cmd,
    procedure(const Value: Boolean)
    begin
      OptionNoBuild := Value;
    end);



  AddCommand(cmd.Name, CommandGroups.Install, RunRestoreCommand);
end;

{ TBackup }

constructor TBackupData.Create;
begin
  inherited;
  Products := TObjectList<TProductStatus>.Create;
  NameToIndexMap := TDictionary<string, integer>.Create;
  NameToProductMap := TDictionary<string, TProductStatus>.Create;
end;

destructor TBackupData.Destroy;
begin
  NameToProductMap.Free;
  NameToIndexMap.Free;
  Products.Free;
  inherited;
end;

function TBackupData.ProductInBackupList(const ProductId: string;
  const ProductVersion: TLenientVersion; out BackupIndex: integer): boolean;
begin
  var BackupFileName := TDownloadFileName.GenerateFileName(ProductId, ProductVersion);
  Result := NameToIndexMap.TryGetValue(BackupFileName, BackupIndex);
end;

{ TBackupSingleFile }

constructor TBackupSingleFile.Create(const aIndex: integer; aProductId: string;
  aVersion: TLenientVersion; const aServer: string);
begin
  Index := aIndex;
  ProductId := aProductId;
  Version := aVersion;
  Server := aServer;
end;

end.


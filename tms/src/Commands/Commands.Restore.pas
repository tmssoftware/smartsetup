unit Commands.Restore;
interface
uses
  System.Generics.Collections, System.SysUtils, UCommandLine;

procedure RegisterRestoreCommand;

implementation
uses
  Commands.CommonOptions, Commands.Logging, Commands.GlobalConfig, Status.Manager, Deget.CoreTypes,
  Deget.Version, Snapshot.Reader, Actions.Fetch, Actions.Build, Commands.Pin, Masks;

procedure Install(const ProductsToFetch, ProductsToPin, ProductsToUnpin, ProductsToBuild: TArray<string>);
begin
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

procedure RestoreComponents(FileName: string; const LatestVersions, NoBuild, IncludeLocalProducts: boolean; const IncludeMasks, ExcludeMasks: TArray<string>; const SkipRegister: boolean);
begin
  Config.ForceSkipRegistering := SkipRegister;
  var Products := TObjectList<TProductStatus>.Create;
  try
    if (FileName.Trim = '') and (Config.AutoSnapshotFileNames.Count = 1) then FileName := Config.AutoSnapshotFileNames.KeyList[0]; 
    if FileName.Trim = '' then raise Exception.Create('Please specify the filename of the snapshot to restore. You can only call "tms restore" without filename if a single "automatic snapshot filename" is provided.');

    
    LoadSnapshot(FileName, Products);

    var ProductsToFetch: TList<string> := nil;
    var ProductsToPin: TList<string> := nil;
    var ProductsToUnpin: TList<string> := nil;
    var ProductsToBuild: TList<string> := nil;
    try
      ProductsToFetch := TList<string>.Create;
      ProductsToPin := TList<string>.Create;
      ProductsToUnpin := TList<string>.Create;
      ProductsToBuild := TList<string>.Create;

      for var Product in Products do
      begin        
        //We could use Config.IsIncluded here but we would get no message that the component was excluded because it was in excluded components in the config file
        //If we add everything, we will still not include the excluded components in the file, but get a less confusing message.
        if Ignore(Product.Id, IncludeMasks, ExcludeMasks) then continue;
        if (Product.Server = '') and not IncludeLocalProducts then continue;

        
        var Version := '';
        if not LatestVersions then Version := ':'+ String(Product.Version);

        ProductsToFetch.Add(Product.Id + Version);
        if not LatestVersions then
        begin
          if Product.Pinned then ProductsToPin.Add(Product.Id)
          else ProductsToUnpin.Add(Product.Id);
        end;

        if not NoBuild then ProductsToBuild.Add(Product.Id);

      end;

      Install(ProductsToFetch.ToArray, ProductsToPin.ToArray, ProductsToUnpin.ToArray, ProductsToBuild.ToArray);
    finally
      ProductsToFetch.Free;
      ProductsToPin.Free;
      ProductsToUnpin.Free;
      ProductsToBuild.Free;
    end;
  finally
    Products.Free;
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

end.


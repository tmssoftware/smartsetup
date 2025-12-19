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

procedure RestoreComponents(FileName: string; const RestoreVersions, NoBuild, RestoreServerless: boolean; const IncludeMasks, ExcludeMasks: TArray<string>);
begin
  var Products := TObjectList<TProductStatus>.Create;
  try
    if FileName.Trim = '' then FileName := Config.AutoSnapshotFileName; 
    if FileName.Trim = '' then raise Exception.Create('Please specify the filename of the snapshot to restore. You can only call "tms restore" without filename if an "automatic snapshot filename" is provided.');

    
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
        if (Product.Server = '') and not (RestoreServerless) then continue;
        
        //We could use Config.IsIncluded here but we would get no message that the component was excluded because it was in excluded components in the config file
        //If we add everything, we will still not include the excluded components in the file, but get a less confusing message.
        if Ignore(Product.Id, IncludeMasks, ExcludeMasks) then continue;
        
        
        var Version := '';
        if RestoreVersions then Version := ':'+ String(Product.Version);

        ProductsToFetch.Add(Product.Id + Version);
        if RestoreVersions then
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
var OptionRestoreVersions: boolean;
var OptionNoBuild: boolean;
var OptionRestoreServerless: boolean;
var OptionInclude: TArray<string>;
var OptionExclude: TArray<string>;

procedure RunRestoreCommand;
begin
  InitFolderBasedCommand(true);
  RestoreComponents(OptionFileName, OptionRestoreVersions, OptionNoBuild, OptionRestoreServerless, OptionInclude, OptionExclude);
end;


procedure RegisterRestoreCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('restore', '', 'restore a list of components from a snapshot file.',
    '',
    'restore [filename] [-full] [-nobuild] [-serverless] [-include:...] [-exclude:...]');

  cmd.Examples.Add('restore');
  cmd.Examples.Add('restore c:\test\tms.snapshot.yaml -full');
  cmd.Examples.Add('restore -exclude:tms.biz.* -exclude:example.test.*');

  var option := cmd.RegisterUnnamedOption<string>('File from where to load the snapshot. If not specified, we will load the "auto snapshot filename" in the config file.', 'filename',
    procedure(const Value : string)
    begin
      OptionFileName := Value;
    end);
  option.Required := false;

  option := cmd.RegisterOption<boolean>('full', '', 'without this parameter, the components will be restored to their latest versions. with it, the components will be restored to the exact version saved in the snapshot, along with the pinned status.',
    procedure(const Value : boolean)
    begin
      OptionRestoreVersions := true;
    end);
  option.HasValue := False;

  option := cmd.RegisterOption<boolean>('serverless', '', 'Try to restore also local products that weren''t loaded from any server. This option is for advanced uses, normally you will want it off',
    procedure(const Value : boolean)
    begin
      OptionRestoreServerless := true;
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


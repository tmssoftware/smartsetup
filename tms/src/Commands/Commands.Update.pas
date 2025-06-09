unit Commands.Update;

interface

procedure RegisterUpdateCommand;

implementation

uses
  System.StrUtils, System.Diagnostics, UCommandLine, Commands.CommonOptions, Commands.Logging, Actions.Fetch,
  Actions.Build, Commands.GlobalConfig, Downloads.VersionManager, UMultiLogger;

var
  NoBuild: Boolean = False;
  ProductIds: TArray<string>;

procedure RunUpdateCommand;
begin
  InitFolderBasedCommand;

  var AllStopWatch := TStopWatch.Create;
  AllStopWatch.Start;

  ExecuteFetchAction(ProductIds, TFetchMode.OnlyInstalled);

  RotateDownloads(Config.MaxVersionsPerProduct);

  // Build
  if not NoBuild then
    ExecuteBuildAction(ProductIds, False);

  AllStopWatch.Stop;
  Logger.Info('');
  Logger.Info('Updated finished. Elapsed time: ' + AllStopWatch.Elapsed.ToString);
end;

procedure RegisterUpdateCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('update', '', 'update currently installed products',
    'Checks for new versions of installed products and updates them to the latest versions.' + sLineBreak +
    'You can optionally specify the ids of the products to be updated. '+
    'If no product id is provided, it will update all installed products.' + sLineBreak +
    'If you specify the id of a product that is not installed, an error will be raised.' + sLineBreak +
    '''tms update'' downloads and installs the products. Calling ''tms update'' is the same as calling ''tms fetch'' and then ''tms build''.' + sLineBreak,
    'update [<product-ids>] [options]');

  cmd.Examples.Add('update');
  cmd.Examples.Add('update tms.biz.aurelius,tms.fnc.maps');
  cmd.Examples.Add('update tms.biz.* tms.vcl.*');

  var option := cmd.RegisterUnNamedOption<string>('the ids of the products to be updated, optional', 'product-ids',
    procedure(const Value: string)
    begin
      ProductIds := ProductIds + SplitString(Value, ',');
    end);
  option.AllowMultiple := True;

  RegisterRepoOption(cmd);

  AddCommand(cmd.Name, CommandGroups.Install, RunUpdateCommand);
end;

end.

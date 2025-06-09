unit Commands.Install;

interface

uses
  System.SysUtils, System.StrUtils, System.Diagnostics, VSoft.CommandLine.Options, UCommandLine, UMultiLogger;

procedure RegisterInstallCommand;

implementation

uses
  Commands.CommonOptions, URepositoryManager, Fetching.Manager, Fetching.Summary, Commands.Logging, Commands.GlobalConfig,
  Actions.Build, Actions.Fetch;

var
  ProductIds: TArray<string>;
  NoBuild: Boolean = False;

procedure RunInstallCommand;
begin
  InitFolderBasedCommand;

  var AllStopWatch := TStopWatch.Create;
  AllStopWatch.Start;

  // fetch
  ExecuteFetchAction(ProductIds, TFetchMode.DownloadNew);
  // Build
  if not NoBuild then
    ExecuteBuildAction(ProductIds, False);

  AllStopWatch.Stop;
  Logger.Info('');
  Logger.Info('Install finished. Elapsed time: ' + AllStopWatch.Elapsed.ToString);
end;

procedure RegisterInstallCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('install', '', 'Installs the specified product(s)',
    'Downloads all the specified products to the local computer, builds the packages and registers them in the IDEs.',
    'install <product-ids>');
  cmd.Examples.Add('install tms.biz.*');
  cmd.Examples.Add('install tms.biz.aurelius tms.fnc.maps');
  cmd.Examples.Add('install tms.biz.* tms.vcl.*');

  var option := cmd.RegisterUnNamedOption<string>('The ids of the products to be installed', 'product-ids',
    procedure(const Value: string)
    begin
      ProductIds := ProductIds + SplitString(Value, ',');
    end);
  option.Required := True;
  option.AllowMultiple := True;

  RegisterRepoOption(cmd);

  RegisterNoBuildOption(cmd,
    procedure(const Value: Boolean)
    begin
      NoBuild := Value;
    end);

  AddCommand(cmd.Name, CommandGroups.Install, RunInstallCommand);
end;

end.

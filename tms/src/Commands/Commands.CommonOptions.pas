unit Commands.CommonOptions;

interface

uses
  VSoft.CommandLine.Options, System.SysUtils, UCommandLine, ULogger, UConsoleLogger;

procedure RegisterGlobalOptions;
procedure RegisterRepoOption(cmd: TCommandDefinition);
procedure RegisterNoBuildOption(cmd: TCommandDefinition; Action: TConstProc<Boolean>);
procedure RegisterNoFetchOption(cmd: TCommandDefinition; Action: TConstProc<Boolean>);
procedure RegisterVersionCommand;
procedure CheckAppAlreadyRunning;

var
  VerbositySetByCommandLine :boolean = false;
  ConsoleLogDisplayOptions: TConsoleLogDisplayOptionSet = LogDisplayOptionsDefaut;

type
  CommandGroups = class
    public const
      Install = 'Installing and updating:';
      Config = 'Configuration:';
      Status = 'Informative:';
      Diagnosis = 'Check and Repair:';
      Self = 'Concerning tms tool itself:';
  end;

implementation

uses
  Commands.Logging, Commands.GlobalConfig, UMultiLogger, IOUTils,
  UTmsBuildSystemUtils, Testing.CommandLineOptions;

procedure CheckAppAlreadyRunning;
begin
  //Only test after -config file was parsed. Or we will get the wrong ConfigFileName.
  var FullConfigFileName := TPath.GetFullPath(ConfigFileName);

  if AppIsAlreadyRunning(FullConfigFileName) then
  begin
    WriteLn('There is another instance of tms smart setup working in this same folder.');
    WriteLn('It might be another user in another session.');
    WriteLn('Only one instance of tms smart setup can run in the same folder at the same time.');
    WriteLn('If you can''t locate the other smart setup instance, please reboot the computer and retry.');
    ExitCode := 100;
    raise Exception.Create('TMS Smart Setup is already running in folder "' + TPath.GetDirectoryName(FullConfigFileName) + '"');
  end;

end;

procedure PrintVersion;
const
  {$i ../../../Version.inc}
begin
  WriteLn('tms version ' + TMSVersion)
end;

procedure RunDefaultCommand;
const
  {$i ../../../Version.inc}
begin
  PrintVersion;

  WriteLn('');
  WriteLn('Getting started:');
  WriteLn('');
  WriteLn('  tms credentials             set the credentials to access remote repository');
  WriteLn('  tms list-remote            list products available to be installed');
  WriteLn('  tms install <product-ids>   install the specified products');
  WriteLn('  tms help <command>          display usage for a specific command');

  WriteLn('');
  WriteLn('');
  WriteLn('Usage: tms [command] [options]');
  WriteLn('');
  PrintGroupedCommands;
  WriteLn('Documentation:');
  WriteLn('  https://doc.tmssoftware.com/smartsetup');
  ExitCode := 2;
  Abort;
end;

function GetDisplayOption(const Entry: string): TConsoleLogDisplayOptions;
const
  Options: Array[TConsoleLogDisplayOptions] of string = (
    'no-ansi',
    'product-progress'
  );
begin
  for var option := Low(TConsoleLogDisplayOptions) to High(TConsoleLogDisplayOptions) do
  begin
    if SameText(Entry.Trim, Options[option]) then exit(option);
  end;

  raise Exception.Create('"' + Entry + '" is not a valid option for display. The possible options are: "' + string.Join(',', Options) + '"');
end;

function ParseConsoleLogDisplayOptions(const Value: string): TConsoleLogDisplayOptionSet;
begin
  Result := LogDisplayOptionsDefaut;
  var ValueArray := Value.Split([','], TStringSplitOptions.ExcludeEmpty);

  for var Entry in ValueArray do
  begin
    var opt := GetDisplayOption(Entry);
    if opt in LogDisplayOptionsDefaut then Exclude(Result, opt) else Include(Result, opt);
  end;
end;

procedure RegisterGlobalOptions;
begin
  // config file name
  var option := TOptionsRegistry.RegisterOption<string>(
    'config', '', 'specifies the location of a configuration file other than the default',
    procedure(const Value: string)
    begin
      SetConfigFileName(Value);
    end);

  option := TOptionsRegistry.RegisterOption<string>(
    'add-config', '', 'adds the options from a new config file to the existing ones',
    procedure(const Value: string)
    begin
      AddConfigFileName(Value);
    end);

  // log verbose mode
  option := TOptionsRegistry.RegisterOption<Boolean>(
    'verbose', '', 'shows more detailed information in the output',
    procedure(const Value: Boolean)
    begin
      if Value then
      begin
        Logger.Verbosity := TVerbosity.trace;
        VerbositySetByCommandLine := true;
      end;
    end);
  option.HasValue := False;

  // Console Log display options
  option := TOptionsRegistry.RegisterOption<string>(
    'display-options', '', 'changes what tms displays in the console.',
    procedure(const Value: string)
    begin
      ConsoleLogDisplayOptions := ParseConsoleLogDisplayOptions(Value);
    end);
  option.HasValue := true;


  // see https://github.com/tmssoftware/tms-smartsetup/discussions/139
  // tms build -p:configuration-for-all-products:delphi-versions=[delphi12] -p:configuration-for-all-products:platforms=[win32Intel,win64intel]
  option := TOptionsRegistry.RegisterOption<string>(
    'property', 'p', 'overrides a parameter from tms.config.yaml. For example "-p:configuration-for-all-products:platforms=[win32intel,win64intel]"',
    procedure(const Value: string)
    begin
      AddConfigParameter(Value);
    end);

  {$IFDEF DEBUG}
  RegisterTestingOptions;
  {$ENDIF}

  // default command
  AddCommand('', RunDefaultCommand);
end;

procedure RegisterRepoOption(cmd: TCommandDefinition);
begin
  var option := cmd.RegisterOption<string>(
    'repo', '', 'The target repository to use. Valid values are production, sandbox and local',
    procedure(const Value: string)
    begin
      FetchOptions.TargetRepository := Value;
    end);
  option.Hidden := True;
end;

procedure RegisterNoBuildOption(cmd: TCommandDefinition; Action: TConstProc<Boolean>);
begin
  var option := cmd.RegisterOption<Boolean>('nobuild', '', 'Do not build products', Action);
  option.HasValue := False;
end;

procedure RegisterNoFetchOption(cmd: TCommandDefinition; Action: TConstProc<Boolean>);
begin
  var option := cmd.RegisterOption<Boolean>('nofetch', '',
    'Do not fetch newer versions from the server, only use what is already downloaded', Action);
  option.HasValue := False;
end;

procedure RegisterVersionCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('version', '', 'prints current version of tms utility',
    '',
    '');

  AddCommand(cmd.Name, CommandGroups.Self, procedure
    begin
      PrintVersion;
    end);
end;

end.

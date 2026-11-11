unit Commands.ConfigRead;

interface
uses Classes, SysUtils, Generics.Collections, UConfigSources;

procedure RegisterConfigReadCommand;

implementation
uses UCommandLine, Commands.CommonOptions, Commands.Logging,
     UConfigWriter, Types, Commands.LocalConfig,
     BBCmd, UConfigLoaderStateMachine, BBYaml.Writer, UMultiLogger;

var
  VariableName: string;
  CmdSyntax: boolean;


procedure RunConfigReadCommand;
begin
  InitFolderBasedCommand(false);
  var Cfg := TSourcedConfig.Create(AllConfigSources);
  try
    WriteLn(TConfigWriter.GetProperty(Cfg.Instance, VariableName, TWritingFormat.NoComments, Logger.JsonMode, CmdSyntax));
  finally
    Cfg.Free;
  end;
end;


procedure RegisterConfigReadCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('config-read', '', 'Reads one setting from the smart-setup configuration file.',
    'The output of the command is the value of the variable if it has any set value.' + sLineBreak +
    'More information: https://doc.tmssoftware.com/smartsetup/reference/tms-config-read.html',
    'config-read <variable-name>');

  cmd.Examples.Add('config-read tms-smart-setup-options:git:git-location');
  cmd.Examples.Add('config-read -cmd');

  var option := cmd.RegisterUnNamedOption<string>('the name of the setting to read', 'variable-name',
    procedure(const Value: string)
    begin
      VariableName := Value;
    end);
  option.AllowMultiple := False;
  option.Required := false;

  RegisterJsonOption(cmd);

  option := cmd.RegisterOption<Boolean>('cmd', '', 'show the property names as you need them in the command line',
    procedure(const Value: Boolean)
    begin
      CmdSyntax := Value;
      Logger.CmdMode := Value;
    end);
  option.HasValue := False;

  RegisterConfigSourceOption(cmd);

  AddCommand(cmd.Name, CommandGroups.Config, RunConfigReadCommand);
end;


end.

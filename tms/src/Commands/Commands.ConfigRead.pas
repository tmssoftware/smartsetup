unit Commands.ConfigRead;

interface
uses Classes, SysUtils, Generics.Collections;

procedure RegisterConfigReadCommand;

implementation
uses UCommandLine, Commands.CommonOptions,
     UConfigWriter, Types, Commands.GlobalConfig,
     BBCmd, UConfigLoaderStateMachine, BBYaml.Writer;

var
  VariableName: string;
  UseJson: boolean;
  CmdSyntax: boolean;


procedure RunConfigReadCommand;
begin
  WriteLn(TConfigWriter.GetProperty(Config, VariableName, TWritingFormat.NoComments, UseJson, CmdSyntax));
end;


procedure RegisterConfigReadCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('config-read', '', 'Reads one setting from the smart-setup configuration file.',
    'The output of the command is the value of the variable if it has any set value.',
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

  option := cmd.RegisterOption<Boolean>('json', '', 'output data in JSON format',
    procedure(const Value: Boolean)
    begin
      UseJson := Value;
    end);
  option.HasValue := False;

  option := cmd.RegisterOption<Boolean>('cmd', '', 'show the property names as you need them in the command line',
    procedure(const Value: Boolean)
    begin
      CmdSyntax := Value;
    end);
  option.HasValue := False;

  AddCommand(cmd.Name, CommandGroups.Config, RunConfigReadCommand);
end;


end.

unit Commands.ConfigRead;

interface
uses Classes, SysUtils, Generics.Collections;

procedure RegisterConfigReadCommand;

implementation
uses UCommandLine, Commands.CommonOptions,
     UConfigWriter, Types, Commands.GlobalConfig,
     BBCmd, UConfigLoaderStateMachine;

var
  VariableName: string;


procedure RunConfigReadCommand;
begin
  var FullyChangeThisMethod:='r';
  if String.IsNullOrWhiteSpace(VariableName) then
  begin
    raise Exception.Create('There is no variable specified.');
  end;

  var ConfigWriter := TConfigWriter.Create(true);
  try
    var MainSection := TMainSectionConf.Create(Config);
    try
      var ExtraInfos := TList<string>.Create;
      try

        //var value := ConfigWriter.Create();
        if ExtraInfos.Count > 0 then
        begin
          //value := ConfigWriter.ReplaceVariables(Config, '%config-for-product_' + ExtraInfos[0].Replace(' ', '.') + '%', '%' + PercentVariableName + '%');
        end
        else
        begin
          //value := ConfigWriter.ReplaceVariables(Config, '%' + PercentVariableName + '%', '');
        end;
        //WriteLn(value);
      finally
        ExtraInfos.Free;
      end;
    finally
      MainSection.Free;
    end;
  finally
    ConfigWriter.Free;
  end;
end;


procedure RegisterConfigReadCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('config-read', '', 'Reads one setting from the smart-setup configuration file.',
    'The output of the command is the value of the variable if it has any set value.',
    'config-read <variable-name>');

  cmd.Examples.Add('config-read tms-smart-setup-options:git:git-location');

  var option := cmd.RegisterUnNamedOption<string>('the name of the setting to read', 'variable-name',
    procedure(const Value: string)
    begin
      VariableName := Value;
    end);
  option.AllowMultiple := False;
  option.Required := True;

  AddCommand(cmd.Name, CommandGroups.Config, RunConfigReadCommand);
end;


end.

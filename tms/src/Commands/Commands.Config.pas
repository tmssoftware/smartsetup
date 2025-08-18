unit Commands.Config;

interface

uses
  System.SysUtils, UCommandLine, VSoft.CommandLine.Options, Commands.CommonOptions;

procedure RegisterConfigCommand;

implementation

uses
  UTmsBuildSystemUtils, Commands.GlobalConfig, System.IOUtils, System.Classes,
  System.Types, UConfigWriter, UConfigDefinition, UConfigKeys,
  Deget.CoreTypes;

var
  Print,
  Reset: boolean;

function CreateDefaultConfig: TConfigDefinition;
begin
  Result := TConfigDefinition.Create('');
  Result.Products.Add(GlobalProductId, TProductConfigDefinition.Create(GlobalProductId));
end;

procedure CreateNewConfigFile;
begin
  var ConfigWriter := TConfigWriter.Create(false);
  try
   var DefaultConfig := CreateDefaultConfig;
   try
     ConfigWriter.Save(DefaultConfig, ConfigFileName);
   finally
     DefaultConfig.Free;
   end;
  finally
   ConfigWriter.Free;
  end;
end;

procedure RunConfigCommand;
begin
  if Reset or not TFile.Exists(ConfigFileName) then
  begin
    CreateNewConfigFile;
  end;

  if Print then
    WriteLn(ConfigFileName)
  else
    LaunchFile(ConfigFileName);
end;

procedure RegisterConfigCommand;
begin
  Print := False;
  Reset := False;
  var cmd := TOptionsRegistry.RegisterCommand('config', '', 'Opens the config file in the default editor for yaml files',
    'Opens the config file in the default editor for yaml files, so you can configure it.' +
    ' If the config file doesn''t exist, this command will create a new standard configuration file and then open it.',
    'config [<options>]');

  var optionPrint := cmd.RegisterOption<Boolean>('print', '', 'Writes the config filename to the screen and do not open config file',
    procedure(const Value : Boolean)
    begin
      Print := Value;
    end);
  optionPrint.HasValue := False;

  var optionReset := cmd.RegisterOption<Boolean>('reset', '', 'Generates a new config filename and deletes any existing one',
    procedure(const Value : Boolean)
    begin
      Reset := Value;
    end);
  optionReset.HasValue := False;

  AddCommand(cmd.Name, CommandGroups.Config, RunConfigCommand);
end;

end.

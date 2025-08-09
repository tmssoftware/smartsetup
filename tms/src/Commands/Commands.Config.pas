unit Commands.Config;

interface

uses
  System.SysUtils, UCommandLine, VSoft.CommandLine.Options, Commands.CommonOptions;

procedure RegisterConfigCommand;

implementation

uses
  UTmsBuildSystemUtils, Commands.GlobalConfig, System.IOUtils, System.Classes,
  System.Types, UConfigWriter, UConfigDefinition, UConfigKeys, Deget.CoreTypes, BBArrays, BBClasses, UConfigLoader;

var
  Print,
  Reset,
  Empty: Boolean;
  ArrayPrefix: TArrayOverrideBehavior;

function CreateDefaultConfig: TConfigDefinition;
begin
  Result := TConfigLoader.LoadConfig('', CommandLineConfig);
  Result.EnsureAllProducts;
  for var ArrayProperty := Low(TGlobalPrefixedProperties) to High(TGlobalPrefixedProperties) do
  begin
    Result.PrefixedProperties[ArrayProperty] := ArrayPrefix;
  end;

  for var Product in Result.Products.Values do
  begin
    for var ArrayProperty := Low(TProductPrefixedProperties) to High(TProductPrefixedProperties) do
    begin
      Product.PrefixedProperties[ArrayProperty] := ArrayPrefix;
    end;
  end;

end;

procedure CreateNewConfigFile;
begin
  if Empty then
  begin
    TFile.WriteAllText(ConfigFileName, '', TUTF8NoBOMEncoding.Instance);
    exit;
  end;

  var StandardConfigFileGlobal := TResourceStream.Create(HInstance, 'StandardConfigFileGlobal', RT_RCDATA);
  try
    var StandardConfigFileProduct := TResourceStream.Create(HInstance, 'StandardConfigFileProduct', RT_RCDATA);
    try
       var ConfigWriter := TConfigWriter.Create(false);
       try
         var DefaultConfig := CreateDefaultConfig;
         try
           ConfigWriter.Save(DefaultConfig, StandardConfigFileGlobal, StandardConfigFileProduct, ConfigFileName);
         finally
           DefaultConfig.Free;
         end;
       finally
         ConfigWriter.Free;
       end;
    finally
      StandardConfigFileProduct.Free;
    end;
  finally
    StandardConfigFileGlobal.Free;
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

  var optionArray := cmd.RegisterOption<string>('array-prefix', '', 'When creating a config file, it will add the prefix to array properties.',
    procedure(const Value : string)
    begin
      ArrayPrefix := TArrayOverrideBehavior_FromString(Value);
    end);

  AddCommand(cmd.Name, CommandGroups.Config, RunConfigCommand);
end;

end.

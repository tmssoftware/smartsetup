unit UConfigLoader;
{$i ../../tmssetup.inc}

interface
uses UConfigDefinition, BBYaml, BBCmd, UConfigLoaderStateMachine, IOUtils, Generics.Collections;
type

  TConfigLoader = class
  private
    class procedure LoadIntoConfig(const BaseFilename, Filename: string; const Config: TConfigDefinition; const CmdParameters: TArray<string>; const aStopAt: string; const aIgnoreOtherFiles: boolean);

  public
    class function LoadConfig(const ConfigFile: string; const CmdParameters: TArray<string>; const aStopAt: string = ''; const aIgnoreOtherFiles: boolean = false): TConfigDefinition;
  end;
implementation
uses Classes, SysUtils, UMultiLogger;

{ TProjectLoader }

class procedure TConfigLoader.LoadIntoConfig(const BaseFilename, Filename: string; const Config: TConfigDefinition;
   const CmdParameters: TArray<string>; const aStopAt: string; const aIgnoreOtherFiles: boolean);
var
  MainSection: TMainSectionConf;
begin
  MainSection := TMainSectionConf.Create(Config);
  try
    MainSection.CreatedBy := 'Base: ' + BaseFilename;
    if (TFile.Exists(BaseFilename)) then TBBYamlReader.ProcessFile(BaseFilename, MainSection, aStopAt, aIgnoreOtherFiles);
    MainSection.CreatedBy := 'Main: ' + Filename;
    if (TFile.Exists(Filename)) then TBBYamlReader.ProcessFile(Filename, MainSection, aStopAt, aIgnoreOtherFiles);
    MainSection.CreatedBy := 'Command line';
    TBBCmdReader.ProcessCommandLine(CmdParameters, MainSection, ':');

  finally
    MainSection.Free;
  end;
end;

class function TConfigLoader.LoadConfig(const ConfigFile: string; const CmdParameters: TArray<string>; const aStopAt: string; const aIgnoreOtherFiles: boolean): TConfigDefinition;
begin
  var BaseConfigFile := GetEnvironmentVariable('TMS_STARTING_CONFIGURATION');
  if BaseConfigFile <> '' then
  begin
    if TFile.Exists(BaseConfigFile) then
    begin
      Logger.Trace('Loading Base Configuration from ' + BaseConfigFile);
    end else
    begin
      raise Exception.Create('Can''t find the base configuration file "' + BaseConfigFile +'" defined in the Environment Variable TMS_STARTING_CONFIGURATION');
    end;
  end;
  if TFile.Exists(ConfigFile) then
  begin
    Logger.Trace('Loading Configuration from ' + ConfigFile);
  end;
  Result := TConfigDefinition.Create(TPath.GetDirectoryName(TPath.GetFullPath(ConfigFile)));
  try
    LoadIntoConfig(BaseConfigFile, ConfigFile, Result, CmdParameters, aStopAt, aIgnoreOtherFiles);
  Except
    Result.Free;
    raise;
  end;
  Logger.Trace('Loaded Configuration.');

end;
end.

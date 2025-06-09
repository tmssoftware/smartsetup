unit UConfigLoader;
{$i ../../tmssetup.inc}

interface
uses UConfigDefinition, BBYaml, BBCmd, UConfigLoaderStateMachine, IOUtils, Generics.Collections;
type

  TConfigLoader = class
  private
    class procedure LoadIntoConfig(const Filename: string; const Config: TConfigDefinition; const CmdParameters: TArray<string>; const aStopAt: string; const aIgnoreOtherFiles: boolean);

  public
    class function LoadConfig(const ConfigFile: string; const CmdParameters: TArray<string>; const aStopAt: string = ''; const aIgnoreOtherFiles: boolean = false): TConfigDefinition;
  end;
implementation
uses Classes, SysUtils, UMultiLogger;

{ TProjectLoader }

class procedure TConfigLoader.LoadIntoConfig(const Filename: string; const Config: TConfigDefinition;
   const CmdParameters: TArray<string>; const aStopAt: string; const aIgnoreOtherFiles: boolean);
var
  MainSection: TMainSectionConf;
begin
  MainSection := TMainSectionConf.Create(Config);
  try
    if (TFile.Exists(Filename)) then TBBYamlReader.ProcessFile(Filename, MainSection, aStopAt, aIgnoreOtherFiles);
    TBBCmdReader.ProcessCommandLine(CmdParameters, MainSection, ':');

  finally
    MainSection.Free;
  end;
end;

class function TConfigLoader.LoadConfig(const ConfigFile: string; const CmdParameters: TArray<string>; const aStopAt: string; const aIgnoreOtherFiles: boolean): TConfigDefinition;
begin
  if TFile.Exists(ConfigFile) then
  begin
    Logger.Trace('Loading Configuration from ' + ConfigFile);
  end;
  Result := TConfigDefinition.Create(TPath.GetDirectoryName(TPath.GetFullPath(ConfigFile)));
  try
    LoadIntoConfig(ConfigFile, Result, CmdParameters, aStopAt, aIgnoreOtherFiles);
  Except
    Result.Free;
    raise;
  end;
  Logger.Trace('Loaded Configuration.');

end;
end.

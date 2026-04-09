unit UConfigLoader;
{$i ../../tmssetup.inc}

interface
uses UConfigDefinition, BBYaml, BBCmd, UConfigLoaderStateMachine, IOUtils, Generics.Collections;
type

  TConfigLoader = class
  private
    class procedure LoadIntoConfig(const Filename: string; const Config: TConfigDefinition; const ExtraConfigFiles: TArray<string>; const CmdParameters: TArray<string>; const aStopAt: string; const aIgnoreOtherFiles: boolean);
    class procedure SetMaxCores(const Config: TConfigDefinition); static;

  public
    class function LoadConfig(const ConfigFile: string; const ExtraConfigFiles: TArray<string>; const CmdParameters: TArray<string>; const aStopAt: string = ''; const aIgnoreOtherFiles: boolean = false): TConfigDefinition;
  end;
implementation
uses Classes, SysUtils, UMultiLogger, System.Threading;

{ TProjectLoader }

class procedure TConfigLoader.SetMaxCores(const Config: TConfigDefinition);
begin
  if (Config.BuildCores < 0) then raise Exception.Create('The number of cores must equal or greater than 0.');
  if (Config.BuildCores > 0) then
  begin
    TThreadPool.Default.SetMaxWorkerThreads(Config.BuildCores);
    //If MaxWorkingThreads < MinWorkerThreads then Max will be ignored.
    TThreadPool.Default.SetMinWorkerThreads(Config.BuildCores);
  end;
end;

class procedure TConfigLoader.LoadIntoConfig(const Filename: string; const Config: TConfigDefinition;
   const ExtraConfigFiles: TArray<string>;
   const CmdParameters: TArray<string>; const aStopAt: string; const aIgnoreOtherFiles: boolean);
var
  MainSection: TMainSectionConf;
begin
  var FullFilename := TPath.GetFullPath(Filename);
  MainSection := TMainSectionConf.Create(Config);
  try
    if (TFile.Exists(FullFilename)) then
    begin
      MainSection.CreatedBy := 'Main: ' + Filename;
      TBBYamlReader.ProcessFile(FullFilename, MainSection, aStopAt, aIgnoreOtherFiles);
    end;

    var LocalFilename := TPath.Combine(TPath.GetDirectoryName(FullFilename), TPath.GetFileNameWithoutExtension(FullFilename) + '.local.yaml');
    if (TFile.Exists(LocalFilename)) then
    begin
      MainSection.CreatedBy := 'Local: ' + LocalFilename;
      Logger.Trace('Loading local configuration from ' + LocalFilename);
      TBBYamlReader.ProcessFile(LocalFilename, MainSection, aStopAt, aIgnoreOtherFiles);
    end;


    for var ExtraConfig in ExtraConfigFiles do
    begin
      MainSection.CreatedBy := 'Extra: ' + ExtraConfig;
      if (not TFile.Exists(ExtraConfig)) then raise Exception.Create('Cannot find extra configuration file: ' + ExtraConfig);
      Logger.Trace('Loading extra configuration from ' + ExtraConfig);
      TBBYamlReader.ProcessFile(ExtraConfig, MainSection, aStopAt, aIgnoreOtherFiles);

    end;

    MainSection.CreatedBy := 'Command line';
    TBBCmdReader.ProcessCommandLine(CmdParameters, MainSection, ':', false);

  finally
    MainSection.Free;
  end;

  SetMaxCores(Config);
end;

class function TConfigLoader.LoadConfig(const ConfigFile: string; const ExtraConfigFiles: TArray<string>; const CmdParameters: TArray<string>; const aStopAt: string; const aIgnoreOtherFiles: boolean): TConfigDefinition;
begin
  if TFile.Exists(ConfigFile) then
  begin
    Logger.Trace('Loading Configuration from ' + ConfigFile);
  end;

  var ConfigFileFullName := ConfigFile;
  if ConfigFile <> '' then ConfigFileFullName := TPath.GetDirectoryName(TPath.GetFullPath(ConfigFile));

  Result := TConfigDefinition.Create(ConfigFileFullName);
  try
    LoadIntoConfig(ConfigFile, Result, ExtraConfigFiles, CmdParameters, aStopAt, aIgnoreOtherFiles);
  Except
    Result.Free;
    raise;
  end;
  Logger.Trace('Loaded Configuration.');

end;
end.

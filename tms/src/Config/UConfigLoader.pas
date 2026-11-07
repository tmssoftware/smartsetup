unit UConfigLoader;
{$i ../../tmssetup.inc}

interface
uses UConfigDefinition, BBYaml, BBCmd, UConfigLoaderStateMachine, IOUtils, Generics.Collections, UConfigSources;
type

  TConfigLoader = class
  private
    class procedure LoadIntoConfig(const Filename: string; const Config: TConfigDefinition; const ConfigSources: TConfigSourceSet; const ExtraConfigFiles: TArray<string>; const CmdParameters: TArray<string>; const aStopAt: string; const aIgnoreOtherFiles: boolean);
    class procedure SetMaxCores(const Config: TConfigDefinition); static;

  public
    class function LoadConfig(const ConfigFile: string; const ConfigSources: TConfigSourceSet; const ExtraConfigFiles: TArray<string>; const CmdParameters: TArray<string>; const aStopAt: string = ''; const aIgnoreOtherFiles: boolean = false): TConfigDefinition;
    class function GetGlobalConfigFileName: string; static;
    class function GetLocalConfigFileName(const FullFileName: string): string;
  end;
implementation
uses Classes, SysUtils, UMultiLogger, System.Threading, SmartSetup.Locks;

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

class function TConfigLoader.GetGlobalConfigFileName: string;
begin
  {$IFDEF MSWINDOWS}
  var UserFolder := GetEnvironmentVariable('USERPROFILE');
  {$ELSE}
  var UserFolder := GetEnvironmentVariable('HOME');
  {$ENDIF}
  if UserFolder = '' then exit('');
  Result := TPath.Combine(TPath.Combine(UserFolder, '.smartsetup'), 'tms.config.global.yaml');
end;

class function TConfigLoader.GetLocalConfigFileName(const FullFileName: string): string;
begin
  Result := TPath.Combine(TPath.GetDirectoryName(FullFileName), TPath.GetFileNameWithoutExtension(FullFileName) + '.local.yaml')
end;

class procedure TConfigLoader.LoadIntoConfig(const Filename: string; const Config: TConfigDefinition;
   const ConfigSources: TConfigSourceSet;
   const ExtraConfigFiles: TArray<string>;
   const CmdParameters: TArray<string>; const aStopAt: string; const aIgnoreOtherFiles: boolean);

begin
  var FullFilename := TPath.GetFullPath(Filename);
  var Protector := LockConfig(FullFileName);
  try
    var MainSection := TMainSectionConf.Create(Config);
    try
      var GlobalFilename := GetGlobalConfigFileName;
      if (GlobalFilename <> '') and (TFile.Exists(GlobalFilename)) and (TConfigSource.Global in ConfigSources) then
      begin
        MainSection.CreatedBy := 'Global: ' + GlobalFilename;
        Logger.Trace('Loading global configuration from ' + GlobalFilename);
        Config.SetLoadedFile(GlobalFilename, TConfigSource.Global);
        TBBYamlReader.ProcessFile(GlobalFilename, MainSection, aStopAt, aIgnoreOtherFiles);
      end;

      if (TFile.Exists(FullFilename) and (TConfigSource.Main in ConfigSources)) then
      begin
        MainSection.CreatedBy := 'Main: ' + Filename;
        Config.SetLoadedFile(FullFilename, TConfigSource.Main);
        TBBYamlReader.ProcessFile(FullFilename, MainSection, aStopAt, aIgnoreOtherFiles);
      end;

      var LocalFilename := GetLocalConfigFileName(FullFileName);
      if (TFile.Exists(LocalFilename) and (TConfigSource.Local in ConfigSources)) then
      begin
        MainSection.CreatedBy := 'Local: ' + LocalFilename;
        Logger.Trace('Loading local configuration from ' + LocalFilename);
        Config.SetLoadedFile(LocalFilename, TConfigSource.Local);
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
  finally
    Protector.Free;
  end;

  SetMaxCores(Config);
end;

class function TConfigLoader.LoadConfig(const ConfigFile: string; const ConfigSources: TConfigSourceSet; const ExtraConfigFiles: TArray<string>; const CmdParameters: TArray<string>; const aStopAt: string; const aIgnoreOtherFiles: boolean): TConfigDefinition;
begin
  if TFile.Exists(ConfigFile) then
  begin
    Logger.Trace('Loading Configuration from ' + ConfigFile);
  end;

  var ConfigFileFullName := ConfigFile;
  if ConfigFile <> '' then ConfigFileFullName := TPath.GetDirectoryName(TPath.GetFullPath(ConfigFile));

  Result := TConfigDefinition.Create(ConfigFileFullName);
  try
    LoadIntoConfig(ConfigFile, Result, ConfigSources, ExtraConfigFiles, CmdParameters, aStopAt, aIgnoreOtherFiles);
  Except
    Result.Free;
    raise;
  end;
  Logger.Trace('Loaded Configuration.');

end;
end.

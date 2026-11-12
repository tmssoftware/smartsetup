unit Commands.GlobalConfig;

interface

uses
  System.IOUtils, System.SysUtils, UConfigDefinition, UConfigLoader, UConfigFolders, VCS.Registry, UConfigSources, BBYaml.Writer;

function Config: TConfigDefinition;
function ConfigNoCheck: TConfigDefinition;
function NewConfig(const aConfigSources: TConfigSourceSet): TConfigDefinition;
function ConfigHasBeenLoaded: boolean;
procedure SetConfigFileName(const FileName: string);
procedure AddConfigFileName(const FileName: string);
procedure AddConfigParameter(const Parameter: string);
function ConfigFileName: string;
function GetConfigSingleSource(const aConfigSources: TConfigSourceSet): TConfigSource;
function GetConfigFileName(const aConfigSources: TConfigSourceSet) : string;
function GetWritingFormat(const aConfigSources: TConfigSourceSet) : TWritingFormat;

function IsValidTMSSetupFolder: Boolean;
procedure CheckValidTMSSetupFolder;
function RegisteredVCSRepos(const Server: string=''): TProductRegistry;
function CommandLineConfig: TArray<string>;
function AddedConfigFileNames: TArray<string>;
var
  NeedsToRestartIDE: boolean; // Not sure if here is the best place to put it.

implementation

var
  _Config: TConfigDefinition;
  _CustomFileName: string;
  _CommandLineConfig: TArray<string>;
  _AddedConfigFileNames: TArray<string>;
  _RegisteredVCSRepos: TProductRegistry;

function CommandLineConfig: TArray<string>;
begin
  Result := _CommandLineConfig;
end;

function AddedConfigFileNames: TArray<string>;
begin
  Result := _AddedConfigFileNames;
end;

function GetConfigSingleSource(const aConfigSources: TConfigSourceSet): TConfigSource;
begin
  var CfgSourceCount := 0;
  Result := TConfigSource.Main;
  for var source in aConfigSources do
  begin
    inc(CfgSourceCount);
    Result := source;
  end;
  if CfgSourceCount <> 1 then raise Exception.Create('Cannot save the file. Only one ConfigSource is allowed when saving.');
end;

function GetConfigFileName(const aConfigSources: TConfigSourceSet) : string;
begin
  case GetConfigSingleSource(aConfigSources) of
    TConfigSource.Main: exit(ConfigFileName);
    TConfigSource.Global:
    begin
      Result := TConfigLoader.GetGlobalConfigFileName;
      if Result = '' then raise Exception.Create('Cannot find the user profile folder to save tms.config.global.yaml. Make sure the USERPROFILE (Windows) or HOME (macOS and Linux) environment variable is set.');
      exit;
    end;
    TConfigSource.Local: exit(TConfigLoader.GetLocalConfigFileName(ConfigFileName));
  end;
  raise Exception.Create('Internal error: CfgSource is not a supported value.');
end;

function GetWritingFormat(const aConfigSources: TConfigSourceSet) : TWritingFormat;
begin
  case GetConfigSingleSource(aConfigSources) of
    TConfigSource.Main: exit(TWritingFormat.Full);
    else exit(TWritingFormat.Minimal);
  end;
end;

function ConfigFileName: string;
begin
  Result := _CustomFileName;
  if Result = '' then
    Result := TPath.Combine(System.SysUtils.GetCurrentDir, 'tms.config.yaml');
end;

procedure SetConfigFileName(const FileName: string);
begin
  if _Config <> nil then
    raise Exception.Create('Can''t set new configuration file, configuration already loaded');

  if not TFile.Exists(FileName) then
    raise Exception.CreateFmt('Can''t find the configuration file: "%s"', [TPath.GetFullPath(FileName)]);

  _CustomFileName:= FileName;
end;

procedure AddConfigFileName(const FileName: string);
begin
  _AddedConfigFileNames := _AddedConfigFileNames + [FileName];
end;

procedure AddConfigParameter(const Parameter: string);
begin
  SetLength(_CommandLineConfig, Length(_CommandLineConfig) + 1); //not worth using a TList here. FastMM is good enough resizing arrays
  _CommandLineConfig[Length(_CommandLineConfig) - 1] := Parameter;

end;

function IsValidTMSSetupFolder: Boolean;
begin
  if TFile.Exists(ConfigFileName) then
    Exit(True);

  //If the config file doesn't exist, then there is no working folder and we can search in the folder where configfilename is.
  var Folders: IBuildFolders := TBuildFolders.Create(TPath.GetDirectoryName(ConfigFileName));
  if TDirectory.Exists(Folders.MetaFolder) then
    Exit(True);

{$IFDEF MSWINDOWS}
  const tmsexe = 'tms.exe';
{$ELSE}
  const tmsexe = 'tms';
{$ENDIF}
  if TFile.Exists(TPath.Combine(TPath.GetDirectoryName(ConfigFileName), tmsexe)) then
    Exit(true);


  if TDirectory.IsEmpty(TPath.GetDirectoryName(ConfigFileName)) then
    Exit(True);

  Result := False;

end;

procedure CheckValidTMSSetupFolder;
begin
  if not IsValidTMSSetupFolder then
    raise Exception.CreateFmt('Folder %s doesn''t look like a valid folder for TMS Smart Setup. To use this folder anyway, run "tms config" on it.',
      [TPath.GetDirectoryName(ConfigFileName)]);
end;

function Config: TConfigDefinition;
begin
  if _Config = nil then
  begin
    CheckValidTMSSetupFolder;
    _Config := TConfigLoader.LoadConfig(ConfigFileName, AllConfigSources, AddedConfigFileNames, _CommandLineConfig);

  end;
  Result :=_Config;
end;

function NewConfig(const aConfigSources: TConfigSourceSet): TConfigDefinition;
begin
  CheckValidTMSSetupFolder;
  Result := TConfigLoader.LoadConfig(ConfigFileName, aConfigSources, AddedConfigFileNames, _CommandLineConfig);
end;


function ConfigHasBeenLoaded: boolean;
begin
  Result := _Config <> nil;
end;

function ConfigNoCheck: TConfigDefinition;
begin
  if _Config = nil then
  begin
    _Config := TConfigLoader.LoadConfig(ConfigFileName, AllConfigSources, AddedConfigFileNames, _CommandLineConfig);

  end;
  Result :=_Config;
end;

function RegisteredVCSRepos(const Server: string): TProductRegistry;
begin
  if _RegisteredVCSRepos = nil then
  begin
  _RegisteredVCSRepos := TProductRegistry.Create(Server);
  end;
  Result := _RegisteredVCSRepos;

end;

initialization
  _Config := nil;
  _RegisteredVCSRepos := nil;
  _CustomFileName := '';

finalization
  _Config.Free;
  _RegisteredVCSRepos.Free;
end.

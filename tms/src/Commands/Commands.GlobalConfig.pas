unit Commands.GlobalConfig;

interface

uses
  System.IOUtils, System.SysUtils, UConfigDefinition, UConfigLoader, UConfigFolders, VCS.Registry;

function Config: TConfigDefinition;
procedure SetConfigFileName(const FileName: string);
procedure AddConfigParameter(const Parameter: string);
function ConfigFileName: string;
function IsValidTMSSetupFolder: Boolean;
procedure CheckValidTMSSetupFolder;
function RegisteredVCSRepos: TProductRegistry;

var
  NeedsToRestartIDE: boolean; // Not sure if here is the best place to put it.

implementation

var
  _Config: TConfigDefinition;
  _CustomFileName: string;
  _CommandLineParameters: TArray<string>;
  _RegisteredVCSRepos: TProductRegistry;

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

procedure AddConfigParameter(const Parameter: string);
begin
  SetLength(_CommandLineParameters, Length(_CommandLineParameters) + 1); //not worth using a TList here. FastMM is good enough resizing arrays
  _CommandLineParameters[Length(_CommandLineParameters) - 1] := Parameter;

end;

function IsValidTMSSetupFolder: Boolean;
begin
  if TFile.Exists(ConfigFileName) then
    Exit(True);

  var Folders: IBuildFolders := TBuildFolders.Create(TPath.GetDirectoryName(ConfigFileName));
  Result := TDirectory.Exists(Folders.MetaFolder);
end;

procedure CheckValidTMSSetupFolder;
begin
  if not IsValidTMSSetupFolder then
    raise Exception.CreateFmt('Folder %s is not prepared for TMS Smart Setup, run "tms credentials" or "tms config" first.',
      [TPath.GetDirectoryName(ConfigFileName)]);
end;

function Config: TConfigDefinition;
begin
  if _Config = nil then
  begin
    CheckValidTMSSetupFolder;
    _Config := TConfigLoader.LoadConfig(ConfigFileName, _CommandLineParameters);

  end;
  Result :=_Config;
end;

function RegisteredVCSRepos: TProductRegistry;
begin
  if _RegisteredVCSRepos = nil then
  begin
  _RegisteredVCSRepos := TProductRegistry.Create(Config.Folders.VCSMetaFolder);
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

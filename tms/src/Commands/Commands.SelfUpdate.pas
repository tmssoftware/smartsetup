unit Commands.SelfUpdate;
interface

uses
  System.SysUtils, System.StrUtils, VSoft.CommandLine.Options, UCommandLine, UMultiLogger, Deget.Version,
  ZipFile.Download;

procedure RegisterSelfUpdateCommand;
function NewSmartSetupAvailable: string; //returns empty is there are none.

var
  SmartSetupUpdated: Boolean;

implementation
uses
  Commands.CommonOptions, URepositoryManager, Commands.Logging, Commands.Update, IOUtils, UTmsBuildSystemUtils, Deget.CoreTypes,
  {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} //to keep compiler happy
  Commands.GlobalConfig, System.Zip, Downloads.VersionManager,
  UConfigDefinition, Fetching.Manager, ULogger, Deget.CommandLine, Character,
  UGenericDecompressor, Commands.SelfUpdate.Verify, Testing.Globals, Downloads.FileNameManager;


const
  {$i ../../../Version.inc}

function GetNewVersion: string;
begin
  Result := '';
  if not TDirectory.Exists(Config.Folders.DownloadsFolder) then Exit;
  var Updated := TDirectory.GetFiles(Config.Folders.DownloadsFolder, TDownloadFileName.GenerateRootFileName(TRepositoryManager.TMSSetupProductId) + '*.zip');
  if Length(Updated) = 0 then exit;

  var Current: TVersion := TMSVersion;
{$IFDEF DEBUG}
  if TestParameters.ForceSelfUpdate then Current := TVersion('0.0');
{$ENDIF}

  var MaxVersion := Current;
  for var FileName in Updated do
  begin
    var NextVersion: TVersion := TDownloadFileName.ExtractVersion(FileName);
    if NextVersion > MaxVersion then
    begin
      MaxVersion := NextVersion;
      Result := FileName;
    end;
  end;
end;

function NewSmartSetupAvailable: string;
begin
  try
    if SmartSetupUpdated then exit(''); //We just came from a successful autoupdate, no need to tell the user that the current version is not yet updated.

    var NewVersion := GetNewVersion;
    if NewVersion = '' then exit('');
    Result := TDownloadFileName.ExtractVersion(NewVersion);
  except on ex: Exception do
    //no log here, as it might not be initialized yet.
  end;
end;

procedure MoveUpdatedFiles(const Source, Dest: string);
begin
  //We won't bother adding with files not at the top.
  var Files := TDirectory.GetFiles(Source, '*.*', TSearchOption.soTopDirectoryOnly);
  for var f in Files do
  begin
    var DestFile := TPath.Combine(Dest, TPath.GetFileName(f));
    DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, DestFile);
    RenameAndCheck(f, DestFile);
  end;
end;

procedure AutoUpdate;
begin
  var NewVersion := GetNewVersion;
  if (NewVersion = '') then
  begin
    //Must go after we autoupdated, but before the logs.
    //Because if the user choose to keep 0 files, this would remove the downloaded file before it was extracted.
    RotateDownloads(Config.MaxVersionsPerProduct);

    Logger.Info('');
    Logger.Info('You are using the latest version of TMS Smart Setup.');
    Logger.Info('Your current version is ' + TMSVersion);
    exit;
  end;

  //It should be the place where tms.exe is, not the working folder.
  //We can't move this to the locked folder because tms.exe could be in a different hard disk than the meta folder. See discussion in issue #2
  var RootFolder := TPath.GetDirectoryName(ParamStr(0));
  var TmpFolder := TPath.Combine(RootFolder, '$$$');
  try
    TBundleDecompressor.ExtractCompressedFile(NewVersion, TmpFolder);

    //Verify the new tms.exe is signed by the same publisher as the running
    //binary BEFORE overwriting it. The bundle's `file_hash` comes from the
    //same server as the bundle, so the hash is not a trust anchor — only an
    //offline-signed binary is. See Commands.SelfUpdate.Verify for details.
    VerifySelfUpdateBundle(ParamStr(0), TmpFolder);
    MoveUpdatedFiles(TmpFolder, RootFolder);
  finally
    DeleteFolderMovingToLocked(Config.Folders.LockedFilesFolder, TmpFolder, true, false);
  end;


  //Must go after we autoupdated, but before the logs.
  //Because if the user choose to keep 0 files, this would remove the downloaded file before it was extracted.
  RotateDownloads(Config.MaxVersionsPerProduct);
  Logger.Info('');

  Logger.Info(Format('TMS Smart Setup has been updated from version %s to version %s', [TMSVersion, TDownloadFileName.ExtractVersion(NewVersion)]));
  SmartSetupUpdated := true;
end;

function GetVersionFromBundle(const ZipFileName: string): TVersion;
begin
{$IFDEF MSWINDOWS}
  const TmsExe = 'tms.exe';
{$ELSE}
  const TmsExe = 'tms';
{$ENDIF}
  var ExtractFolder := Config.Folders.TempSelfUpdateFolder;
  var tms := TPath.Combine(ExtractFolder, TmsExe);
  try
    var Zip := TZipFile.Create;
    try
      Zip.Open(ZipFileName, TZipMode.zmRead);
      Zip.Extract(TmsExe, ExtractFolder);
    finally
      Zip.Free;
    end;

    // Verify the signature and publisher before executing downloaded code.
    VerifySelfUpdateFile(ParamStr(0), tms);

    const id = 'tms version ';
    var VersionString: string;
    ExecuteCommand('"' + tms + '" version', '', VersionString);
    var Idx := VersionString.IndexOf(id);
    if (Idx < 0) then raise Exception.Create('Can''t find the version of the downloaded file.');
    var V := VersionString.Substring(Idx + id.Length);
    for var i := 0 to V.Length do
    begin
      if V.Chars[i].IsWhiteSpace then
      begin
        V := V.Substring(0, i);
        break;
      end;
    end;


    if not TVersion.TryFromString(V, Result) then raise Exception.Create('Invalid version number: "' + V + '"');

  finally
    System.SysUtils.DeleteFile(tms);
  end;
end;

procedure FetchSmartSetupFromGithub;
const
  {$IFDEF MSWINDOWS}
    SmartSetupId = 'tmssmartsetup';
  {$ENDIF}
  {$IFDEF LINUX}
    SmartSetupId = 'tmssmartsetup.linux';
  {$ENDIF}
  {$IFDEF MACOS}
    SmartSetupId = 'tmssmartsetup.macos';
  {$ENDIF}

begin

  var DownloadFileName := CombinePath(Config.Folders.MetaSelfUpdateFolder, TRepositoryManager.TMSSetupProductId + '.zip');

  //At the time of writing this code, the url below doesn't incur in rate-limits.
  //To check if it is using, them, the request should return a x-ratelimit-limit header or related.
  //This url doesn't at this time, and github states there are no bandwith restrictions except for abuse: https://docs.github.com/en/repositories/working-with-files/managing-large-files/about-large-files-on-github#distributing-large-binaries
  ZipDownloader.GetRepo(
    'https://github.com/tmssoftware/smartsetup/releases/latest/download/' + SmartSetupId + '.zip',
    DownloadFileName,
    'tms', Logger.Write, false);

  var Version := GetVersionFromBundle(DownloadFileName);
  var FinalFileName := TDownloadFileName.GenerateFileName(TRepositoryManager.TMSSetupProductId, Version) + '.zip';
  TDirectory_CreateDirectory(Config.Folders.DownloadsFolder);
  TFile.Copy(DownloadFileName, CombinePath(Config.Folders.DownloadsFolder, FinalFileName), true);
end;

procedure FetchSmartSetupFromApiServer;
begin
  var ApiServer :=  TServerConfig.CreateInternalServer('tms'); //hardcoded. doesn't matter if tms is disabled.
  var Repo := CreateRepositoryManager(Config.Folders.CredentialsFile(ApiServer.Name), FetchOptions, ApiServer.Url, ApiServer.Name, ApiServer.AllowInsecureConnections, true);
  try
    var Manager := TFetchManager.Create(Config.Folders, Repo, nil);
      try
        Logger.StartSection(TMessageType.Update, 'Self-Updating SmartSetup');
        try
          Manager.UpdateItems;
        finally
          Logger.FinishSection(TMessageType.Update, false);
        end;
      finally
        Manager.Free;
      end;
  finally
    Repo.Free;
  end;
end;

procedure FetchSmartSetup;
begin
  var GotUpdate := false;
  try
    FetchSmartSetupFromApiServer;
    GotUpdate := true;
  except on ex: Exception do
    Logger.Trace('Can''t get update from API server: ' + ex.Message);
  end;

  if not GotUpdate then FetchSmartSetupFromGithub;

  RotateDownloads(Config.MaxVersionsPerProduct);


end;

var
  NoFetch: Boolean = False;

procedure RunSelfUpdateCommand;
begin
  InitFolderBasedCommand;
  if not NoFetch then
    FetchSmartSetup;

  AutoUpdate;

end;

procedure RegisterSelfUpdateCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('self-update', '', 'Updates tms smart setup to the latest version',
    'Checks if there is a new tms smart setup version available, and if there is, downloads it and installs it' + sLineBreak +
    'More information: https://doc.tmssoftware.com/smartsetup/reference/tms-self-update.html',
    'self-update');

  RegisterNoFetchOption(cmd,
    procedure(const Value: Boolean)
    begin
      NoFetch := Value;
    end);

  RegisterRepoOption(cmd);

  AddCommand(cmd.Name, CommandGroups.Self, RunSelfUpdateCommand);
end;

end.

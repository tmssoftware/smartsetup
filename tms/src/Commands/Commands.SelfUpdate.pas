unit Commands.SelfUpdate;
interface

uses
  System.SysUtils, Classes, System.StrUtils, VSoft.CommandLine.Options, UCommandLine, UMultiLogger, Deget.Version,
  ZipFile.Download;

procedure RegisterSelfUpdateCommand;
function NewSmartSetupAvailable: string; //returns empty is there are none.
procedure CheckSelfUpdating(const CommandName: string);

var
  SmartSetupUpdated: Boolean;

implementation
uses
  Commands.CommonOptions, URepositoryManager, Commands.Logging, Commands.Update, IOUtils, UTmsBuildSystemUtils, Deget.CoreTypes,
  {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} //to keep compiler happy
  Commands.GlobalConfig, System.Zip, Downloads.VersionManager, JSON,
  UConfigDefinition, Fetching.Manager, ULogger, Deget.CommandLine, Character, Hash,
  UGenericDecompressor, Commands.SelfUpdate.Verify, Testing.Globals, Downloads.FileNameManager;


const
  {$i ../../../Version.inc}

  SmartSetupHashFileName = 'tms.smartsetup.hashes.txt';

function InstallingLock: string;
begin
  Result := TPath.Combine(Config.Folders.ExeFolder, 'INSTALLING.LOCK');
end;

function CurrentVersion: TVersion;
begin
  Result := TMSVersion;
end;

function SmartSetupTmpHash(const TmpFolder: string): string;
begin
  Result := TPath.Combine(TmpFolder, SmartSetupHashFileName);
end;

function SmartSetupExeHash: string;
begin
  Result := TPath.Combine(Config.Folders.ExeFolder, SmartSetupHashFileName);
end;

function GetNewVersion: string;
begin
  Result := '';
  if not TDirectory.Exists(Config.Folders.DownloadsFolder) then Exit;
  var Updated := TDirectory.GetFiles(Config.Folders.DownloadsFolder, TDownloadFileName.GenerateRootFileName(TRepositoryManager.TMSSetupProductId) + '*.zip');
  if Length(Updated) = 0 then exit;

  var Current := CurrentVersion;
{$IFDEF DEBUG}
  if TestParameters.ForceSelfUpdate then Current := TVersion('0.0');
{$ENDIF}
  if TFile.Exists(InstallingLock) then Current := TVersion('0.0'); //if install state is not right, always update


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
    if TVersion(Result) <= CurrentVersion then exit('');

  except on ex: Exception do
    //no log here, as it might not be initialized yet.
  end;
end;

procedure EnsureHashFile(const TmpFolder: string);
begin
  //To update from smartsetup 3.5 or older, we can't have .txt files in the bundle, so we need to generate it.
  var Hashes := SmartSetupTmpHash(TmpFolder);
  if TFile.Exists(Hashes) then exit;
  var Files := TDirectory.GetFiles(TmpFolder, '*', TSearchOption.soTopDirectoryOnly);
  var Encoding := TUTF8Encoding.Create(False);
  try
    var Sw := TStreamWriter.Create(Hashes, false, Encoding);
    try
      for var F in Files do
      begin
        Sw.WriteLine(TPath.GetFileName(F) + ': ' + THashSHA2.GetHashStringFromFile(F, THashSHA2.TSHA2Version.SHA256));
      end;
    finally
      Sw.Free;
    end;
  finally
    Encoding.Free;
  end;
end;

procedure MoveUpdatedFiles(const Source, Dest: string);
begin
  //We won't bother adding with files not at the top folder.
  //Files in use (the running tms.exe) are moved to Dest\.locked, not to the working folder's locked folder: that can be
  //on another disk, and a running exe can only be renamed within its own disk. UMain.Cleanup removes them on the next run.
  var LockedFolder := Config.Folders.SameDiskLockedFolder;
  TDirectory_CreateDirectory(LockedFolder);

  var Files := TDirectory.GetFiles(Source, '*.*', TSearchOption.soTopDirectoryOnly);
  for var f in Files do
  begin
    var DestFile := TPath.Combine(Dest, TPath.GetFileName(f));
    AtomicMove(f, DestFile, TPath.Combine(LockedFolder, TPath.GetFileName(DestFile)) + '.' + GuidToStringN(TGUID.NewGuid) + TempExtension);
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
  var TmpFolder := Config.Folders.SameDiskTempSelfUpdateFolder;
  if TDirectory.Exists(TmpFolder, false) then
  begin
    DeleteFolderMovingToLocked(Config.Folders.SameDiskLockedFolder, TmpFolder, true, false);
    if TDirectory.Exists(TmpFolder, false) then raise Exception.Create('Cannot create the folder "' + TmpFolder + '" to unpack the new version. Verify it is not locked, and try removing it manually.');
  end;

  try
    TBundleDecompressor.ExtractCompressedFile(NewVersion, TmpFolder);

    //Verify the new tms.exe is signed by the same publisher as the running
    //binary BEFORE overwriting it. The bundle's `file_hash` comes from the
    //same server as the bundle, so the hash is not a trust anchor — only an
    //offline-signed binary is. See Commands.SelfUpdate.Verify for details.
    VerifySelfUpdateBundle(ParamStr(0), TmpFolder);
    EnsureHashFile(TmpFolder);
    TFile.WriteAllText(InstallingLock, 'A SmartSetup installation is in process and couldn''t be completed. You can retry `tms self-update` or reinstall again from https://doc.tmssoftware.com/smartsetup/download/index.html');
    MoveUpdatedFiles(TmpFolder, TPath.GetDirectoryName(ParamStr(0)));
    DeleteFileOrMoveToLocked(Config.Folders.SameDiskLockedFolder, InstallingLock);
  finally
    DeleteFolderMovingToLocked(Config.Folders.SameDiskLockedFolder, TmpFolder, true, false);
  end;


  //Must go after we autoupdated, but before the logs.
  //Because if the user choose to keep 0 files, this would remove the downloaded file before it was extracted.
  RotateDownloads(Config.MaxVersionsPerProduct);
  Logger.Info('');

  var NewVersionVer: TVersion;
  if TVersion.TryFromString(TDownloadFileName.ExtractVersion(NewVersion), NewVersionVer)
    and (NewVersionVer = CurrentVersion) then
  begin
    Logger.Info(Format('TMS Smart Setup installation of version %s has been repaired', [TMSVersion]));
  end else
  begin
    Logger.Info(Format('TMS Smart Setup has been updated from version %s to version %s', [TMSVersion, TDownloadFileName.ExtractVersion(NewVersion)]));
  end;
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

    var JsonOutputString: string;
    ExecuteCommand('"' + tms + '" info -json', '', JsonOutputString);

    var JsonOutputParsed := TJSONValue.ParseJSONValue(JsonOutputString);
    try
      if not (JsonOutputParsed is TJSONObject) then
      raise Exception.Create('Error getting version of the downloaded file. Could not parse info result as JSON object: "' + JsonOutputString + '"');
      var JsonObject := TJSONObject(JsonOutputParsed);

      var v := JsonObject.GetValue('tms version', 'invalid');
      if v = 'invalid' then raise Exception.Create('Invalid version in downloaded file: "' + JsonOutputString + '"');

      if not TVersion.TryFromString(V, Result) then raise Exception.Create('Invalid version number: "' + V + '"');
    finally
      JsonOutputParsed.Free;
    end;

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

  //No RotateDownloads here: with "max versions per product: 0" it deleted the bundle we just downloaded, and
  //AutoUpdate then said "You are using the latest version". AutoUpdate rotates once it is done with the bundle.
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

procedure VerifyBundleHashesFiles;
begin
  var Hashes := TFile.ReadAllLines(SmartSetupExeHash, TEncoding.UTF8);
  for var Hash in Hashes do
  begin
    if Hash.Trim = '' then continue;
    var idx := Hash.IndexOf(':');
    if idx <= 0 then raise Exception.Create('The file "' + SmartSetupExeHash + '" is corrupt. Invalid line: "' + Hash + '"');
    var FileName := Hash.Substring(0, idx).Trim;
    var ExpectedHash := Hash.Substring(idx + 1).Trim;

    var ActualFileName := TPath.Combine(Config.Folders.ExeFolder, FileName);
    if not TFile.Exists(ActualFileName) then raise Exception.Create('The file "' + ActualFileName + '" is missing.');

    var ActualHash := THashSHA2.GetHashStringFromFile(ActualFileName, THashSHA2.TSHA2Version.SHA256);
    if ExpectedHash <> ActualHash then raise Exception.Create('The file "' + ActualFileName + '" is not the correct one for this Smart Setup release.');
  end;
end;

procedure CheckSelfUpdating(const CommandName: string);
begin
  if not TFile.Exists(InstallingLock) then exit;

  try
    if not TFile.Exists(SmartSetupExeHash) then raise Exception.Create('The file "' + SmartSetupExeHash + '" doesn''t exist.');
    VerifyBundleHashesFiles;
  except
    on ex: Exception do
    begin
      //self-update will retry the install: GetNewVersion always updates while the lock exists.
      if SameText(CommandName, 'self-update') then exit;
      raise Exception.Create('Fatal: This smartsetup update didn''t complete correctly. Please run tms self-update to fix it, or if that doesnt''t work, try reinstalling from https://doc.tmssoftware.com/smartsetup/download/index.html. Error message: ' + ex.Message);
    end;
  end;

  //We are in a consistent state. The user might have reinstalled manually.
  //There is no need to reject the command, so we can remove the lock.
  DeleteFileOrMoveToLocked(Config.Folders.SameDiskLockedFolder, InstallingLock);
end;

end.

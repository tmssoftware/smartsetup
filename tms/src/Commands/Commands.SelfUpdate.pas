unit Commands.SelfUpdate;
interface

uses
  System.SysUtils, System.StrUtils, VSoft.CommandLine.Options, UCommandLine, UMultiLogger, Deget.Version;

procedure RegisterSelfUpdateCommand;
function NewSmartSetupAvailable: string; //returns empty is there are none.

var
  SmartSetupUpdated: Boolean;

implementation
uses
  Commands.CommonOptions, URepositoryManager, Commands.Logging, Commands.Update, IOUtils, UTmsBuildSystemUtils, Deget.CoreTypes,
  {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} //to keep compiler happy
  Commands.GlobalConfig, System.Zip, Actions.Fetch, Downloads.VersionManager;


const
  {$i ../../../Version.inc}

function RootFileName: string;
begin
  Result := TRepositoryManager.TMSSetupProductId + '_production_';
end;

function ExtractVersion(const FileName: string): string;
begin
  var OnlyFileName := TPath.GetFileNameWithoutExtension(FileName);
  Result := OnlyFileName.Substring(Length(RootFileName))
end;


function GetNewVersion: string;
begin
  Result := '';
  if not TDirectory.Exists(Config.Folders.DownloadsFolder) then Exit;
  var Updated := TDirectory.GetFiles(Config.Folders.DownloadsFolder, RootFileName + '*.zip');
  if Length(Updated) = 0 then exit;

  var Current: TVersion := TMSVersion;
  var MaxVersion := Current;
  for var FileName in Updated do
  begin
    var NextVersion: TVersion := ExtractVersion(FileName);
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
    Result := ExtractVersion(NewVersion);
  except on ex: Exception do
    //no log here, as it might not be initialized yet.
  end;
end;

procedure UnzipOneFile(const Zip: TZipFile; const RootFolder, NameOnZip: string);
begin
  if NameOnZip.EndsWith('/') then exit; //folder.

  var FileName := TPath.Combine(RootFolder, NameOnZip);
{$IFDEF MSWINDOWS} // ZIP stores files with '/', so translate to a relative Windows path.
    FileName := StringReplace(FileName, '/', '\', [rfReplaceAll]);
{$ENDIF}

  DeleteFileOrMoveToLocked(TPath.Combine(RootFolder, '.locked'), FileName);

  Zip.Extract(NameOnZip, RootFolder);

end;

procedure AutoUpdate;
begin
  var NewVersion := GetNewVersion;
  if NewVersion = '' then
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


  var Zip := TZipFile.Create;
  try
    Zip.Open(NewVersion, TZipMode.zmRead);
    for var i := 0 to Zip.FileCount - 1 do
    begin
      UnzipOneFile(Zip, RootFolder, Zip.FileNames[i]);
    end;
  finally
    Zip.Free;
  end;
  var FileName := '';
      TZipFile.ExtractZipFile(NewVersion, RootFolder);



  //Must go after we autoupdated, but before the logs.
  //Because if the user choose to keep 0 files, this would remove the downloaded file before it was extracted.
  RotateDownloads(Config.MaxVersionsPerProduct);
  Logger.Info('');

  Logger.Info(Format('TMS Smart Setup has been updated from version %s to version %s', [TMSVersion, ExtractVersion(NewVersion)]));
  SmartSetupUpdated := true;
end;

var
  NoFetch: Boolean = False;

procedure RunSelfUpdateCommand;
begin
  InitFolderBasedCommand;
  if not NoFetch then
    ExecuteFetchAction([TRepositoryManager.TMSSetupProductId], TFetchMode.OnlyInstalled);

  AutoUpdate;

end;

procedure RegisterSelfUpdateCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('self-update', '', 'Updates tms smart setup to the latest version',
    'Checks if there is a new tms smart setup version available, and if there is, downloads it and installs it',
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

unit ULogRotate;
{$i ../tmscommon.inc}

interface
procedure LogRotate(const LogFileFullName, SessionId: string); overload;
function ZipLogLocation(const LogFolder: string): string;

implementation
uses Classes, SysUtils, IOUtils, Zip, Generics.Defaults, Generics.Collections,
UTmsBuildSystemUtils,
{$IFDEF POSIX}
  Posix.UniStd,
{$ENDIF}
Commands.GlobalConfig;

const MaxLogs = 10;

function ZipLogLocation(const LogFolder: string): string;
begin
  Result := TPath.Combine(LogFolder, 'logs.zip');
end;

procedure TrimLogs(const LogFolder: string; const LogExtension: string);
begin
  var files := TDirectory.GetFiles(LogFolder, '*' + LogExtension);
  if Length(files) > MaxLogs then
  begin
    TArray.Sort<string>(files);
    for var i := 0 to Length(files) - MaxLogs - 1 do
    begin
      SysUtils.DeleteFile(files[i]);   //Not worth moving to locked. We will just retry to delete it next time.
    end;
  end;

end;

procedure LogRotate(const LogFolder, LogFile, SessionId, LogExtension: string); overload;
begin
  if LogFolder.Trim = '' then Exit;
  var LogFileName := TPath.Combine(LogFolder, LogFile);
  if not TFile.Exists(LogFileName) then Exit;

  var NewFileName := SessionId + LogExtension;
  TFile.Copy(LogFileName, TPath.Combine(LogFolder, NewFileName));
  TrimLogs(LogFolder, LogExtension);
  var zip := ZipLogLocation(LogFolder);
  DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, zip);
  TZipFile.ZipDirectoryContents(zip, LogFolder);
end;

procedure LogRotate(const LogFileFullName, SessionId: string); overload;
begin
  LogRotate(TPath.GetDirectoryName(LogFileFullName), TPath.GetFileName(LogFileFullName), SessionId, '.saved.log');
  LogRotate(TPath.GetDirectoryName(LogFileFullName), TPath.GetFileName(LogFileFullName) + '.html', SessionId, '.saved.log.html');
end;

end.

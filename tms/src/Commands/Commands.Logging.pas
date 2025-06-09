unit Commands.Logging;

interface

uses
{$IFDEF MSWINDOWS}
  WinApi.Windows,
{$ENDIF}
  System.IOUtils, System.SysUtils, UMultiLogger, UConsoleLogger, ULogger, ULogRotate, UPlainTextLogger, UHTMLLogger,
  Fetching.Options;

var
  FetchOptions: TFetchOptions; // better remove this later and not make logging depending on fetch options

procedure FinishLogging;
procedure AlertAboutNewVersions(const NewSmartSetupVersion: string);
procedure AlertAboutDiskSpace;
procedure InitFolderBasedCommand(EnableLog: Boolean = True);

implementation
uses Commands.GlobalConfig, commands.SelfUpdate, UTmsBuildSystemUtils, Deget.Version, Commands.CommonOptions;
const
  {$i ../../../Version.inc}

function GetFileSystemName(const Path: string): string;
{$IFDEF MSWINDOWS}
var
  VolumeName: array of Char;
  FileSystemBuffer: array of Char;
  SerialNumber, FileSystemFlags, ComponentLength: DWORD;
begin
  SetLength(VolumeName, MAX_PATH);
  SetLength(FileSystemBuffer, MAX_PATH);
  var Drive := IncludeTrailingPathDelimiter(ExtractFileDrive(Path));

  if not GetVolumeInformation(PChar(Drive), @VolumeName[0], MAX_PATH, @SerialNumber,
    ComponentLength, FileSystemFlags, @FileSystemBuffer[0], MAX_PATH) then
  begin
    Exit('Unknown');
  end;

  Result := PChar(FileSystemBuffer);
{$ELSE}
begin
  Result := 'Not retrieved';
{$ENDIF}
end;

procedure LogCommand;
var
  CommandLine: string;
  I: Integer;
begin
  CommandLine := ParamStr(0);
  for I := 1 to ParamCount do
    CommandLine := CommandLine + ' ' + ParamStr(I);
  Logger.Trace(CommandLine);
end;

var LogFile: string;

procedure StartLogging;
begin
  LogFile := Config.Folders.LogFile;
  TDirectory_CreateDirectory(TPath.GetDirectoryName(LogFile));
  Logger.AddLogger(TPlainTextLogger.Create(LogFile));
  Logger.AddLogger(THTMLLogger.Create(LogFile + '.html'));

  Logger.StartSection(TMessageType.BasicInfo, '');
  LogCommand;
  Logger.Trace('tms version ' + TMSVersion);
  Logger.Trace(TOSVersion.ToString);
  Logger.Trace('File system: ' + GetFileSystemName(TDirectory.GetCurrentDirectory));
  Logger.Trace('Current dir: ' + TDirectory.GetCurrentDirectory);
  Logger.FinishSection(TMessageType.BasicInfo);
end;

procedure FinishLogging;
begin
  if LogFile <> '' then
    LogRotate(LogFile);
  if ExitCode <> 0 then
  begin
    WriteLn;
    if (ExitCode <> 2) then // 2 means command-line parsing error, so no need for extra error message
    begin
      var LogZipFileName := '';
      if LogFile <> '' then
        LogZipFileName := ZipLogLocation(TPath.GetDirectoryName(LogFile));

      if TFile.Exists(LogZipFileName) then
        WriteLn('There were errors. If you need to contact support, please attach the logs at "'
           + LogZipFileName + '".')
      else
        WriteLn('There were errors. No log files generated.');

    if NeedsToRestartIDE then
    begin
      WriteLn;
      WriteLn('**********************************************************');
      WriteLn('** SOME FILES WERE LOCKED DURING THE PROCESS.           **');
      WriteLn('** PLEASE RESTART RAD STUDIO IF YOU WANT TO START USING **');
      WriteLn('** THE COMPONENTS THAT INSTALLED CORRECTLY.             **');
      WriteLn('**********************************************************');
      WriteLn;
    end;

    end;
  end
  else
  begin
    if NeedsToRestartIDE then
    begin
      WriteLn;
      WriteLn('*************************************************************************************');
      WriteLn('** THE INSTALLATION WAS SUCCESSFUL, BUT SOME FILES WERE LOCKED DURING THE PROCESS. **');
      WriteLn('** PLEASE RESTART RAD STUDIO TO START USING THE NEW COMPONENTS.                    **');
      WriteLn('*************************************************************************************');
      WriteLn;
    end;
  end;
end;

procedure InitFolderBasedCommand(EnableLog: Boolean = True);
begin
  if EnableLog then CheckAppAlreadyRunning;
  CheckValidTMSSetupFolder;
  if EnableLog then
    StartLogging;
end;

procedure AlertAboutNewVersions(const NewSmartSetupVersion: string);
begin
  if (NewSmartSetupVersion <> '') and not SmartSetupUpdated then
  begin
    WriteLn;
    WriteLn('There is a new version of TMS Smart Setup available');
    WriteLn(Format('You are using %s and %s is available.',[TVersion(TMSVersion).ToString, TVersion(NewSmartSetupVersion).ToString]));
    WriteLn('To update, you can call "tms self-update"');
  end;
end;

procedure AlertAboutDiskSpace;
begin
{$IFDEF MSWINDOWS}
  var DiskSpaceWarningLimit := 500 * 1024 * 1024; // 500 MB
  var TotalFree, TotalSpace: TLargeInteger;
  if GetDiskFreeSpaceEx(PChar(TPath.GetDirectoryName(ConfigFileName)), TotalFree, TotalSpace, nil) then
  begin
    // Warn if disk space is lower than 500 MB
    if TotalFree < DiskSpaceWarningLimit then
    begin
      WriteLn;
      WriteLn(Format('WARNING: You only have %d MB left in disk. TMS Smart Setup might not work properly.',
        [TotalFree div 1024 div 1024]));
      WriteLn('We recommend you to free up disk space. ');
    end;
  end;
{$ENDIF}
end;

initialization
  FetchOptions := TFetchOptions.Create;
  LogFile := '';

finalization
  FetchOptions.Free;
end.

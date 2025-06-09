unit UOSFileLinks;

interface
uses SysUtils;
type
  TFileLinkType = (HardLink, SymLink);

procedure CreateFileLink(const LockedFilesFolder, FileToLink, LinkToCreate: string; const LinkType: TFileLinkType); overload;
procedure CreateFileLink(const LockedFilesFolder, FileToLink, LinkToCreate: string; const LinkType: TFileLinkType; var NeedsToRestartIDE: boolean); overload;

implementation
uses IOUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  UMultiLogger,
  UTmsBuildSystemUtils;

procedure DoHardLink(const FileToLink, LinkToCreate: string);
begin
{$IFDEF MSWINDOWS}
  if CreateHardLink(PChar(LinkToCreate), PChar(FileToLink), nil) then
  begin
    Logger.Trace(Format('Hard link created from "%s" to "%s"', [LinkToCreate, FileToLink]));
    Exit;
  end;

  var LastError := GetLastError;
  raise Exception.Create(Format('Could not create hard link "%s" to "%s": (%d) %s', [
    LinkToCreate, FileToLink, LastError, SysErrorMessage(LastError)]));
{$ENDIF}

//mac and linux not still implemented.

end;

function CreateSymLink(const Link, Target: string): boolean;
const
  SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE = 2;
begin
//The method in SysUtils doesn't pass SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE in Windows, to not require admin rights
{$IFDEF MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
begin
  Result := False;

  if (Target = '') or (Link = '') or not CheckWin32Version(6, 0) then
    Exit;

  var Path := ExtractFilePath(Link);

  var Flags: DWord;
  if fsSymLink in FileSystemAttributes(Path) then
  begin
    if IsRelativePath(Target) then
      Flags := GetFileAttributes(PChar(IncludeTrailingPathDelimiter(Path) + Target))
    else
      Flags := GetFileAttributes(PChar(Target));

    if (Flags <> INVALID_FILE_ATTRIBUTES) and (faDirectory and Flags <> 0) then
      Flags := SYMBOLIC_LINK_FLAG_DIRECTORY
    else
      Flags := 0;

    var IsAtLeastWin10Build14972 :=
      ((Win32MajorVersion = 10) and (Win32BuildNumber >= 14972)) or
      (Win32MajorVersion >= 11);

    if IsAtLeastWin10Build14972 then
    begin
      Flags := Flags or SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE;
    end;

    Result := CreateSymbolicLink(PChar(Link), PChar(Target), Flags);
  end;
end;
{$WARN SYMBOL_PLATFORM ON}
{$ELSE}
  Result := FileCreateSymLink(Link, Target);
{$ENDIF}

end;

procedure DoSymLink(const FileToLink, LinkToCreate: string);
begin
  if CreateSymLink(LinkToCreate, FileToLink) then
  begin
    Logger.Trace(Format('Symlink created from "%s" to "%s"', [LinkToCreate, FileToLink]));
    Exit;
  end;

  var LastError := GetLastError;
  raise Exception.Create(Format('Could not create symlink "%s" to "%s": (%d) %s', [
    LinkToCreate, FileToLink, LastError, SysErrorMessage(LastError)]));
end;

procedure CreateFileLink(const LockedFilesFolder, FileToLink, LinkToCreate: string; const LinkType: TFileLinkType);
begin
  var NeedsToRestartIDE := false;
  CreateFileLink(LockedFilesFolder, FileToLink, LinkToCreate, LinkType, NeedsToRestartIDE)
end;

procedure CreateFileLink(const LockedFilesFolder, FileToLink, LinkToCreate: string; const LinkType: TFileLinkType; var NeedsToRestartIDE: boolean);
begin
  if TFile.Exists(LinkToCreate, false) then  //if we follow the symlink we wont delete it when the linked file doesn't exist.
    DeleteFileOrMoveToLocked(LockedFilesFolder, LinkToCreate, false, procedure (s: string) begin Logger.Trace(s); end, NeedsToRestartIDE);

  TDirectory_CreateDirectory(TPath.GetDirectoryName(LinkToCreate));

  case LinkType of
    HardLink: DoHardLink(FileToLink, LinkToCreate);
    SymLink: DoSymLink(FileToLink, LinkToCreate);
  end;

end;

end.

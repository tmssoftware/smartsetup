unit UOSShortcuts;
{$i ../tmscommon.inc}

interface
  procedure CreateShortcut(const LinkFileName, TargetFileName, Description, WorkingDirectory: string);
  procedure DeleteShortcut(const LockedFilesFolder, OrigFolder, LinkFileName: string);
  function GetStartMenuPath: string;

implementation
{$IFDEF MSWINDOWS}
uses ComObj, ShlObj, ActiveX, SysUtils, IOUtils, KnownFolders,
     UTmsBuildSystemUtils;

procedure Check(const R: HResult; const Err: string);
begin
  if R <> S_OK then raise Exception.Create(Err);

end;

procedure CreateShortcut(const LinkFileName, TargetFileName, Description, WorkingDirectory: string);
begin
  CoInitialize(nil);
  try
    var LinkFolder := TPath.GetDirectoryName(LinkFileName);
    if not TDirectory.Exists(LinkFolder) then TDirectory_CreateDirectory(LinkFolder);

    var ShellLinkObject := CreateComObject(CLSID_ShellLink);
    var ShellLink := ShellLinkObject as IShellLink;

    var Err := 'Can''t create shortcut for ' + TargetFileName;
    Check(ShellLink.SetDescription(PChar(Description)), Err);
    Check(ShellLink.SetPath(PChar(TargetFileName)), Err);
    Check(ShellLink.SetWorkingDirectory(PChar(WorkingDirectory)), Err);

    var FileLink := ShellLinkObject as IPersistFile;
    Err := 'Can''t save shortcut ' + LinkFileName;
    Check(FileLink.Save(PChar(LinkFileName), false), Err);
  finally
     CoUninitialize;
  end;
end;

procedure DeleteShortcut(const LockedFilesFolder, OrigFolder, LinkFileName: string);
begin
  if SameText(TPath.GetFullPath(OrigFolder), TPath.GetFullPath(TPath.GetDirectoryName(LinkFileName))) then
  begin
    DeleteFileOrMoveToLocked(LockedFilesFolder, LinkFileName);
  end
  else
  begin
    TryDeleteFileAndRemoveParentFolderIfEmpty(LockedFilesFolder, LinkFileName);
  end;
end;

function GetStartMenuPath: string;
var
  LStr: PChar;
begin
  Result := '';
  if SHGetKnownFolderPath(FOLDERID_Programs, 0, 0, LStr) = 0 then
  begin
    Result := LStr;
    CoTaskMemFree(LStr);
  end;
end;

{$ELSE}
procedure CreateShortcut(const LinkFileName, TargetFileName, Description, WorkingDirectory: string);
begin
end;

procedure DeleteShortcut(const LockedFilesFolder, OrigFolder, LinkFileName: string);
begin
end;

function GetStartMenuPath: string;
begin
end;

{$ENDIF}
end.

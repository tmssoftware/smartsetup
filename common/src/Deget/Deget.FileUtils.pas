unit Deget.FileUtils;

interface

function GetTempFileNameExt(Extension: string; const TempFolder: string = ''): string;
procedure CreateSymLink(const LinkFileName, TargetFileName, LockedFilesFolder: string);
procedure RemoveSymLink(const LinkFileName, LockedFilesFolder: string);
procedure CopyFilesToDirectory(const SourceFileMask, DestDir: string); overload;
procedure CopyFilesToDirectory(const SourceDir, FileMask, DestDir: string;
  Recursive: Boolean = False); overload;
procedure DeleteFilesFromDirectory(const Dir, FileMask, LockedFilesFolder: string);

// Add/remove directory to Windows PATH environment variable
{$IFDEF MSWINDOWS}
procedure AddEnvironmentPath(path: string);
procedure RemoveEnvironmentPath(path: string);
{$ENDIF}

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows, System.Win.Registry, Winapi.Messages,
{$ELSE}
  Posix.Unistd,
{$ENDIF}
  System.Types, System.SysUtils, System.IOUtils, System.Classes, System.StrUtils,
  UMultiLogger, UTmsBuildSystemUtils;

{$IFDEF MSWINDOWS}
function GetSystemEnvironmentVariable(const name: string; const Key: HKEY): string;
begin
  with TRegistry.Create do
  try
    RootKey := Key;
    OpenKey('SYSTEM\CurrentControlSet\Control\Session ' +
      'Manager\Environment', False);
    result := ReadString(name);
  finally
    Free;
  end;
end;

function CheckExists(SysPath: string; path: string): boolean;
var
  i: Integer;
  List: TStringList;
begin
  //This will return path in local machine and current user. If it is in any of them, we won't add it.
  List := TStringList.Create;
  try
    List.Delimiter := ';';
    List.StrictDelimiter := true;
    List.DelimitedText := SysPath;
    for i := 0 to List.Count - 1 do
    begin
      if SameText(Trim(List[i]), path) then exit (true);
    end;
    //path already exists.
  finally
    FreeAndNil(List);
  end;

  exit(false);
end;

procedure SetEnv(const name, value: string);
var
  dwResult: ULong_Ptr;

begin
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey('SYSTEM\CurrentControlSet\Control\Session ' +
      'Manager\Environment', False);
    WriteExpandString(name, value);
  finally
    Free;
  end;

  // notify other process.
  SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0,
       LParam(PChar('Environment')), SMTO_ABORTIFHUNG, 5000, @dwResult);

end;

procedure AddEnvironmentPath(path: string);
var
  SysPath: string;
begin
  path := trim(path);
  //syspath in currentuser is in other place. not worth
  //SysPath := GetSystemEnvironmentVariable('PATH', HKEY_CURRENT_USER);
  //if CheckExists(SysPath, path) then exit;

  SysPath := GetSystemEnvironmentVariable('PATH', HKEY_LOCAL_MACHINE); //now we need the value we are going to modify. If we used the "full" syspath here, we would add all the user entries to the machine entries.
  if CheckExists(SysPath, path) then exit;

  if (Length(SysPath) > 0) and (SysPath[Length(SysPath)] <> ';') then SysPath := SysPath + ';';
  SetEnv('PATH', SysPath + path);
end;

procedure RemoveEnvironmentPath(path: string);
var
  SysPath,s: string;
  p1, p1a, p2 : integer;
begin
  path := Trim(Path);
  SysPath := GetSystemEnvironmentVariable('PATH', HKEY_LOCAL_MACHINE); //We will only look in local machine. If we added it, we added it there. If not, the user added it.
  p1:= 1;
  repeat
    p2:= Posex(';', SysPath + ';', p1);
    s := Trim(copy(SysPath, p1, p2 - p1));
    if SameText(s, path) then
    begin
      p1a := p1 - 1; if p1a < 1 then p1a := 1; //Except when we are at the beginning, we want to delete the ";" before too.
      Delete(SysPath, p1a, p2 - p1a);
      SetEnv('PATH', SysPath);
      exit;
    end;
    p1 := p2 + 1;
  until p2 < 1;
end;
{$ENDIF}

procedure CopyFilesToDirectory(const SourceDir, FileMask, DestDir: string;
  Recursive: Boolean = False); overload;
begin
  Logger.Trace(Format('Copying files "%s\%s" to "%s"', [SourceDir, FileMask, DestDir]));
  TDirectory_CreateDirectory(DestDir);
  if TDirectory.Exists(SourceDir) then
  begin
    TDirectory.GetFiles(SourceDir, FileMask,
      function(const Path: string; const SearchRec: TSearchRec): Boolean
      begin
        Result := true;
        TFile.Copy(TPath.Combine(Path, SearchRec.Name), TPath.Combine(DestDir, SearchRec.Name), true);
//        Logger.Note(Format('"%s" copied', [SearchRec.Name]));
      end
    );

    if Recursive then
      TDirectory.GetDirectories(SourceDir, TSearchOption.soTopDirectoryOnly,
        function(const Path: string; const SearchRec: TSearchRec): Boolean
        begin
          Result := True;
          if (SearchRec.Name = '.') or (SearchRec.Name = '..') then Exit;

          CopyFilesToDirectory(
            TPath.Combine(Path, SearchRec.Name),
            '*.*',
            TPath.Combine(DestDir, SearchRec.Name),
            True);
        end
      );
  end;
end;

procedure CopyFilesToDirectory(const SourceFileMask, DestDir: string);
begin
  CopyFilesToDirectory(TPath.GetDirectoryName(SourceFileMask), TPath.GetFileName(SourceFileMask), DestDir);
end;


procedure DeleteFilesFromDirectory(const Dir, FileMask, LockedFilesFolder: string);
begin
  TDirectory.GetFiles(Dir, FileMask,
    function(const Path: string; const SearchRec: TSearchRec): Boolean
    begin
      Result := true;
      DeleteFileOrMoveToLocked(LockedFilesFolder, TPath.Combine(Path, SearchRec.Name));
//      Logger.Note(Format('"%s" deleted', [SearchRec.Name]));
    end
  );
end;

function GetTempFileNameExt(Extension: string; const TempFolder: string = ''): string;
begin
  var TempPath := TempFolder;
  if TempPath = '' then
    TempPath := TPath.GetTempPath;

  Result := TPath.Combine(TempPath, TPath.ChangeExtension(TPath.GetGUIDFileName, Extension));
end;

procedure CreateSymLink(const LinkFileName, TargetFileName, LockedFilesFolder: string);
begin
  DeleteFileOrMoveToLocked(LockedFilesFolder, LinkFileName);
  TDirectory_CreateDirectory(TPath.GetDirectoryName(LinkFileName));
  if not FileCreateSymLink(LinkFileName, TargetFileName) then
    RaiseLastOSError(GetLastError, sLineBreak + 'Can''t create symlink: ' + TargetFileName + ' => ' + LinkFileName);
end;

procedure RemoveSymLink(const LinkFileName, LockedFilesFolder: string);
begin
  DeleteFileOrMoveToLocked(LockedFilesFolder, LinkFileName);
end;

end.

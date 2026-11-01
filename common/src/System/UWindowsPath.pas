unit UWindowsPath;

interface
function GetUserWindowsPath: string;
function GetLocalMachineWindowsPath: string;
procedure SetUserWindowsPath(p: string);
procedure SetLocalMachineWindowsPath(p: string);

function AddToWindowsPath(path: string): boolean;
function RemoveFromWindowsPathWithChildren(path: string): TArray<string>;

function FindExeInPath(const Path: string; const ExeName: string): string;
function ExpandWindowsPath(const Path: string): string;


{$IFDEF DEBUG}
procedure WindowsPath_InternalTests;
{$ENDIF}

implementation
{$IFDEF MSWINDOWS}
uses Classes, Windows, Registry, Messages, SysUtils, StrUtils, SyncObjs,
  Testing.Globals, IOUtils, UTmsBuildSystemUtils;
const
  Mutex_WindowsPath = 'Global\tms-smart-setup-EC27DF85-D2BE-4C45-9882-7172FA00786A';

function GetWindowsPathLock: TMutex;
begin
  Result := TMutex.Create(nil, false, Mutex_WindowsPath);
end;

procedure SetUserEnv(const name, value: string);
var
  dwResult: ULong_Ptr;

begin
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    OpenKey('Environment', False);
    WriteExpandString(name, value);
  finally
    Free;
  end;

  // notify other process.
  SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0,
       LParam(PChar('Environment')), SMTO_ABORTIFHUNG, 5000, @dwResult);

end;

procedure SetLocalMachineEnv(const name, value: string);
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


function GetUserEnvironmentVariable(const name: string): string;
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    OpenKeyReadOnly('Environment');
    result := ReadString(name);
  finally
    Free;
  end;
end;

function GetLocalMachineEnvironmentVariable(const name: string): string;
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKeyReadOnly('SYSTEM\CurrentControlSet\Control\Session ' +
      'Manager\Environment');
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

function GetUserWindowsPath: string;
begin
  Result := '';
{$IFDEF DEBUG}
  if TestParameters.WindowsPath <> '' then exit(TestParameters.WindowsPath);
{$ENDIF}


  var Lock := GetWindowsPathLock;
  try
    Lock.Acquire;
    try
      Result := GetUserEnvironmentVariable('PATH').Trim;
    finally
      Lock.Release;
    end;
  finally
    Lock.Free;
  end;
end;

function GetLocalMachineWindowsPath: string;
begin
  Result := '';
{$IFDEF DEBUG}
  if TestParameters.WindowsPath <> '' then exit; //we will only set the user windows path
{$ENDIF}

  var Lock := GetWindowsPathLock;
  try
    Lock.Acquire;
    try
      Result := GetLocalMachineEnvironmentVariable('PATH').Trim;
    finally
      Lock.Release;
    end;
  finally
    Lock.Free;
  end;
end;

procedure SetUserWindowsPath(p: string);
begin
  var path := trim(p);
  if Length(path) = 0 then exit;

  var Lock := GetWindowsPathLock;
  try
    Lock.Acquire;
    try
      SetUserEnv('PATH', path);
    finally
      Lock.Release;
    end;
  finally
    Lock.Free;
  end;
end;

procedure SetLocalMachineWindowsPath(p: string);
begin
  var path := trim(p);
  if Length(path) = 0 then exit;

  var Lock := GetWindowsPathLock;
  try
    Lock.Acquire;
    try
      try
        SetLocalMachineEnv('PATH', path);
      except
        on ex:Exception do
        begin
          raise Exception.Create(ex.Message + ' - Verify you are running as ADMIN.');
        end;
      end;
    finally
      Lock.Release;
    end;
  finally
    Lock.Free;
  end;

end;


function AddToWindowsPath(path: string): boolean;
begin
  Result := false;
  path := trim(path);
  if Length(path) = 0 then exit;

  var Lock := GetWindowsPathLock;
  try
    Lock.Acquire;
    try
      var UserPath := GetUserEnvironmentVariable('PATH').Trim;
      if CheckExists(UserPath, path) then exit;

      var SysPath := GetLocalMachineEnvironmentVariable('PATH').Trim;
      if CheckExists(SysPath, path) then exit;

      if (Length(UserPath) > 0) and (UserPath[Length(UserPath)] <> ';') then UserPath := UserPath + ';';
      SetUserEnv('PATH', UserPath + path);
    finally
      Lock.Release;
    end;
  finally
    Lock.Free;
  end;
  Result := true;
end;

function IsPathInside(TestedPath, RootPath: string): boolean;
var
  FullTestedPath, FullRootPath: string;
begin
  TestedPath := TestedPath.Trim;
  RootPath := RootPath.Trim;
  if TestedPath = '' then exit(false);
  if RootPath = '' then exit(false);
  //For relative paths, we only remove them if they are a text match.
  FullTestedPath := IncludeTrailingPathDelimiter(TestedPath);
  FullRootPath := IncludeTrailingPathDelimiter(RootPath);
  if TPath_IsPathRootedNoValidation(TestedPath) and TPath_IsPathRootedNoValidation(RootPath) then
  begin
    // a path like c:\a\b\..\c would in theory not have been set by us, but anyway,
    // if we are removing smartsetup from the path, we should remove if anyway.
    try
      var NormalizedTestedPath := IncludeTrailingPathDelimiter(TPath.GetFullPath(TestedPath));
      var NormalizedRootPath := IncludeTrailingPathDelimiter(TPath.GetFullPath(RootPath));
      FullTestedPath := NormalizedTestedPath;
      FullRootPath := NormalizedRootPath;
    except
      //GetFullPath raises on invalid chars like | or ". We keep the text match for those.
      on Exception do;
    end;
  end;

  Result := FullTestedPath.StartsWith(FullRootPath, true)
end;

function RemoveMatchesFromWindowsPath(const Path, WindowsPath: string; out Removed: TArray<string>): string;
begin
  Removed := nil;
  Result := '';
  var HasResult := false;
  var Starting := 0;
  while Starting <= WindowsPath.Length do
  begin
    var Ending := WindowsPath.IndexOf(';', Starting);
    if Ending < 0 then Ending := WindowsPath.Length + 1;
    var Entry := WindowsPath.Substring(Starting, Ending - Starting);
    if IsPathInside(Entry, Path) then
    begin
      Removed := Removed + [Entry];
      Starting := Ending + 1;
      continue;
    end;
    if HasResult then Result := Result + ';';
    Result := Result + Entry;
    HasResult := true;

    Starting := Ending + 1;
  end;

  if Removed = nil then exit(WindowsPath);

end;

function RemoveFromWindowsPathWithChildren(path: string): TArray<string>;
begin
  var Lock := GetWindowsPathLock;
  try
    Lock.Acquire;
    try
      Result := nil;
      var WindowsPath := GetUserEnvironmentVariable('PATH'); //We will only look in user. If we added it, we added it there. If not, the user added it.
      var NewWindowsPath := RemoveMatchesFromWindowsPath(path, WindowsPath, Result);

      if WindowsPath <> NewWindowsPath then SetUserEnv('PATH', NewWindowsPath);
    finally
      Lock.Release;
    end;
  finally
    Lock.Free;
  end;
end;

function ExpandEnvironmentVariables(const variable: string): string;
begin
  var ExpandedLen := ExpandEnvironmentStrings(pchar(variable), nil, 0);
  if (ExpandedLen <= 1) then exit('');

  SetLength(Result, ExpandedLen);
  ExpandEnvironmentStrings(pchar(variable), @Result[1], length(Result));
  SetLength(Result, Length(Result) - 1); //remove #0
end;

function FindExeInPath(const Path: string; const ExeName: string): string;
begin
  Result := '';
  if Path.Trim = '' then exit;
  var Entries := Path.Split([';']);
  if Length(Entries) = 0 then exit;

  for var p in Entries do
  begin
    var ExpandedPath := ExpandEnvironmentVariables(p).Trim;
    if ExpandedPath = '' then continue;
    var ExeLocation := TPath.Combine(ExpandedPath, ExeName);
    if TFile.Exists(ExeLocation) then exit(ExeLocation);


  end;
end;

function ExpandWindowsPath(const Path: string): string;
const
  MAXSIZE = 32768;
begin
  Result := '';
  SetLength(Result, MAXSIZE);
  var ResultLength := ExpandEnvironmentStrings(PCHAR(Path), @Result[1], Length(Result));
  SetLength(Result, ResultLength - 1);
end;

{$ELSE}
uses Classes, SysUtils, StrUtils, SyncObjs, IOUtils;

function GetUserWindowsPath: string;
begin
  Result := ''
end;

function GetLocalMachineWindowsPath: string;
begin
  Result := GetEnvironmentVariable('PATH');
end;

procedure SetUserWindowsPath(p: string);
begin
end;

procedure SetLocalMachineWindowsPath(p: string);
begin
end;


function AddToWindowsPath(path: string): boolean;
begin
  Result := false;
end;

function RemoveFromWindowsPathWithChildren(path: string): TArray<string>;
begin
  Result := nil;
end;

function FindExeInPath(const Path: string; const ExeName: string): string;
begin
  Result := '';
  if Path.Trim = '' then exit;
  var Entries := Path.Split([':']);
  if Length(Entries) = 0 then exit;

  for var p in Entries do
  begin
    var ExpandedPath := p.Trim; //No expanded variables in linux
    if ExpandedPath = '' then continue;
    var ExeLocation := TPath.Combine(ExpandedPath, ExeName);
    if TFile.Exists(ExeLocation) then exit(ExeLocation);


  end;
end;

function ExpandWindowsPath(const Path: string): string;
begin
  Result := Path;
end;


{$ENDIF}
{$IFDEF DEBUG}
procedure WindowsPath_InternalTests;
begin
{$IFDEF MSWINDOWS}
  var Removed: TArray<string>;
  Assert(RemoveMatchesFromWindowsPath('','c:\test;c:\test2', Removed) = 'c:\test;c:\test2');
  Assert(RemoveMatchesFromWindowsPath('C:\potato\..\test','c:\test;c:\test2', Removed) = 'c:\test2');
  Assert(RemoveMatchesFromWindowsPath('C:\potato\..\test','c:\test;c:\test;c:\test2;c:\test', Removed) = 'c:\test2');
  Assert(RemoveMatchesFromWindowsPath('C:\potato\..\test','c:\test;c:\test;c:\test2', Removed) = 'c:\test2');
  Assert(RemoveMatchesFromWindowsPath('potato\..\test','test;c:\test2', Removed) = 'test;c:\test2');
  Assert(RemoveMatchesFromWindowsPath('potato\..\test','test;potato\..\test', Removed) = 'test');
  Assert(RemoveMatchesFromWindowsPath('C:\test','c:\test', Removed) = '');
  Assert(RemoveMatchesFromWindowsPath('b','a;b', Removed) = 'a');
  Assert(RemoveMatchesFromWindowsPath('a','a;b', Removed) = 'b');
  Assert(RemoveMatchesFromWindowsPath('b','a;b;c', Removed) = 'a;c');
  Assert(RemoveMatchesFromWindowsPath('b','a;b;b\c;c', Removed) = 'a;c');
  Assert(RemoveMatchesFromWindowsPath('C:\test',';c:\test', Removed) = '');
  Assert(RemoveMatchesFromWindowsPath('C:\test','c:\test;', Removed) = '');
  Assert(RemoveMatchesFromWindowsPath('C:\test','c:\test;c:\test;c:\test;c:\test', Removed) = '');
  Assert(RemoveMatchesFromWindowsPath('C:\test',';c:\test;c:\test;c:\test;c:\test;', Removed) = ';');
  Assert(RemoveMatchesFromWindowsPath('C:\test','a;c:\test;c:\test;c:\test;c:\test;', Removed) = 'a;');
  Assert(RemoveMatchesFromWindowsPath('C:\test',';c:\test;c:\test;c:\test;c:\test;a', Removed) = ';a');
  Assert(RemoveMatchesFromWindowsPath('C:\test','c:\test1;c:\test;c:\test2;', Removed) = 'c:\test1;c:\test2;');
  Assert(RemoveMatchesFromWindowsPath('C:\test','c:\test;c:\test;c:\test2;', Removed) = 'c:\test2;');
  Assert(RemoveMatchesFromWindowsPath('C:\test','c:\test;c:\test;c:\test2;c:\test', Removed) = 'c:\test2');
  Assert(RemoveMatchesFromWindowsPath('C:\test','c:\test1;c:\test\potato;c:\test\tomato\other;c:\test2;c:\test\cucumber\', Removed) = 'c:\test1;c:\test2');
  Assert(RemoveMatchesFromWindowsPath('C:\test','c:\test1;;;c:\test\potato;c:\test\tomato\other;c:\test2;c:\test\cucumber\', Removed) = 'c:\test1;;;c:\test2');
  Assert(RemoveMatchesFromWindowsPath('C:\test', 'a;c:\test;c:\test\x', Removed) = 'a');
  Assert(RemoveMatchesFromWindowsPath('C:\test', ';;;c:\test;;;', Removed) = ';;;;;');
  Assert(RemoveMatchesFromWindowsPath('C:\test', ' ; ; ; c:\test ; ; ; ', Removed) = ' ; ; ; ; ; ');
  Assert(RemoveMatchesFromWindowsPath('C:\test', 'c:\a|b;c:\test\x;"c:\test"', Removed) = 'c:\a|b;"c:\test"');
  Assert(RemoveMatchesFromWindowsPath('C:\test', 'a;c:\test\a|b;c:\test\<x>', Removed) = 'a');
{$ENDIF}

end;

{$ENDIF}
end.

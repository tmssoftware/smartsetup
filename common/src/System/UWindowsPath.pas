unit UWindowsPath;

interface
function GetUserWindowsPath: string;
function GetLocalMachineWindowsPath: string;
procedure SetUserWindowsPath(p: string);
procedure SetLocalMachineWindowsPath(p: string);

function AddToWindowsPath(path: string): boolean;
function RemoveFromWindowsPathWithChildren(path: string): TArray<string>;

function FindExeInPath(const Path: string; const ExeName: string): string;

implementation
{$IFDEF MSWINDOWS}
uses Classes, Windows, Registry, Messages, SysUtils, StrUtils, SyncObjs, IOUtils;

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

  var Lock := TMutex.Create(nil, false, 'Global\tms-smart-setup-EC27DF85-D2BE-4C45-9882-7172FA00786A');
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

  var Lock := TMutex.Create(nil, false, 'Global\tms-smart-setup-EC27DF85-D2BE-4C45-9882-7172FA00786A');
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

  var Lock := TMutex.Create(nil, false, 'Global\tms-smart-setup-EC27DF85-D2BE-4C45-9882-7172FA00786A');
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

  var Lock := TMutex.Create(nil, false, 'Global\tms-smart-setup-EC27DF85-D2BE-4C45-9882-7172FA00786A');
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

  var Lock := TMutex.Create(nil, false, 'Global\tms-smart-setup-EC27DF85-D2BE-4C45-9882-7172FA00786A');
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

function RemoveOneMatchFromWindowsPath(path, WindowsPath: string; var Removed: TArray<string>): string;
var
  p2: integer;
begin
  Path := Trim(Path);
  var p1 := 1;

  repeat
    p2:= Posex(';', WindowsPath + ';', p1);
    var s := Trim(copy(WindowsPath, p1, p2 - p1));
    if s.StartsWith(path, true) then
    begin
      var p1a := p1 - 1; if p1a < 1 then p1a := 1; //Except when we are at the beginning, we want to delete the ";" before too.
      Removed := Removed + [System.Copy(WindowsPath, p1, p2 - p1)];
      System.Delete(WindowsPath, p1a, p2 - p1a);
      exit(WindowsPath);
    end;

    p1 := p2 + 1;

  until p2 < 1;
  Result := WindowsPath;
end;

function RemoveFromWindowsPathWithChildren(path: string): TArray<string>;
begin
  Result := nil;
  var WindowsPath := GetUserEnvironmentVariable('PATH'); //We will only look in user. If we added it, we added it there. If not, the user added it.
  var NewWindowsPath := WindowsPath;
  while True do
  begin
    var TmpWindowsPath := RemoveOneMatchFromWindowsPath(path, NewWindowsPath, Result);
    if TmpWindowsPath = NewWindowsPath then break;
    NewWindowsPath := TmpWindowsPath;
  end;

  if WindowsPath <> NewWindowsPath then SetUserEnv('PATH', NewWindowsPath);

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

{$ENDIF}
end.

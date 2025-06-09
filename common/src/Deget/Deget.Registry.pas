unit Deget.Registry;

interface

{$IFDEF MSWINDOWS}

uses
  Winapi.Windows, System.Win.Registry;

function RegQueryStringValue(Root: HKEY; const Key, Name: string; out Value: string): boolean; overload;
function RegQueryStringValue(const Key, Name: string; out Value: string): boolean; overload;
function RegDeleteValue(Root: HKEY; const Key, Ident: string): boolean;
function RegDeleteKey(Root: HKEY; const Key: string): boolean;
function RegKeyExists(Root: HKEY; const Key: string): boolean;
function RegReadString(Root: HKEY; const Key, Ident: string; out Value: string): boolean;
function RegValueExists(Root: HKEY; const Key, Ident: string): boolean;
function RegKeyHasValuesOrSubkeys(Root: HKEY; const Key: string): boolean;
procedure RegWriteString(Root: HKEY; const Key, Ident, Value: string; const CanCreate: boolean = true);
procedure RegWriteInteger(Root: HKEY; const Key, Ident: string; Value: Integer);

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses
  System.SysUtils;

function RegQueryStringValue(Root: HKEY; const Key, Name: string; out Value: string): boolean; overload;
var
  R: TRegistry;
begin
  R := TRegistry.Create;
  try
    R.RootKey := Root;
    R.OpenKey(Key, false);
    Result := R.ValueExists(Name);
    if Result then
      Value := R.ReadString(Name);
  finally
    R.Free;
  end;
end;

function RegQueryStringValue(const Key, Name: string; out Value: string): boolean; overload;
begin
  Result := RegQueryStringValue(HKEY_CURRENT_USER, Key, Name, Value);
  if not Result then
    Result := RegQueryStringValue(HKEY_LOCAL_MACHINE, Key, Name, Value);
end;

function RegDeleteValue(Root: HKEY; const Key, Ident: string): boolean;
var
  R: TRegistry;
begin
//  Logger.Note(Format('Deleting registry value: %s\%s (%s)', [Key, Ident, IntToHex(Root, 8)]));
  R := TRegistry.Create;
  try
    R.RootKey := Root;
    Result := R.OpenKey(Key, false) and R.ValueExists(Ident);
//    Logger.Info('Value exists: %s', [BoolToStr(Result, true)]);
    if Result then
    begin
      R.DeleteValue(Ident);
//      Logger.Info('Value deleted.');
    end;
  finally
    R.Free;
  end;
//  Logger.Leave;
end;

function RegDeleteKey(Root: HKEY; const Key: string): boolean;
var
  R: TRegistry;
begin
//  Logger.Note(Format('Deleting registry value: %s\%s (%s)', [Key, Ident, IntToHex(Root, 8)]));
  R := TRegistry.Create;
  try
    R.RootKey := Root;
    Result := r.DeleteKey(Key);
  finally
    R.Free;
  end;
//  Logger.Leave;
end;

function RegKeyExists(Root: HKEY; const Key: string): boolean;
var
  R: TRegistry;
//  Logger: ILogger;
begin
//  Logger := TLogManager.GetLogger;
//  Logger.Enter('Check existing key: %s (%s)', [Key, IntToHex(Root, 8)]);
  R := TRegistry.Create;
  try
    R.RootKey := Root;
    Result := R.KeyExists(Key);
//    Logger.Info('Key exists: %s', [BoolToStr(Result, true)]);
  finally
    R.Free;
  end;
//  Logger.Leave;
end;

function RegReadString(Root: HKEY; const Key, Ident: string;
  out Value: string): boolean;
var
  R: TRegistry;
begin
  R := TRegistry.Create;
  try
    R.RootKey := Root;
    Result := R.OpenKeyReadOnly(Key) and R.ValueExists(Ident);
//    Logger.Info('Value exists: %s', [BoolToStr(Result, true)]);
    if Result then
      Value := R.ReadString(Ident)
    else
      Value := '';
//    Logger.Note(Format('Value %s read registry: %s\%s (%s)', [Value, Key, Ident, IntToHex(Root, 8)]));
  finally
    R.Free;
  end;
end;

function RegValueExists(Root: HKEY; const Key, Ident: string): boolean;
var
  R: TRegistry;
//  Logger: ILogger;
begin
//  Logger := TLogManager.GetLogger;
//  Logger.Enter('Check existing value: %s\%s (%s)', [Key, Ident, IntToHex(Root, 8)]);
  R := TRegistry.Create;
  try
    R.RootKey := Root;
    Result := R.OpenKeyReadOnly(Key) and R.ValueExists(Ident);
//    Logger.Info('Value exists: %s', [BoolToStr(Result, true)]);
  finally
    R.Free;
  end;
//  Logger.Leave;
end;

function RegKeyHasValuesOrSubkeys(Root: HKEY; const Key: string): boolean;
var
  R: TRegistry;
  Info: TRegKeyInfo;

begin
  R := TRegistry.Create;
  try
    R.RootKey := Root;
    if not R.OpenKeyReadOnly(Key) then exit(false);

    Result := R.GetKeyInfo(Info) and ((Info.NumSubKeys > 0) or (Info.NumValues > 0));

  finally
    R.Free;
  end;
//  Logger.Leave;
end;

procedure RegWriteString(Root: HKEY; const Key, Ident, Value: string; const CanCreate: boolean = true);
var
  R: TRegistry;
begin
//  Logger.Note(Format('Writing string value: %s\%s : %s (%s)', [Key, Ident, Value, IntToHex(Root, 8)]));
  R := TRegistry.Create;
  try
    R.RootKey := Root;
    if R.OpenKey(Key, CanCreate) then
      R.WriteString(Ident, Value);
//    Logger.Info('Value written');
  finally
    R.Free;
  end;
//  Logger.Leave;
end;

procedure RegWriteInteger(Root: HKEY; const Key, Ident: string; Value: Integer);
var
  R: TRegistry;
begin
//  Logger.Note(Format('Writing Integer value: %s\%s : %d (%s)', [Key, Ident, Value, IntToHex(Root, 8)]));
  R := TRegistry.Create;
  try
    R.RootKey := Root;
    if R.OpenKey(Key, true) then
      R.WriteInteger(Ident, Value);
  finally
    R.Free;
  end;
end;

{$ENDIF}

end.

unit Util.Credentials;
// from https://stackoverflow.com/questions/13145112/secure-way-to-store-password-in-windows
// To change/add the credentials, go to Control Panel\All Control Panel Items\Credential Manager
// Add them with 'Add generic credential'
interface
{$IFDEF MSWINDOWS}
type
  TWindowsCredential = record
  public
    Name: string;
    User: string;
    Password: string;

    constructor Create(const aName, aUser, aPassword: string);
  end;

function CredReadGenericCredentials(const Target: UnicodeString; out Username, Password: UnicodeString; const ThrowExceptions: boolean = true): string;
procedure CredWriteGenericCredentials(const Target: UnicodeString; Username, Password: UnicodeString);
function CredDeleteGenericCredential(const Target: UnicodeString; const ThrowExceptions: boolean = true): string;
function EnumerateGenericCredentials(const Filter: string): TArray<TWindowsCredential>;

{$ENDIF}
implementation
{$IFDEF MSWINDOWS}
uses SysUtils, Classes, Windows, Winapi.WinCred;

function CredReadGenericCredentials(const Target: UnicodeString; out Username, Password: UnicodeString; const ThrowExceptions: boolean = true): string;
var
    Credential: PCREDENTIALW;
    le: DWORD;
begin
  Result := '';
  Credential := nil;
  if not CredReadW(PWideChar(WideString(Target)), CRED_TYPE_GENERIC, 0, {var}Credential) then
  begin
    le := GetLastError;
    Result := 'Could not get "'+ Target + '" generic credentials: '+SysErrorMessage(le)+' '+IntToStr(le);
    if ThrowExceptions then raise Exception.Create(Result) else
    begin
      UserName := '';
      Password := '';
      exit;
    end;
  end;

  try
    Username := Credential.UserName;
    SetString(Password, PWideChar(Credential.CredentialBlob), Credential.CredentialBlobSize div 2); //By convention blobs that contain strings do not have a trailing NULL.
  finally
    CredFree(Credential);
  end;
end;

procedure CredWriteGenericCredentials(const Target: UnicodeString; Username, Password: UnicodeString);
var
  Credential: CREDENTIALW;
  le: DWORD;
  PassBytes: TBytes;
begin
  ZeroMemory(@Credential, SizeOf(Credential));
  Credential.&Type := CRED_TYPE_GENERIC;
  Credential.TargetName := PChar(Target);
  Credential.UserName := PChar(UserName);
  Credential.Persist := CRED_PERSIST_LOCAL_MACHINE;
  PassBytes := TEncoding.Unicode.GetBytes(Password);  //No trailing #0
  Credential.CredentialBlobSize := Length(PassBytes);
  if Length(PassBytes) > 0 then Credential.CredentialBlob := @(PassBytes[0]);
  if not CredWriteW(@Credential, 0) then
  begin
    le := GetLastError;
    raise Exception.Create('Could not write "'+ Target + '" in generic credentials: '+SysErrorMessage(le)+' '+IntToStr(le));
  end;
end;

function CredDeleteGenericCredential(const Target: UnicodeString; const ThrowExceptions: boolean): string;
var
  le: DWORD;
begin
  Result := '';
  if not CredDelete(PChar(Target), CRED_TYPE_GENERIC, 0) then
  begin
    le := GetLastError;
    Result := 'Could not delete "'+ Target + '" from generic credentials: '+SysErrorMessage(le)+' '+IntToStr(le);
    if ThrowExceptions then raise Exception.Create(Result);
  end;

end;

function EnumerateGenericCredentials(const Filter: string): TArray<TWindowsCredential>;
var
  Count: DWORD;
  Credentials: PPCREDENTIAL;
begin
  Result := nil;
  Count := 0;
  Credentials := nil;
  try
    // Enumerate all credentials
    if not CredEnumerate(PWideChar(WideString(Filter)), 0, Count, Credentials) then
    begin
      // No credentials found or error occurred
      Exit;
    end;

    for var i := 0 to Count - 1 do
    begin
     {$POINTERMATH ON}
      var Cred := Credentials[i];
     {$POINTERMATH OFF}
      if Cred.&Type = CRED_TYPE_GENERIC then
      begin
        var Password := '';
        SetString(Password, PWideChar(Cred.CredentialBlob), Cred.CredentialBlobSize div 2); //By convention blobs that contain strings do not have a trailing NULL.
        Result := Result + [TWindowsCredential.Create(Cred.TargetName, Cred.UserName, Password)];
      end;
    end;

  finally
    CredFree(Credentials);
  end;




end;


{ TWindowsCredential }

constructor TWindowsCredential.Create(const aName, aUser, aPassword: string);
begin
  Name := aName;
  User := aUser;
  Password := aPassword;
end;
{$ENDIF}
end.

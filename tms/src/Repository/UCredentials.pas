unit UCredentials;

interface

uses
  System.IniFiles, System.SysUtils, System.IOUtils, System.DateUtils, Fetching.Options, Util.Credentials;

type
  TCredentials = class
  strict private
    FEmail: string;
    FCode: string;
    FAccessToken: string;
    FExpiration: TDateTime;
  private
    procedure SetCode(const Value: string);
    procedure SetEmail(const Value: string);
  public
    property Email: string read FEmail write SetEmail;
    property Code: string read FCode write SetCode;
    property AccessToken: string read FAccessToken write FAccessToken;
    property Expiration: TDateTime read FExpiration write FExpiration;
  end;

  TCredentialsManager = class
  public const
    EnvEmail = 'SMARTSETUP_EMAIL';
    EnvCode = 'SMARTSETUP_CODE';
    EnvServer = 'tms'; //Environment variables only apply to this server.
  private const
    IniEmail = 'email';
    IniCode = 'code';
    IniToken = 'token';
    IniExpiration = 'expiration';
  private
    FCredentialsFile: string;
    FDefaultProfile: string;
    FServerName: string;
    procedure LoadCredentials(Credentials: TCredentials);
    //Do* versions don't lock, so they can call each other. Locks aren't reentrant in posix.
    procedure DoLoadCredentials(Credentials: TCredentials);
    procedure DoSaveCredentials(Credentials: TCredentials; const OnlyToken: boolean);
    function AuthCredName(Profile: string): string;
    function TokensCredName(Profile: string): string;
    function CredName(Profile, Name: string): string;
    procedure LoadEnvironmentCredentials(Credentials: TCredentials);
    procedure SaveEnvironmentCredentials(Credentials: TCredentials; const OnlyToken: boolean);
  protected
    function RetrieveAccessToken(const AuthUrl: string): string;
  public
    constructor Create(const ACredentialsFile, DefaultProfile, ServerName: string);
    destructor Destroy; override;

    procedure UpdateAccessToken(Credentials: TCredentials; const AuthUrl: string);

    procedure SaveCredentials(Credentials: TCredentials; const OnlyToken: boolean);
    function ReadCredentials: TCredentials;

    // True if credentials for this server come from SMARTSETUP_EMAIL / SMARTSETUP_CODE.
    // In that case the Credential Manager (or credentials file) is completely ignored.
    function UsesEnvironmentCredentials: boolean;
  public
    class function GetAccessToken(const CredentialsFile: string; Options: TFetchOptions; const AuthUrl, Server: string): string;
  end;

function CreateCredentialsManager(const CredentialsFile: string; Options: TFetchOptions; const ServerName: string): TCredentialsManager;

implementation

uses
  System.Classes, UMultiLogger, REST.Authenticator.OAuth, Testing.Globals, ZSTD, SmartSetup.Locks;

const
  ZstdPrefix = 'zstd:';

var
  // Access token cache for credentials coming from environment variables.
  // We can't persist it anywhere (that's the point of using env vars), so it only lives for this process.
  EnvTokenLock: TObject;
  EnvAccessToken: string;
  EnvTokenExpiration: TDateTime;

function CompressToken(const Token: string): TBytes;
begin
  Result := nil;
  if Token = '' then Exit(nil);
  var TokenBytes := TEncoding.UTF8.GetBytes(Token);
  var Source := TBytesStream.Create(TokenBytes);
  try
    var Dest := TBytesStream.Create;
    try
      ZSTDCompressStream(Source, Dest, 8);
      var ZstdBytes := TEncoding.UTF8.GetBytes(ZstdPrefix);
      SetLength(Result, Length(ZstdBytes) + Dest.Size);
      System.Move(ZstdBytes[0], Result[0], Length(ZstdBytes));
      if Dest.Size > 0 then System.Move(Dest.Bytes[0], Result[Length(ZstdBytes)], Dest.Size);

    finally
      Dest.Free;
    end;
  finally
    Source.Free;
  end;
end;

function TokenIsZstd(const Token: TBytes): boolean;
begin
  var Prefix := TEncoding.UTF8.GetBytes(ZstdPrefix);
  if Length(Token) < Length(Prefix) then Exit(False);
  for var i := 0 to Length(Prefix) - 1 do
    if Token[i] <> Prefix[i] then Exit(False);
  Result := True;
end;

function DecompressToken(const Token: TBytes): string;
begin
  if Token = nil then Exit('');
  if not TokenIsZstd(Token) then exit(TEncoding.Unicode.GetString(Token));

  var Source := TBytesStream.Create(Token);
  try
    var Dest := TBytesStream.Create;
    try
      Source.Position := Length(TEncoding.UTF8.GetBytes(ZstdPrefix));
      ZSTDDecompressStream(Source, Dest);
      Result := TEncoding.UTF8.GetString(Dest.Bytes, 0, Dest.Size);
    finally
      Dest.Free;
    end;
  finally
    Source.Free;
  end;
end;

function CreateCredentialsManager(const CredentialsFile: string; Options: TFetchOptions; const ServerName: string): TCredentialsManager;
begin
  Result := TCredentialsManager.Create(CredentialsFile, Options.TargetRepository, ServerName);
end;

{ TCredentialsManager }

constructor TCredentialsManager.Create(const ACredentialsFile, DefaultProfile, ServerName: string);
begin
  FCredentialsFile := ACredentialsFile;
{$IFDEF DEBUG}
  if TestParameters.NoCredentials then FCredentialsFile := '$invalid-credentials';
{$ENDIF}
  FDefaultProfile := DefaultProfile;
  FServerName := ServerName;
end;

destructor TCredentialsManager.Destroy;
begin
  inherited;
end;

class function TCredentialsManager.GetAccessToken(const CredentialsFile: string; Options: TFetchOptions; const AuthUrl, Server: string): string;
begin
  var Manager := TCredentialsManager.Create(CredentialsFile, Options.TargetRepository, Server);
  try
    Result := Manager.RetrieveAccessToken(AuthUrl);
  finally
    Manager.Free;
  end;
end;

function TCredentialsManager.RetrieveAccessToken(const AuthUrl: string): string;
begin
  var Credentials := ReadCredentials;
  try
    // Use a 5-minute margin to check for token expiration. See https://github.com/tmssoftware/tms-smartsetup/issues/301
    if (Credentials.AccessToken <> '') and (Now < IncMinute(Credentials.Expiration, -5)) then
      Exit(Credentials.AccessToken);

    if (Credentials.Email = '') or (Credentials.Code = '') then
    begin
      if UsesEnvironmentCredentials then
        raise Exception.Create('Credentials for the ' + FServerName + ' server are being read from environment variables, but they are incomplete. Both ' + EnvEmail + ' and ' + EnvCode + ' must be set.');

      raise Exception.Create('Credentials not provided. Use "tms credentials" to access the ' + FServerName + ' server, or disable it with "tms server-enable ' + FServerName + ' false"');
    end;

    // Retrieve access token using credentials
    if Credentials.AccessToken <> '' then
      Logger.Trace('Access token expired, retrieving a new one')
    else
      Logger.Trace('Retrieving access token using credentials');

    UpdateAccessToken(Credentials, AuthUrl);

    // Save access token
    SaveCredentials(Credentials, true);

    // Return
    Result := Credentials.AccessToken;
  finally
    Credentials.Free;
  end;
end;

function TCredentialsManager.CredName(Profile, Name: string): string;
begin
{$IFDEF DEBUG}
  if TestParameters.CredentialsProfile <> '' then Profile := TestParameters.CredentialsProfile;
{$ENDIF}
  var ProfileDot := Profile;
  if Profile = 'production' then ProfileDot := ''; //backwards compat, also production is the most common case.
  
  if ProfileDot <> '' then ProfileDot := ProfileDot + '.';
  Result := 'tms.smartsetup.' + ProfileDot + TPath.GetFileName(FCredentialsFile) + Name;
end;

function TCredentialsManager.AuthCredName(Profile: string): string;
begin
  Result := CredName(Profile, '.auth');
end;

function TCredentialsManager.TokensCredName(Profile: string): string;
begin
  Result := CredName(Profile, '.tokens');
end;

procedure TCredentialsManager.DoLoadCredentials(Credentials: TCredentials);
begin
{$IFDEF MSWINDOWS}
  var Email, Code: string;

  var Error := CredReadGenericCredentials(AuthCredName(FDefaultProfile), Email, Code, false);
  if Error <> '' then
  begin
    Logger.Trace(Error);
  end;
  Credentials.Email := Email;
  Credentials.Code := Code;

  var Expiration: string;
  var AccessToken: TBytes;
  var Error2 := CredReadGenericCredentials(TokensCredName(FDefaultProfile), Expiration, AccessToken, false);
  if Error2 <> '' then
  begin
    Logger.Trace(Error2);
  end;

  Credentials.Expiration := 0;
  var ExpirationDate: TDateTime := Now;
  if Expiration <> ''
    then if TryISO8601ToDate(Expiration, ExpirationDate, False)
      then  Credentials.Expiration := ExpirationDate;

  Credentials.AccessToken := DecompressToken(AccessToken);
  if Credentials.Email <> '' then
  begin
    if TFile.Exists(FCredentialsFile) then TFile.Delete(FCredentialsFile);
    exit;
  end;

  //if no credentials, try reading legacy ones.
{$ENDIF}
  var IniFile := TMemIniFile.Create(FCredentialsFile);
  try
    var IniSection := FDefaultProfile;
    Credentials.Email := IniFile.ReadString(IniSection, IniEmail, '');
    Credentials.Code := IniFile.ReadString(IniSection, IniCode, '');
    Credentials.AccessToken := IniFile.ReadString(IniSection, IniToken, '');
    var IsoDate := IniFile.ReadString(IniSection, IniExpiration, '');
    if IsoDate <> '' then
      Credentials.Expiration := ISO8601ToDate(IsoDate, False)
    else
      Credentials.Expiration := 0;
  finally
    IniFile.Free;
  end;

{$IFDEF MSWINDOWS}
  //found the legacy credentials. Delete them, and save them in the new place.
  //If we didn't find any, don't save: with an empty email it would delete the credentials another process might have just saved.
  if Credentials.Email <> '' then
  begin
    DoSaveCredentials(Credentials, false);
    if TFile.Exists(FCredentialsFile) then TFile.Delete(FCredentialsFile);
  end;
{$ENDIF}
end;

procedure TCredentialsManager.LoadCredentials(Credentials: TCredentials);
begin
  var Protector := LockCredentials;
  try
    DoLoadCredentials(Credentials);
  finally
    Protector.Free;
  end;
end;

function TCredentialsManager.UsesEnvironmentCredentials: boolean;
begin
  if not SameText(FServerName, EnvServer) then exit(false);
  //If any of them is set, we use the environment. A missing one is reported as an error, instead of silently mixing sources.
  Result := (Trim(GetEnvironmentVariable(EnvEmail)) <> '') or (Trim(GetEnvironmentVariable(EnvCode)) <> '');
end;

procedure TCredentialsManager.LoadEnvironmentCredentials(Credentials: TCredentials);
begin
  Logger.Trace('Reading credentials for server "' + FServerName + '" from environment variables ' + EnvEmail + ' and ' + EnvCode + '. Stored credentials are ignored.');
  //Set Email and Code first: their setters clear the access token.
  Credentials.Email := Trim(GetEnvironmentVariable(EnvEmail));
  Credentials.Code := Trim(GetEnvironmentVariable(EnvCode));

  TMonitor.Enter(EnvTokenLock);
  try
    Credentials.AccessToken := EnvAccessToken;
    Credentials.Expiration := EnvTokenExpiration;
  finally
    TMonitor.Exit(EnvTokenLock);
  end;
end;

procedure TCredentialsManager.SaveEnvironmentCredentials(Credentials: TCredentials; const OnlyToken: boolean);
begin
  if not OnlyToken then
    raise Exception.Create('Cannot save credentials for server "' + FServerName + '" because they are being read from environment variables ' + EnvEmail + ' and ' + EnvCode + '.');

  TMonitor.Enter(EnvTokenLock);
  try
    EnvAccessToken := Credentials.AccessToken;
    EnvTokenExpiration := Credentials.Expiration;
  finally
    TMonitor.Exit(EnvTokenLock);
  end;
end;

function TCredentialsManager.ReadCredentials: TCredentials;
begin
  Result := TCredentials.Create;
  try
    if UsesEnvironmentCredentials then
    begin
      LoadEnvironmentCredentials(Result);
      exit;
    end;
    LoadCredentials(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TCredentialsManager.DoSaveCredentials(Credentials: TCredentials; const OnlyToken: boolean);
begin
{$IFDEF MSWINDOWS}
  try
    if not OnlyToken then
    begin
      if String.IsNullOrWhiteSpace(Credentials.Email) or String.IsNullOrWhiteSpace(Credentials.Code) then
      begin
        var CmdResult := CredDeleteGenericCredential(AuthCredName(FDefaultProfile), false);
        if CmdResult <> '' then Logger.Trace(CmdResult);

        CmdResult := CredDeleteGenericCredential(TokensCredName(FDefaultProfile), false);
        if CmdResult <> '' then Logger.Trace(CmdResult);
        exit;
      end;

      CredWriteGenericCredentials(AuthCredName(FDefaultProfile), Credentials.Email, Credentials.Code);
    end;

    var Expiration := '';
    if YearOf(Credentials.Expiration) > 1900 then
        Expiration := DateToISO8601(TTimeZone.Local.ToUniversalTime(Credentials.Expiration));

    CredWriteGenericCredentials(TokensCredName(FDefaultProfile), Expiration, CompressToken(Credentials.AccessToken));
  except
    on E: ECredentialsError do
    begin
      if E.ErrorCode <> CRED_ERROR_NO_SUCH_LOGON_SESSION then raise;
      raise Exception.Create(E.Message + sLineBreak
        + 'The Windows Credential Manager is not available in this session. This usually happens when running '
        + 'from SSH, a remote PowerShell session, a service, a CI agent or a scheduled task.' + sLineBreak
        + 'In this case, provide the credentials for the "' + EnvServer + '" server in the environment variables '
        + EnvEmail + ' and ' + EnvCode + ' instead of using "tms credentials".');
    end;
  end;
{$ELSE}
  var IniFile := TMemIniFile.Create(FCredentialsFile);
  try
    var IniSection := FDefaultProfile;
    IniFile.WriteString(IniSection, IniEmail, Credentials.Email);
    IniFile.WriteString(IniSection, IniCode, Credentials.Code);
    if Credentials.AccessToken <> '' then
      IniFile.WriteString(IniSection, IniToken, Credentials.AccessToken)
    else
      IniFile.DeleteKey(IniSection, IniToken);
    if YearOf(Credentials.Expiration) > 1900 then
      IniFile.WriteString(IniSection, IniExpiration, DateToISO8601(TTimeZone.Local.ToUniversalTime(Credentials.Expiration)))
    else
      IniFile.DeleteKey(Inisection, IniExpiration);
    IniFile.UpdateFile;
  finally
    IniFile.Free;
  end;
{$ENDIF}
end;

procedure TCredentialsManager.SaveCredentials(Credentials: TCredentials; const OnlyToken: boolean);
begin
  if UsesEnvironmentCredentials then
  begin
    SaveEnvironmentCredentials(Credentials, OnlyToken);
    exit;
  end;

  var Protector := LockCredentials;
  try
    DoSaveCredentials(Credentials, OnlyToken);
  finally
    Protector.Free;
  end;
end;

procedure TCredentialsManager.UpdateAccessToken(Credentials: TCredentials; const AuthUrl: string);
begin
  var OAuth := TOAuth2Authenticator.Create(nil);
  try
  {$IFDEF DEBUG}
    TestParameters.CheckOffline('credentials');
  {$ENDIF}
    OAuth.ClientID := Credentials.Email;
    OAuth.ClientSecret := Credentials.Code;
    OAuth.AccessTokenEndpoint := AuthUrl.TrimRight(['/']) + '/oauth/token';
    OAuth.AuthorizeWithClientCredentials;
    Credentials.AccessToken := OAuth.AccessToken;
    Credentials.Expiration := OAuth.AccessTokenExpiry;
    Logger.Trace('Access token retrieved');
  finally
    OAuth.Free;
  end;
end;

{ TCredentials }

procedure TCredentials.SetCode(const Value: string);
begin
  if FCode <> Value then
  begin
    FCode := Value;
    FAccessToken := '';
    FExpiration := 0;
  end;
end;

procedure TCredentials.SetEmail(const Value: string);
begin
  if FEmail <> Value then
  begin
    FEmail := Value;
    FAccessToken := '';
    FExpiration := 0;
  end;
end;

initialization
  EnvTokenLock := TObject.Create;
finalization
  EnvTokenLock.Free;
end.

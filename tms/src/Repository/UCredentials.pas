unit UCredentials;

interface

uses
  System.IniFiles, System.SysUtils, System.IOUtils, System.DateUtils, Fetching.Options, Util.Credentials,
  UConfigDefinition, URepositoryInfo, Auth.Client;

type
  TCredentials = class
  strict private
    FEmail: string;
    FCode: string;
    FAccessToken: string;
    FExpiration: TDateTime;
    FRefreshToken: string;
  private
    procedure SetCode(const Value: string);
    procedure SetEmail(const Value: string);
  public
    property Email: string read FEmail write SetEmail;
    property Code: string read FCode write SetCode;
    property AccessToken: string read FAccessToken write FAccessToken;
    property Expiration: TDateTime read FExpiration write FExpiration;
    property RefreshToken: string read FRefreshToken write FRefreshToken;
  end;

  // Everything needed to talk to an OIDC provider for one server, resolved from
  // the server config (and, for the built-in tms server, from the repository profile).
  TOidcServerParams = record
    Authority: string;
    ClientId: string;
    Scope: string;
    AuthorizationEndpoint: string;
    TokenEndpoint: string;

    class function Resolve(const ServerConfig: TServerConfig; const RepoInfo: IRepositoryInfo): TOidcServerParams; static;
  end;

  TCredentialsManager = class
  private const
    IniEmail = 'email';
    IniCode = 'code';
    IniToken = 'token';
    IniExpiration = 'expiration';
    IniRefreshToken = 'refreshtoken';
  private
    FCredentialsFile: string;
    FDefaultProfile: string;
    FServerName: string;
    procedure LoadCredentials(Credentials: TCredentials);
    function AuthCredName(Profile: string): string;
    function TokensCredName(Profile: string): string;
    function RefreshCredName(Profile: string): string;
    function CredName(Profile, Name: string): string;
    function LoginCommandHint: string;
  protected
    function RetrieveAccessToken(const AuthUrl: string): string;
    function RetrieveOidcAccessToken(const Params: TOidcServerParams): string;
  public
    constructor Create(const ACredentialsFile, DefaultProfile, ServerName: string);
    destructor Destroy; override;

    procedure UpdateAccessToken(Credentials: TCredentials; const AuthUrl: string);

    procedure SaveCredentials(Credentials: TCredentials; const OnlyToken: boolean);
    procedure SaveOidcTokens(Credentials: TCredentials);
    procedure ClearOidcTokens;
    procedure ClearLegacyCredentials;
    function ReadCredentials: TCredentials;
  public
    class function GetAccessToken(const CredentialsFile: string; Options: TFetchOptions; const AuthUrl, Server: string): string;
    class function GetOidcAccessToken(const CredentialsFile: string; Options: TFetchOptions; const ServerConfig: TServerConfig; const RepoInfo: IRepositoryInfo): string;

    // The auth mode actually used to get tokens for a server, as opposed to the
    // configured ServerConfig.AuthMode: it applies the TMSSETUP_AUTH_MODE override
    // and, on Oidc servers, falls back to stored e-mail/code credentials from a
    // previous version (grandfathering, governed by TMSLegacyCredentialsPolicy).
    class function EffectiveAuthMode(const CredentialsFile: string; Options: TFetchOptions;
      const ServerConfig: TServerConfig): TServerAuthMode;
  end;

// Applies the TMSSETUP_AUTH_MODE environment variable ('credentials' or 'oidc') to a
// server's configured auth mode. Undocumented on purpose: it is an escape hatch for
// support to unblock users during the email/code -> browser sign-in migration.
function ApplyAuthModeOverride(const ConfiguredMode: TServerAuthMode; out Mode: TServerAuthMode): Boolean;

function CreateCredentialsManager(const CredentialsFile: string; Options: TFetchOptions; const ServerName: string): TCredentialsManager;
function CreateOidcClient(const Params: TOidcServerParams; const RedirectUri: string = ''): TOidcClient;
function CompressToken(const Token: string): TBytes;

implementation

uses
  System.Classes, UMultiLogger, Testing.Globals, ZSTD, Auth.Classes;

const
  ZstdPrefix = 'zstd:';

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

function ApplyAuthModeOverride(const ConfiguredMode: TServerAuthMode; out Mode: TServerAuthMode): Boolean;
begin
  Mode := ConfiguredMode;
  var Value := GetEnvironmentVariable('TMSSETUP_AUTH_MODE');
  if SameText(Value, 'credentials') then Mode := TServerAuthMode.Credentials
  else if SameText(Value, 'oidc') then Mode := TServerAuthMode.Oidc
  else
  begin
    if Value <> '' then
      Logger.Info('Ignoring unknown TMSSETUP_AUTH_MODE value "' + Value + '" (expected "credentials" or "oidc")');
    Exit(False);
  end;
  Result := True;
  if Mode <> ConfiguredMode then
    Logger.Trace('Auth mode overridden to ' + Value + ' by TMSSETUP_AUTH_MODE');
end;

{ TCredentialsManager }

constructor TCredentialsManager.Create(const ACredentialsFile, DefaultProfile, ServerName: string);
begin
  FCredentialsFile := ACredentialsFile;
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
      raise Exception.Create('Credentials not provided. Use "tms credentials" to access the ' + FServerName + ' server, or disable it with "tms server-enable ' + FServerName + ' false"');

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

function TCredentialsManager.RefreshCredName(Profile: string): string;
begin
  Result := CredName(Profile, '.refresh');
end;

function TCredentialsManager.LoginCommandHint: string;
begin
  if SameText(FServerName, 'tms') then
    Result := 'tms credentials'
  else
    Result := 'tms credentials -server:' + FServerName;
end;

procedure TCredentialsManager.LoadCredentials(Credentials: TCredentials);
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

  var RefreshUser: string;
  var RefreshToken: TBytes;
  var Error3 := CredReadGenericCredentials(RefreshCredName(FDefaultProfile), RefreshUser, RefreshToken, false);
  if Error3 <> '' then
  begin
    Logger.Trace(Error3);
  end;
  Credentials.RefreshToken := DecompressToken(RefreshToken);

  if (Credentials.Email <> '') or (Credentials.RefreshToken <> '') then
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
    Credentials.RefreshToken := IniFile.ReadString(IniSection, IniRefreshToken, '');
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
  SaveCredentials(Credentials, false);
  if TFile.Exists(FCredentialsFile) then TFile.Delete(FCredentialsFile);
{$ENDIF}
end;

function TCredentialsManager.ReadCredentials: TCredentials;
begin
  Result := TCredentials.Create;
  try
    LoadCredentials(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TCredentialsManager.SaveCredentials(Credentials: TCredentials; const OnlyToken: boolean);
begin
{$IFDEF MSWINDOWS}
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

procedure TCredentialsManager.SaveOidcTokens(Credentials: TCredentials);
begin
{$IFDEF MSWINDOWS}
  var Expiration := '';
  if YearOf(Credentials.Expiration) > 1900 then
    Expiration := DateToISO8601(TTimeZone.Local.ToUniversalTime(Credentials.Expiration));

  CredWriteGenericCredentials(TokensCredName(FDefaultProfile), Expiration, CompressToken(Credentials.AccessToken));
  CredWriteGenericCredentials(RefreshCredName(FDefaultProfile), '', CompressToken(Credentials.RefreshToken));
{$ELSE}
  var IniFile := TMemIniFile.Create(FCredentialsFile);
  try
    var IniSection := FDefaultProfile;
    if Credentials.AccessToken <> '' then
      IniFile.WriteString(IniSection, IniToken, Credentials.AccessToken)
    else
      IniFile.DeleteKey(IniSection, IniToken);
    if Credentials.RefreshToken <> '' then
      IniFile.WriteString(IniSection, IniRefreshToken, Credentials.RefreshToken)
    else
      IniFile.DeleteKey(IniSection, IniRefreshToken);
    if YearOf(Credentials.Expiration) > 1900 then
      IniFile.WriteString(IniSection, IniExpiration, DateToISO8601(TTimeZone.Local.ToUniversalTime(Credentials.Expiration)))
    else
      IniFile.DeleteKey(IniSection, IniExpiration);
    IniFile.UpdateFile;
  finally
    IniFile.Free;
  end;
{$ENDIF}
end;

procedure TCredentialsManager.ClearLegacyCredentials;
begin
{$IFDEF MSWINDOWS}
  var CmdResult := CredDeleteGenericCredential(AuthCredName(FDefaultProfile), false);
  if CmdResult <> '' then Logger.Trace(CmdResult);
{$ELSE}
  if not TFile.Exists(FCredentialsFile) then exit;
  var IniFile := TMemIniFile.Create(FCredentialsFile);
  try
    IniFile.DeleteKey(FDefaultProfile, IniEmail);
    IniFile.DeleteKey(FDefaultProfile, IniCode);
    IniFile.UpdateFile;
  finally
    IniFile.Free;
  end;
{$ENDIF}
end;

procedure TCredentialsManager.ClearOidcTokens;
begin
{$IFDEF MSWINDOWS}
  var CmdResult := CredDeleteGenericCredential(TokensCredName(FDefaultProfile), false);
  if CmdResult <> '' then Logger.Trace(CmdResult);

  CmdResult := CredDeleteGenericCredential(RefreshCredName(FDefaultProfile), false);
  if CmdResult <> '' then Logger.Trace(CmdResult);
{$ELSE}
  if not TFile.Exists(FCredentialsFile) then exit;
  var IniFile := TMemIniFile.Create(FCredentialsFile);
  try
    var IniSection := FDefaultProfile;
    IniFile.DeleteKey(IniSection, IniToken);
    IniFile.DeleteKey(IniSection, IniRefreshToken);
    IniFile.DeleteKey(IniSection, IniExpiration);
    IniFile.UpdateFile;
  finally
    IniFile.Free;
  end;
{$ENDIF}
end;

function TCredentialsManager.RetrieveOidcAccessToken(const Params: TOidcServerParams): string;
begin
  var Credentials := ReadCredentials;
  try
    // Use a 5-minute margin to check for token expiration. See https://github.com/tmssoftware/tms-smartsetup/issues/301
    if (Credentials.AccessToken <> '') and (Now < IncMinute(Credentials.Expiration, -5)) then
      Exit(Credentials.AccessToken);

    if Credentials.RefreshToken = '' then
      raise Exception.Create('oauth2: not signed in to the ' + FServerName + ' server. Run "'
        + LoginCommandHint + '" to sign in, or disable the server with "tms server-enable ' + FServerName + ' false"');

    if Credentials.AccessToken <> '' then
      Logger.Trace('Access token expired, refreshing it')
    else
      Logger.Trace('Retrieving access token using refresh token');

    var Client := CreateOidcClient(Params);
    try
      try
        var AuthResult: ITokenResult := Client.RefreshTokens(Credentials.RefreshToken, Params.Scope);
        Credentials.AccessToken := AuthResult.AccessToken;
        Credentials.Expiration := AuthResult.Expiration;
        if AuthResult.RefreshToken <> '' then
          Credentials.RefreshToken := AuthResult.RefreshToken; // the server may rotate refresh tokens
      except
        on E: Exception do
          raise Exception.Create('oauth2: could not refresh the access token for the ' + FServerName
            + ' server (' + E.Message + '). Run "' + LoginCommandHint + '" to sign in again.');
      end;
    finally
      Client.Free;
    end;

    SaveOidcTokens(Credentials);
    Result := Credentials.AccessToken;
  finally
    Credentials.Free;
  end;
end;

var
  LegacyCredentialsWarned: Boolean = False;

class function TCredentialsManager.EffectiveAuthMode(const CredentialsFile: string; Options: TFetchOptions;
  const ServerConfig: TServerConfig): TServerAuthMode;
begin
  // An explicit TMSSETUP_AUTH_MODE wins over everything, including grandfathering:
  // it states what the user (or support) wants, so don't second-guess it.
  if ApplyAuthModeOverride(ServerConfig.AuthMode, Result) then Exit;

  if Result <> TServerAuthMode.Oidc then Exit;
  if TServerConfig.TMSLegacyCredentialsPolicy = TLegacyCredentialsPolicy.Deny then Exit;

  // Grandfathering: an Oidc server with no browser sign-in yet, but with e-mail/code
  // credentials stored by a previous version, keeps using them. Updating tms.exe
  // therefore changes nothing for existing users; a successful "tms credentials"
  // browser sign-in deletes the old credentials and ends the grandfathering.
  var Manager := TCredentialsManager.Create(CredentialsFile, Options.TargetRepository, ServerConfig.Name);
  try
    var Credentials := Manager.ReadCredentials;
    try
      if (Credentials.RefreshToken = '') and (Credentials.Email <> '') and (Credentials.Code <> '') then
      begin
        Result := TServerAuthMode.Credentials;
        if (TServerConfig.TMSLegacyCredentialsPolicy = TLegacyCredentialsPolicy.Warn)
          and not LegacyCredentialsWarned then
        begin
          LegacyCredentialsWarned := True;
          Logger.Info('You are using stored e-mail/code credentials for the ' + ServerConfig.Name
            + ' server. They are deprecated and will stop working in a future release: run "'
            + Manager.LoginCommandHint + '" to switch to browser sign-in.');
        end;
      end;
    finally
      Credentials.Free;
    end;
  finally
    Manager.Free;
  end;
end;

class function TCredentialsManager.GetOidcAccessToken(const CredentialsFile: string; Options: TFetchOptions;
  const ServerConfig: TServerConfig; const RepoInfo: IRepositoryInfo): string;
begin
  var Manager := TCredentialsManager.Create(CredentialsFile, Options.TargetRepository, ServerConfig.Name);
  try
    Result := Manager.RetrieveOidcAccessToken(TOidcServerParams.Resolve(ServerConfig, RepoInfo));
  finally
    Manager.Free;
  end;
end;

procedure TCredentialsManager.UpdateAccessToken(Credentials: TCredentials; const AuthUrl: string);
begin
  // Same client_credentials grant (client_id = email, client_secret = code, both as
  // form parameters) that TOAuth2Authenticator used to send, but through TOidcClient
  // so OAuth errors keep their error code: callers can tell "credentials retired by
  // the server" (OAuthErrorCodes.CredentialsAuthDisabled) apart from "credentials wrong".
  var Client := TOidcClient.Create;
  try
    Client.Authority := AuthUrl;
    Client.ClientId := Credentials.Email;
    Client.ClientSecret := Credentials.Code;
    Client.ClientSecretInBody := True;
    Client.AutoDiscover := False;
    Client.ProviderInfo.TokenEndpoint := AuthUrl.TrimRight(['/']) + '/oauth/token';
    var Tokens: ITokenResult := Client.RequestToken;
    Credentials.AccessToken := Tokens.AccessToken;
    Credentials.Expiration := Tokens.Expiration;
    Logger.Trace('Access token retrieved');
  finally
    Client.Free;
  end;
end;

{ TOidcServerParams }

class function TOidcServerParams.Resolve(const ServerConfig: TServerConfig; const RepoInfo: IRepositoryInfo): TOidcServerParams;
begin
  Result := Default(TOidcServerParams);
  Result.ClientId := ServerConfig.OidcClientId;
  Result.Scope := ServerConfig.OidcScope;
  Result.AuthorizationEndpoint := ServerConfig.OidcAuthorizationEndpoint;
  Result.TokenEndpoint := ServerConfig.OidcTokenEndpoint;

  // The built-in tms server has no fixed authority: it depends on the repository
  // profile (production/sandbox/local), so it is resolved the same way as the
  // AuthUrl used by the classic email/code flow.
  Result.Authority := ServerConfig.OidcAuthority;
  if (Result.Authority = '') and (RepoInfo <> nil) then
    Result.Authority := RepoInfo.AuthUrl;
end;

function CreateOidcClient(const Params: TOidcServerParams; const RedirectUri: string = ''): TOidcClient;
begin
  Result := TOidcClient.Create;
  try
    Result.Authority := Params.Authority;
    Result.ClientId := Params.ClientId;
    Result.Scope := Params.Scope;
    Result.RedirectUri := RedirectUri;
    // When both endpoints are given explicitly, the provider does not need a
    // discovery document. If only some data is missing, discovery fills the gaps.
    Result.ProviderInfo.AuthorizationEndpoint := Params.AuthorizationEndpoint;
    Result.ProviderInfo.TokenEndpoint := Params.TokenEndpoint;
    Result.AutoDiscover := (Params.AuthorizationEndpoint = '') or (Params.TokenEndpoint = '');
  except
    Result.Free;
    raise;
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

end.

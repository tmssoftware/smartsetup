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
  private const
    IniEmail = 'email';
    IniCode = 'code';
    IniToken = 'token';
    IniExpiration = 'expiration';
  private
    FCredentialsFile: string;
    FDefaultProfile: string;
    FServerName: string;
    procedure LoadCredentials(Credentials: TCredentials; const Profile: string);
    function AuthCredName(Profile: string): string;
    function TokensCredName(Profile: string): string;
    function CredName(Profile, Name: string): string;
  protected
    function RetrieveAccessToken(const AuthUrl: string; const Profile: string = ''): string;
  public
    constructor Create(const ACredentialsFile, DefaultProfile, ServerName: string);
    destructor Destroy; override;

    procedure UpdateAccessToken(Credentials: TCredentials; const AuthUrl: string);

    procedure SaveCredentials(Credentials: TCredentials; const Profile: string = '');
    function ReadCredentials(const Profile: string = ''): TCredentials;
  public
    class function GetAccessToken(const CredentialsFile: string; Options: TFetchOptions; const AuthUrl, Server: string): string;
  end;

function CreateCredentialsManager(const CredentialsFile: string; Options: TFetchOptions; const ServerName: string): TCredentialsManager;

implementation

uses
  System.NetEncoding, UMultiLogger, REST.Authenticator.OAuth, Testing.Globals;

function CreateCredentialsManager(const CredentialsFile: string; Options: TFetchOptions; const ServerName: string): TCredentialsManager;
begin
  Result := TCredentialsManager.Create(CredentialsFile, Options.TargetRepository, ServerName);
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

function TCredentialsManager.RetrieveAccessToken(const AuthUrl: string; const Profile: string): string;
begin
  var Credentials := ReadCredentials(Profile);
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
    SaveCredentials(Credentials, Profile);

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
  var ProfileDot := Profile; if ProfileDot <> '' then ProfileDot := ProfileDot + '.';
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

procedure TCredentialsManager.LoadCredentials(Credentials: TCredentials; const Profile: string);
begin
{$IFDEF MSWINDOWS}
  var Email, Code: string;

  var Error := CredReadGenericCredentials(AuthCredName(Profile), Email, Code, false);
  if Error <> '' then
  begin
    Logger.Trace(Error);
  end;
  Credentials.Email := Email;
  Credentials.Code := Code;

  var Expiration, AccessToken: string;
  var Error2 := CredReadGenericCredentials(TokensCredName(Profile), Expiration, AccessToken, false);
  if Error2 <> '' then
  begin
    Logger.Trace(Error2);
  end;

  if Expiration <> ''
    then Credentials.Expiration := ISO8601ToDate(Expiration, False)
    else Credentials.Expiration := 0;

  Credentials.AccessToken := AccessToken;
  if Credentials.Email <> '' then
  begin
    if TFile.Exists(FCredentialsFile) then TFile.Delete(FCredentialsFile);
    exit;
  end;

  //if no credentials, try reading legacy ones.
{$ENDIF}
  var IniFile := TMemIniFile.Create(FCredentialsFile);
  try
    var IniSection := Profile;
    if IniSection = '' then
      IniSection := FDefaultProfile;
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
  SaveCredentials(Credentials, Profile);
  if TFile.Exists(FCredentialsFile) then TFile.Delete(FCredentialsFile);
{$ENDIF}
end;

function TCredentialsManager.ReadCredentials(const Profile: string): TCredentials;
begin
  Result := TCredentials.Create;
  try
    LoadCredentials(Result, Profile);
  except
    Result.Free;
    raise;
  end;
end;

procedure TCredentialsManager.SaveCredentials(Credentials: TCredentials; const Profile: string);
begin
{$IFDEF MSWINDOWS}
  if String.IsNullOrWhiteSpace(Credentials.Email) or String.IsNullOrWhiteSpace(Credentials.Code) then
  begin
    var CmdResult := CredDeleteGenericCredential(AuthCredName(Profile), false);
    if CmdResult <> '' then Logger.Trace(CmdResult);

    CmdResult := CredDeleteGenericCredential(TokensCredName(Profile), false);
    if CmdResult <> '' then Logger.Trace(CmdResult);
    exit;
  end;

  CredWriteGenericCredentials(AuthCredName(Profile), Credentials.Email, Credentials.Code);

  var Expiration := '';
  if YearOf(Credentials.Expiration) > 1900 then
      Expiration := DateToISO8601(TTimeZone.Local.ToUniversalTime(Credentials.Expiration));

  CredWriteGenericCredentials(TokensCredName(Profile), Expiration, Credentials.AccessToken);
{$ELSE}
  var IniFile := TMemIniFile.Create(FCredentialsFile);
  try
    var IniSection := Profile;
    if IniSection = '' then
      IniSection := FDefaultProfile;
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

procedure TCredentialsManager.UpdateAccessToken(Credentials: TCredentials; const AuthUrl: string);
begin
  var OAuth := TOAuth2Authenticator.Create(nil);
  try
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

end.

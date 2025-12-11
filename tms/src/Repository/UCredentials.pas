unit UCredentials;

interface

uses
  System.IniFiles, System.SysUtils, System.IOUtils, System.DateUtils, Fetching.Options;

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
    procedure LoadCredentials(Credentials: TCredentials; const Profile: string);
  protected
    function RetrieveAccessToken(const AuthUrl: string; const Profile: string = ''): string;
  public
    constructor Create(const ACredentialsFile, DefaultProfile: string);
    destructor Destroy; override;

    procedure UpdateAccessToken(Credentials: TCredentials; const AuthUrl: string);

    procedure SaveCredentials(Credentials: TCredentials; const Profile: string = '');
    function ReadCredentials(const Profile: string = ''): TCredentials;
  public
    class function GetAccessToken(const CredentialsFile: string; Options: TFetchOptions; const AuthUrl: string): string;
  end;

function CreateCredentialsManager(const CredentialsFile: string; Options: TFetchOptions): TCredentialsManager;

implementation

uses
  System.NetEncoding, UMultiLogger, REST.Authenticator.OAuth;

function CreateCredentialsManager(const CredentialsFile: string; Options: TFetchOptions): TCredentialsManager;
begin
  Result := TCredentialsManager.Create(CredentialsFile, Options.TargetRepository);
end;

{ TCredentialsManager }

constructor TCredentialsManager.Create(const ACredentialsFile, DefaultProfile: string);
begin
  FCredentialsFile := ACredentialsFile;
  FDefaultProfile := DefaultProfile;
end;

destructor TCredentialsManager.Destroy;
begin
  inherited;
end;

class function TCredentialsManager.GetAccessToken(const CredentialsFile: string; Options: TFetchOptions; const AuthUrl: string): string;
begin
  var Manager := TCredentialsManager.Create(CredentialsFile, Options.TargetRepository);
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
    if (Credentials.AccessToken <> '') and (Now < Credentials.Expiration) then
      Exit(Credentials.AccessToken);

    if (Credentials.Email = '') or (Credentials.Code = '') then
      raise Exception.Create('Credentials not provided. Use credentials command to provide your credentials to access repository');

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

procedure TCredentialsManager.LoadCredentials(Credentials: TCredentials; const Profile: string);
begin
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

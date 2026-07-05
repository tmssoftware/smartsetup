unit Auth.Classes;

interface

uses
  System.Generics.Collections, System.JSON, System.SysUtils, System.StrUtils, System.Classes,
  Auth.Utils;

type
  TAuthState = class
  strict private
    FAuthorizeUrl: string;
    FState: string;
    FScope: string;
    FClientId: string;
    FAppState: string;
    FRedirectUri: string;
    FCodeVerifier: string;
    FCodeChallenge: string;
    FCreatedOn: TDateTime;
    FNonce: string;
    FResponseMode: string;
  public
    property AuthorizeUrl: string read FAuthorizeUrl write FAuthorizeUrl;
    property State: string read FState write FState;
    property Scope: string read FScope write FScope;
    property ClientId: string read FClientId write FClientId;
    property AppState: string read FAppState write FAppState;
    property RedirectUri: string read FRedirectUri write FRedirectUri;
    property CodeVerifier: string read FCodeVerifier write FCodeVerifier;
    property CodeChallenge: string read FCodeChallenge write FCodeChallenge;
    property CreatedOn: TDateTime read FCreatedOn write FCreatedOn;
    property Nonce: string read FNonce write FNonce;
    property ResponseMode: string read FResponseMode write FResponseMode;
  end;

  TAuthResultEntry = class
  strict private
    FClientId: string;
    FAccessToken: string;
    FIdToken: string;
    FRefreshToken: string;
    FTokenType: string;
    FExpiresAt: TDateTime;
    FAudience: string;
    FScope: string;
    FAppState: string;
  public
    property ClientId: string read FClientId write FClientId;
    property AccessToken: string read FAccessToken write FAccessToken;
    property IdToken: string read FIdToken write FIdToken;
    property RefreshToken: string read FRefreshToken write FRefreshToken;
    property TokenType: string read FTokenType write FTokenType;
    property Scope: string read FScope write FScope;
    property Audience: string read FAudience write FAudience;
    property ExpiresAt: TDateTime read FExpiresAt write FExpiresAt;
    property AppState: string read FAppState write FAppState;
  end;

  TOidcProfile = class
  strict private
    FSource: TJSONObject;
    FSubject: string;
    FName: string;
    FGivenName: string;
    FFamilyName: string;
    FMiddleName: string;
    FNickName: string;
    FPreferredUserName: string;
    FProfile: string;
    FPicture: string;
    FWebsite: string;
    FEmail: string;
    FEmailVerified: Boolean;
    FGender: string;
    FBirthDate: string;
    FZoneInfo: string;
    FLocale: string;
    FPhoneNumber: string;
    FPhoneNumberVerified: Boolean;
  strict private
    procedure LoadProperties;
  public
    constructor Create(ASource: TJSONObject);
    destructor Destroy; override;
    property Source: TJSONObject read FSource;
    property Subject: string read FSubject write FSubject;
    property Name: string read FName write FName;
    property GivenName: string read FGivenName write FGivenName;
    property FamilyName: string read FFamilyName write FFamilyName;
    property MiddleName: string read FMiddleName write FMiddleName;
    property NickName: string read FNickName write FNickName;
    property PreferredUserName: string read FPreferredUserName write FPreferredUserName;
    property Profile: string read FProfile write FProfile;
    property Picture: string read FPicture write FPicture;
    property Website: string read FWebsite write FWebsite;
    property Email: string read FEmail write FEmail;
    property EmailVerified: Boolean read FEmailVerified write FEmailVerified;
    property Gender: string read FGender write FGender;
    property BirthDate: string read FBirthDate write FBirthDate;
    property ZoneInfo: string read FZoneInfo write FZoneInfo;
    property Locale: string read FLocale write FLocale;
    property PhoneNumber: string read FPhoneNumber write FPhoneNumber;
    property PhoneNumberVerified: Boolean read FPhoneNumberVerified write FPhoneNumberVerified;
  end;

  ITokenResult = interface
  ['{64746A1F-0A3F-488B-A793-854D7A031125}']
    function GetAccessToken: string;
    function GetRefreshToken: string;
    function GetScope: string;
    function GetTokenType: string;
    function GetExpiration: TDateTime;
    function IsExpired: Boolean;

    property AccessToken: string read GetAccessToken;
    property TokenType: string read GetTokenType;
    property RefreshToken: string read GetRefreshToken;
    property Scope: string read GetScope;
    property Expiration: TDateTime read GetExpiration;
  end;

  TAuthResult = class(TInterfacedObject, ITokenResult)
  strict private
    FEntry: TAuthResultEntry;
    FProfile: TOidcProfile;
    function GetAccessToken: string;
    function GetIdToken: string;
    function GetRefreshToken: string;
    function GetScope: string;
    function GetTokenType: string;
    function GetExpiration: TDateTime;
    function GetProfile: TOidcProfile;
    function GetAppState: string;
    property Entry: TAuthResultEntry read FEntry;
  public
    constructor Create(AEntry: TAuthResultEntry);
    destructor Destroy; override;
    function IsExpired: Boolean;
    procedure SetProfile(ASource: TJSONObject);
    property AccessToken: string read GetAccessToken;
    property TokenType: string read GetTokenType;
    property RefreshToken: string read GetRefreshToken;
    property Scope: string read GetScope;
    property Expiration: TDateTime read GetExpiration;
    property IdToken: string read GetIdToken;
    property Profile: TOidcProfile read GetProfile;
    property AppState: string read GetAppState;
  end;

  TClientAuthRequest = class
  private
    FRedirectUri: string;
    FResponseType: string;
    FScope: string;
    FResponseMode: string;
    FState: string;
    FNonce: string;
    FCodeChallenge: string;
    FCodeVerifier: string;
    FPrompt: string;
  private
    function GetIsOpenId: Boolean;
  public
    function GetRequestUrl(const AuthorizationEndpoint, ClientId: string): string;
    property State: string read FState write FState;
    property RedirectUri: string read FRedirectUri write FRedirectUri;
    property ResponseType: string read FResponseType write FResponseType;
    property Scope: string read FScope write FScope;
    property ResponseMode: string read FResponseMode write FResponseMode;
    property Nonce: string read FNonce write FNonce;
    property CodeChallenge: string read FCodeChallenge write FCodeChallenge;
    property CodeVerifier: string read FCodeVerifier write FCodeVerifier;
    property Prompt: string read FPrompt write FPrompt;
    property IsOpenId: Boolean read GetIsOpenId;
  end;

  TOidcProviderMetadata = class
  private
    FIssuer: string;
    FAuthorizationEndpoint: string;
    FTokenEndpoint: string;
    FEndSessionEndpoint: string;
  public
    procedure FromJSONObject(Obj: TJSONObject);
    property Issuer: string read FIssuer write FIssuer;
    property AuthorizationEndpoint: string read FAuthorizationEndpoint write FAuthorizationEndpoint;
    property TokenEndpoint: string read FTokenEndpoint write FTokenEndpoint;
    property EndSessionEndpoint: string read FEndSessionEndpoint write FEndSessionEndpoint;
  end;

  TClientStorage = class
  strict private const
    DefaultStoragePrefix = 'smartsetup.';
  strict private
    FData: TObjectDictionary<string, TObject>;
    FPrefix: string;
    property Prefix: string read FPrefix;
  public
    constructor Create; overload;
    constructor Create(const APrefix: string); overload;
    destructor Destroy; override;
    function Get<T: class>(const Key: string): T;
    procedure Save<T: class>(const Key: string; const Value: T);
    procedure Remove(const Key: string);
  end;

implementation

uses
  Auth.Consts;

{ TOidcProviderMetadata }

procedure TOidcProviderMetadata.FromJSONObject(Obj: TJSONObject);
begin
  Self.Issuer := Obj.GetValue(MetadataProperties.Issuer, '');
  Self.AuthorizationEndpoint := Obj.GetValue(MetadataProperties.AuthorizationEndpoint, '');
  Self.TokenEndpoint := Obj.GetValue(MetadataProperties.TokenEndpoint, '');
  Self.EndSessionEndpoint := Obj.GetValue(MetadataProperties.EndSessionEndpoint, '');
end;

{ TAuthResult }

constructor TAuthResult.Create(AEntry: TAuthResultEntry);
begin
  inherited Create;
  FEntry := AEntry;
end;

destructor TAuthResult.Destroy;
begin
  FProfile.Free;
  FEntry.Free;
  inherited;
end;

function TAuthResult.GetAccessToken: string;
begin
  Result := Entry.AccessToken;
end;

function TAuthResult.GetAppState: string;
begin
  Result := Entry.AppState;
end;

function TAuthResult.GetExpiration: TDateTime;
begin
  Result := Entry.ExpiresAt;
end;

function TAuthResult.GetIdToken: string;
begin
  Result := Entry.IdToken;
end;

function TAuthResult.GetProfile: TOidcProfile;
begin
  if FProfile = nil then
    FProfile := TOidcProfile.Create(GetJwtPayload(IdToken));
  Result := FProfile;
end;

procedure TAuthResult.SetProfile(ASource: TJSONObject);
begin
  FProfile.Free;
  FProfile := TOidcProfile.Create(ASource);
end;

function TAuthResult.GetRefreshToken: string;
begin
  Result := Entry.RefreshToken;
end;

function TAuthResult.GetScope: string;
begin
  Result := Entry.Scope;
end;

function TAuthResult.GetTokenType: string;
begin
  Result := Entry.TokenType;
end;

function TAuthResult.IsExpired: Boolean;
begin
  Result := Now >= Expiration;
end;

{ TClientAuthRequest }

function TClientAuthRequest.GetIsOpenId: Boolean;
begin
  var ScopeItems := SplitString(Trim(Scope), ' ');
  for var Item in ScopeItems do
    if Item = 'openid' then
      Exit(True);
  Result := False;
end;

function TClientAuthRequest.GetRequestUrl(const AuthorizationEndpoint, ClientId: string): string;
var
  Params: TStrings;
begin
  if AuthorizationEndpoint = '' then
    raise Exception.Create('AuthorizationEndpoint required');
  if ResponseType = '' then
    raise Exception.Create('ResponseType required');
  if ClientId = '' then
    raise Exception.Create('ClientId required');
  if State = '' then
    raise Exception.Create('State required');
  if IsOpenId and (Nonce = '') then
    raise Exception.Create('Nonce required for OpenId Connect');

  Params := TStringList.Create;
  try
    Params.Values[AuthorizeRequestParams.ResponseType] := ResponseType;
    Params.Values[AuthorizeRequestParams.ClientId] := ClientId;
    Params.Values[AuthorizeRequestParams.State] := State;

    if RedirectUri <> '' then
      Params.Values[AuthorizeRequestParams.RedirectUri] := RedirectUri;
    if Scope <> '' then
      Params.Values[AuthorizeRequestParams.Scope] := Scope;
    if IsOpenId then
      Params.Values[AuthorizeRequestParams.Nonce] := Nonce;
    if ResponseMode <> '' then
      Params.Values[AuthorizeRequestParams.ResponseMode] := ResponseMode;
    if Prompt <> '' then
      Params.Values[AuthorizeRequestParams.Prompt] := Prompt;
    if CodeChallenge <> '' then
    begin
      Params.Values[AuthorizeRequestParams.CodeChallenge] := CodeChallenge;
      Params.Values[AuthorizeRequestParams.CodeChallengeMethod] := CodeChallengeMethods.Sha256;
    end;
    Result := AddUrlParams(AuthorizationEndpoint, Params);
  finally
    Params.Free;
  end;
end;

{ TOidcProfile }

constructor TOidcProfile.Create(ASource: TJSONObject);
begin
  inherited Create;
  FSource := ASource;
  LoadProperties;
end;

destructor TOidcProfile.Destroy;
begin
  FSource.Free;
  inherited;
end;

procedure TOidcProfile.LoadProperties;
begin
  if Source = nil then Exit;

  Subject := Source.GetValue(JwtClaimNames.Subject, '');
  Email := Source.GetValue(JwtClaimNames.Email, '');
  Name := Source.GetValue(JwtClaimNames.Name, '');
  GivenName := Source.GetValue(JwtClaimNames.GivenName, '');
  FamilyName := Source.GetValue(JwtClaimNames.FamilyName, '');
  MiddleName := Source.GetValue(JwtClaimNames.MiddleName, '');
  NickName := Source.GetValue(JwtClaimNames.NickName, '');
  PreferredUserName := Source.GetValue(JwtClaimNames.PreferredUserName, '');
  Profile := Source.GetValue(JwtClaimNames.Profile, '');
  Picture := Source.GetValue(JwtClaimNames.Picture, '');
  Website := Source.GetValue(JwtClaimNames.Website, '');
  Email := Source.GetValue(JwtClaimNames.Email, '');
  EmailVerified := Source.GetValue(JwtClaimNames.EmailVerified, False);
  Gender := Source.GetValue(JwtClaimNames.Gender, '');
  BirthDate := Source.GetValue(JwtClaimNames.BirthDate, '');
  ZoneInfo := Source.GetValue(JwtClaimNames.ZoneInfo, '');
  Locale := Source.GetValue(JwtClaimNames.Locale, '');
  PhoneNumber := Source.GetValue(JwtClaimNames.PhoneNumber, '');
  PhoneNumberVerified := Source.GetValue(JwtClaimNames.PhoneNumberVerified, False);
end;

{ TClientStorage }

constructor TClientStorage.Create(const APrefix: string);
begin
  inherited Create;
  FPrefix := APrefix;
  FData := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
end;

constructor TClientStorage.Create;
begin
  Create(DefaultStoragePrefix);
end;

destructor TClientStorage.Destroy;
begin
  FData.Free;
  inherited;
end;

function TClientStorage.Get<T>(const Key: string): T;
var
  Value: TObject;
begin
  if FData.TryGetValue(Prefix + Key, Value) then
    Result := T(Value)
  else
    Result := nil;
end;

procedure TClientStorage.Remove(const Key: string);
begin
  FData.Remove(Prefix + Key);
end;

procedure TClientStorage.Save<T>(const Key: string; const Value: T);
begin
  FData.AddOrSetValue(Prefix + Key, Value);
end;

end.

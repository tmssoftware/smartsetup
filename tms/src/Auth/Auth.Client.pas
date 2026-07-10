unit Auth.Client;

{$i ../../tmssetup.inc}

interface

uses
  Generics.Collections, SysUtils, DateUtils, StrUtils, Classes, JSON, NETEncoding, Net.HttpClient, Net.URLClient,
  Auth.Utils, Auth.Classes, Auth.Consts;

type
  EOidcClientException = class(Exception);

  EOidcClientError = class(EOidcClientException)
  strict private
    FError: string;
    FErrorDescription: string;
  public
    constructor Create(const Error, ErrorDescription: string); overload;
    // Raw OAuth error code (e.g. 'invalid_grant'), so callers can react to specific
    // errors without parsing the formatted message.
    property Error: string read FError;
    property ErrorDescription: string read FErrorDescription;
  end;

  TClientAuthResponse = class
  private
    FState: string;
    FError: string;
    FErrorDescription: string;
    FIdToken: string;
    FCode: string;
    FAccessToken: string;
    FTokenType: string;
    FExpiresIn: Integer;
    FExpiration: TDateTime;
    FScope: string;
  public
    property State: string read FState write FState;
    property IdToken: string read FIdToken write FIdToken;
    property Code: string read FCode write FCode;
    property AccessToken: string read FAccessToken write FAccessToken;
    property TokenType: string read FTokenType write FTokenType;
    property ExpiresIn: Integer read FExpiresIn write FExpiresIn;
    property Scope: string read FScope write FScope;
    property Expiration: TDateTime read FExpiration;
    property Error: string read FError write FError;
    property ErrorDescription: string read FErrorDescription write FErrorDescription;
  end;

  TProviderInformation = class
  private
    FIssuer: string;
    FAuthorizationEndpoint: string;
    FTokenEndpoint: string;
    FEndSessionEndpoint: string;
  public
    property Issuer: string read FIssuer write FIssuer;
    property AuthorizationEndpoint: string read FAuthorizationEndpoint write FAuthorizationEndpoint;
    property TokenEndpoint: string read FTokenEndpoint write FTokenEndpoint;
    property EndSessionEndpoint: string read FEndSessionEndpoint write FEndSessionEndpoint;
  end;

  TOidcClient = class
  strict private
    FStorage: TClientStorage;
    FClientId: string;
    FClientSecret: string;
    FScope: string;
    FRedirectUri: string;
    FProviderInfo: TProviderInformation;
    FValidProviderInfo: TProviderInformation;
    FAuthority: string;
    FAutoDiscover: Boolean;
    FSkipIssuerValidation: Boolean;
    FClientSecretInBody: Boolean;
    procedure SetAuthority(const Value: string);
    function AuthStateKey: string;
    function IsCallbackUri(const ResponseUri, ExpectedState: string): Boolean;
    function ResponseFromParams(const Url: string): TClientAuthResponse;
    procedure ValidateIdToken(Entry: TAuthResultEntry; AuthState: TAuthState);
    procedure ValidateResponse(Entry: TAuthResultEntry; Response: TClientAuthResponse; AuthState: TAuthState);
    procedure ProcessCodeResponse(Entry: TAuthResultEntry; Response: TClientAuthResponse; AuthState: TAuthState);
    procedure ValidateIdTokenJwt(Payload: TJSONObject; const Issuer, Audience: string);
    function ProcessAuthResponse(const CallbackUrl: string): TAuthResult;
    function HttpRequestToken(const Params, AuthHeader: string): TJSONObject;
    procedure ReadTokenResponse(TokenResponse: TJSONObject; Entry: TAuthResultEntry; const AScope: string);
    procedure RequestTokenEntry(Entry: TAuthResultEntry; const AScope: string);
    procedure RefreshTokenEntry(Entry: TAuthResultEntry; const ARefreshToken, AScope: string);
    function ExpiresInFromString(const ExpiresInParam: string): Integer;
    procedure CheckProviderInformation;
    procedure MetadataToProvider(Metadata: TOidcProviderMetadata; Info: TProviderInformation);
    function GetJsonProviderMetadata(const Url: string): TJSONObject;
    function RemoveTrailingSlash(const S: string): string;
    function TruncateResponseBody(const Body: string): string;
    function CreateHttpClient: THttpClient;
  strict protected
    property ValidProviderInfo: TProviderInformation read FValidProviderInfo;
  protected
    function GetProviderMetadata: TOidcProviderMetadata;
    procedure DoValidateIssuer(const ExpectedIssuer, TokenIssuer, TenantId: string);
  public
    constructor Create;
    destructor Destroy; override;

    function StartAuthorize(const AppState: string = ''; const APrompt: string = ''): TAuthState;
    function FinishAuthorize(const CallbackUrl: string): TAuthResult; overload;
    function IsValidCallback(const CallbackUrl: string): Boolean; overload;
    function CreateState(Request: TClientAuthRequest; const AppState: string = ''): TAuthState;
    function RequestToken: ITokenResult;
    function BuildLogoutUrl(const IdTokenHint: string = ''; const PostLogoutRedirectUri: string = ''): string;
    function RefreshTokens(const ARefreshToken: string; const AScope: string = ''): TAuthResult;

    property Authority: string read FAuthority write SetAuthority;
    property ClientId: string read FClientId write FClientId;
    property ClientSecret: string read FClientSecret write FClientSecret;
    // Send the client secret as form parameters instead of a Basic Authorization
    // header. Needed by the legacy tms email/code flow, whose "secrets" (registration
    // codes) are not urlencoded the way RFC 6749 requires for the Basic scheme.
    property ClientSecretInBody: Boolean read FClientSecretInBody write FClientSecretInBody;
    property Scope: string read FScope write FScope;
    property RedirectUri: string read FRedirectUri write FRedirectUri;
    property AutoDiscover: Boolean read FAutoDiscover write FAutoDiscover;
    property SkipIssuerValidation: Boolean read FSkipIssuerValidation write FSkipIssuerValidation;
    property ProviderInfo: TProviderInformation read FProviderInfo;
  end;

implementation

const
  // Placeholder found in the issuer of multi-tenant providers (e.g. Microsoft Entra
  // returns "https://login.microsoftonline.com/{tenantid}/v2.0" in its discovery
  // document). It is resolved with the token's tenant id claim before validation.
  IssuerTenantPlaceholder = '{tenantid}';

  SMissingParameter = 'Missing required parameter: %s';
  SInvalidParameter = 'Parameter %s is invalid: %s';
  SAuthStateNotExpected = 'OAuth callback not expected';
  SInvalidCallbackUrl = 'Invalid OAuth callback URL';
  SMissingNonceInState = 'Missing nonce in state';
  SInvalidClaim = 'Claim "%s" is invalid: %s';
  SMissingClaim = 'Missing claim "%s"';
  SIdTokenInvalidPayload = 'Invalid payload in id_token';
  SIdTokenExpired = 'Id token is expired';
  SInvalidState = 'Invalid state: %s';
  SOAuthError = 'OAuth error: %s - %s';
  SOAuthErrorWithoutDescription = 'OAuth error: %s';
  SUnexpectedTokenResponse = 'Unexpected response from token endpoint (HTTP status %d, content type "%s"): %s';
  SEmptyResponseBody = '(empty response body)';

  // Cap the response body included in error messages so an HTML error page or
  // similarly large payload does not flood the message shown to the user/log.
  MaxErrorBodyLength = 2000;

{ TOidcClient }

procedure TOidcClient.CheckProviderInformation;
var
  Metadata: TOidcProviderMetadata;
begin
  if Authority = '' then
    raise EOidcClientException.CreateFmt(SMissingParameter, ['Authority']);
  if ClientId = '' then
    raise EOidcClientException.CreateFmt(SMissingParameter, ['ClientId']);

  if FValidProviderInfo = nil then
  begin
    FValidProviderInfo := TProviderInformation.Create;
    if AutoDiscover then
    begin
      Metadata := GetProviderMetadata;
      try
        if Metadata <> nil then
          MetadataToProvider(Metadata, FValidProviderInfo);
      finally
        Metadata.Free;
      end;
    end;

    // Load custom provider data
    if ProviderInfo <> nil then
    begin
      if FValidProviderInfo.Issuer = '' then
        FValidProviderInfo.Issuer := ProviderInfo.Issuer;
      if FValidProviderInfo.AuthorizationEndpoint = '' then
        FValidProviderInfo.AuthorizationEndpoint := ProviderInfo.AuthorizationEndpoint;
      if FValidProviderInfo.TokenEndpoint = '' then
        FValidProviderInfo.FTokenEndpoint := ProviderInfo.TokenEndpoint;
      if FValidProviderInfo.EndSessionEndpoint = '' then
        FValidProviderInfo.EndSessionEndpoint := ProviderInfo.EndSessionEndpoint;
    end;

    if FValidProviderInfo.Issuer = '' then
      FValidProviderInfo.Issuer := Authority;
  end;

  if FValidProviderInfo.Issuer = '' then
    raise EOidcClientException.CreateFmt(SMissingParameter, ['Issuer']);

  // A templated issuer (e.g. the {tenantid} placeholder used by multi-tenant Microsoft
  // Entra) legitimately differs from the authority, so skip the equality check for it;
  // the per-token issuer is still validated in DoValidateIssuer after resolving the
  // placeholder with the token's tenant id.
  if not FSkipIssuerValidation and not ContainsText(FValidProviderInfo.Issuer, IssuerTenantPlaceholder) then
    if RemoveTrailingSlash(FValidProviderInfo.Issuer) <> RemoveTrailingSlash(Authority) then
      raise EOidcClientException.CreateFmt(SInvalidParameter, ['Issuer', FValidProviderInfo.Issuer]);
end;

constructor TOidcClient.Create;
begin
  inherited Create;
  FStorage := TClientStorage.Create;
  FProviderInfo := TProviderInformation.Create;
  FAutoDiscover := True;
end;

function TOidcClient.CreateHttpClient: THttpClient;
begin
  Result := THttpClient.Create;
end;

function TOidcClient.CreateState(Request: TClientAuthRequest; const AppState: string = ''): TAuthState;
var
  AuthState: TAuthState;
  AuthorizeUrl: string;
begin
  CheckProviderInformation;
  if FValidProviderInfo.AuthorizationEndpoint = '' then
    raise EOidcClientException.CreateFmt(SMissingParameter, ['AuthorizationEndpoint']);
  if Request.ResponseType = ResponseTypes.Code then
    if FValidProviderInfo.TokenEndpoint = '' then
      raise EOidcClientException.CreateFmt(SMissingParameter, ['TokenEndpoint']);

  // Create needed random strings (state, nonce, code verifier...)
  if Request.State = '' then
    Request.State := RandomString;
  if Request.IsOpenId and (Request.Nonce = '') then
    Request.Nonce := RandomString;

  // Hard coded to code flow
  if Request.CodeVerifier = '' then
    Request.CodeVerifier := RandomString(96);
  if Request.CodeChallenge = '' then
    Request.CodeChallenge := Sha256(Request.CodeVerifier);

  AuthorizeUrl := Request.GetRequestUrl(ValidProviderInfo.AuthorizationEndpoint, ClientId);

  // Create state object and save it in storage
  AuthState := TAuthState.Create;
  try
    AuthState.AuthorizeUrl := AuthorizeUrl;
    AuthState.State := Request.State;
    AuthState.Scope := Request.Scope;
    AuthState.ClientId := ClientId;
    AuthState.RedirectUri := Request.RedirectUri;
    AuthState.Nonce := Request.Nonce;
    AuthState.CreatedOn := Now;
    AuthState.ResponseMode := Request.ResponseMode;
    AuthState.CodeVerifier := Request.CodeVerifier;
    AuthState.CodeChallenge := Request.CodeChallenge;
    AuthState.AppState := AppState;
    FStorage.Save<TAuthState>(AuthStateKey, AuthState);
  except
    AuthState.Free;
    raise;
  end;
  Result := AuthState;
end;

destructor TOidcClient.Destroy;
begin
  FStorage.Free;
  FValidProviderInfo.Free;
  FProviderInfo.Free;
  inherited;
end;

function TOidcClient.ExpiresInFromString(const ExpiresInParam: string): Integer;
begin
  if ExpiresInParam = '' then
    Result := 0
  else
    if not TryStrToInt(ExpiresInParam, Result) then
      raise EOidcClientException.CreateFmt(SInvalidParameter, [TokenResponseParams.ExpiresIn, ExpiresInParam]);
end;

function TOidcClient.FinishAuthorize(const CallbackUrl: string): TAuthResult;
begin
  Result := ProcessAuthResponse(CallbackUrl);
end;

function TOidcClient.GetJsonProviderMetadata(const Url: string): TJSONObject;
var
  Client: THttpClient;
  Response: IHttpResponse;
begin
  Result := nil;
  Client := CreateHttpClient;
  try
    Response := Client.Get(Url);
    if Response.StatusCode = 200 then
    begin
      var Json := Response.ContentAsString;
      Result := ParseJsonObject(Json);
      if Result = nil then
        raise Exception.Create('Could not deserialize provider metadata');
    end;
  finally
    Client.Free;
  end;
end;

function TOidcClient.GetProviderMetadata: TOidcProviderMetadata;
var
  Url: string;
  JObject: TJSONObject;
begin
  Url := RemoveTrailingSlash(Authority) + '/.well-known/openid-configuration';
  JObject := GetJsonProviderMetadata(Url);
  try
    Result := TOidcProviderMetadata.Create;
    try
      if JObject <> nil then
        Result.FromJSONObject(JObject);
    except
      Result.Free;
      raise;
    end;
  finally
    JObject.Free;
  end;
end;

function TOidcClient.HttpRequestToken(const Params, AuthHeader: string): TJSONObject;
var
  Client: THttpClient;
  Request: IHttpRequest;
  Response: IHttpResponse;
begin
  CheckProviderInformation;
  Client := CreateHttpClient;
  try
    Request := Client.GetRequest('POST', ValidProviderInfo.TokenEndpoint);
    Request.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    Request.AddHeader('Accept', 'application/json');
    if AuthHeader <> '' then
      Request.AddHeader('Authorization', AuthHeader);
    var Content := TStringStream.Create(Params, TEncoding.UTF8, False);
    try
      Request.SourceStream := Content;
      Response := Client.Execute(Request);

      var ContentType := Response.GetHeaderValue('Content-Type');
      var Body := Response.ContentAsString;
      if SameText(ContentType, 'application/json') or StartsText('application/json;', ContentType) then
      begin
        Result := ParseJsonObject(Body);
        // A JSON content type that does not parse is still a broken response;
        // surface the status and body instead of returning nil.
        if Result = nil then
          raise EOidcClientException.CreateFmt(SUnexpectedTokenResponse,
            [Response.StatusCode, ContentType, TruncateResponseBody(Body)]);
      end
      else
        // The token endpoint answered with something other than JSON (e.g. an
        // HTML error page or a plain-text message from a proxy). Include the
        // status code and body so the actual problem is visible to the user and
        // to support, rather than a generic "could not retrieve token".
        raise EOidcClientException.CreateFmt(SUnexpectedTokenResponse,
          [Response.StatusCode, ContentType, TruncateResponseBody(Body)]);
    finally
      Content.Free;
    end;
  finally
    Client.Free;
  end;
end;

function TOidcClient.StartAuthorize(const AppState: string = ''; const APrompt: string = ''): TAuthState;
var
  Request: TClientAuthRequest;
begin
  Request := TClientAuthRequest.Create;
  try
    Request.RedirectUri := RedirectUri;
    Request.Scope := Scope;
    Request.ResponseType := 'code';
    Request.Prompt := APrompt;
    Result := CreateState(Request, AppState);
  finally
    Request.Free;
  end;
end;

function TOidcClient.IsCallbackUri(const ResponseUri, ExpectedState: string): Boolean;
var
  Params: TStrings;
begin
  Params := ParamsFromUri(ResponseUri);
  try
    if Params.Values[AuthorizeResponseParams.State] = '' then
      Exit(False);
    if (Params.Values[AuthorizeResponseParams.AccessToken] = '')
      and (Params.Values[AuthorizeResponseParams.Error] = '')
      and (Params.Values[AuthorizeResponseParams.IdentityToken] = '')
      and (Params.Values[AuthorizeResponseParams.Code] = '') then
      Exit(False);
    Result := (ExpectedState = '') or (ExpectedState = Params.Values[AuthorizeResponseParams.State]);
  finally
    Params.Free;
  end;
end;

function TOidcClient.IsValidCallback(const CallbackUrl: string): Boolean;
var
  AuthState: TAuthState;
begin
  AuthState := FStorage.Get<TAuthState>(AuthStateKey);
  if AuthState = nil then
    Exit(False);
  Result := IsCallbackUri(CallbackUrl, AuthState.State);
end;

procedure TOidcClient.MetadataToProvider(Metadata: TOidcProviderMetadata; Info: TProviderInformation);
begin
  Info.Issuer := Metadata.Issuer;
  Info.AuthorizationEndpoint := Metadata.AuthorizationEndpoint;
  Info.TokenEndpoint := Metadata.TokenEndpoint;
  Info.EndSessionEndpoint := Metadata.EndSessionEndpoint;
end;

function TOidcClient.BuildLogoutUrl(const IdTokenHint: string = '';
  const PostLogoutRedirectUri: string = ''): string;
var
  Params: string;
begin
  CheckProviderInformation;
  if ValidProviderInfo.EndSessionEndpoint = '' then
    Exit('');

  Params := '';
  if IdTokenHint <> '' then
    Params := Params + '&id_token_hint=' + TNetEncoding.URL.Encode(IdTokenHint);
  if PostLogoutRedirectUri <> '' then
    Params := Params + '&post_logout_redirect_uri=' + TNetEncoding.URL.Encode(PostLogoutRedirectUri);

  if Params = '' then
    Result := ValidProviderInfo.EndSessionEndpoint
  else
    Result := ValidProviderInfo.EndSessionEndpoint + '?' + Copy(Params, 2, MaxInt);
end;

procedure TOidcClient.ProcessCodeResponse(Entry: TAuthResultEntry; Response: TClientAuthResponse; AuthState: TAuthState);
var
  Params: TStrings;
  AuthHeader: string;
  RequestParams: string;
  TokenResponse: TJSONObject;
  Error: string;
  ExpiresIn: Integer;
begin
  if (AuthState.CodeVerifier = '') then
    raise EOidcClientException.Create(Format(SMissingParameter, [TokenRequestParams.CodeVerifier]));

  // build request params
  Params := TStringList.Create;
  try
    Params.Values[TokenRequestParams.GrantType] := GrantTypes.AuthorizationCode;
    Params.Values[TokenRequestParams.Code] := Response.Code;
    Params.Values[TokenRequestParams.RedirectUri] := AuthState.RedirectUri;
    Params.Values[TokenRequestParams.CodeVerifier] := AuthState.CodeVerifier;
    if Self.ClientSecret = '' then // no authentication
    begin
      Params.Values[AuthorizeRequestParams.ClientId] := AuthState.ClientId;
      AuthHeader := '';
    end
    else
      AuthHeader := BasicAuthHeaderValue(AuthState.ClientId, Self.ClientSecret);
    RequestParams := BuildQueryParams(Params);
  finally
    Params.Free;
  end;

  TokenResponse := HttpRequestToken(RequestParams, AuthHeader);
  try
    Error := TokenResponse.GetValue(TokenResponseParams.Error, '');
    if Error <> '' then
      raise EOidcClientError.Create(Error, TokenResponse.GetValue(TokenResponseParams.ErrorDescription, ''));

    Entry.AccessToken := TokenResponse.GetValue(TokenResponseParams.AccessToken, '');
    Entry.RefreshToken := TokenResponse.GetValue(TokenResponseParams.RefreshToken, '');
    Entry.IdToken := TokenResponse.GetValue(TokenResponseParams.IdentityToken, '');
    Entry.TokenType := TokenResponse.GetValue(TokenResponseParams.TokenType, '');
    Entry.Scope := TokenResponse.GetValue(TokenResponseParams.Scope, '');
    if Entry.Scope = '' then
      Entry.Scope := AuthState.Scope;
    ExpiresIn := TokenResponse.GetValue(TokenResponseParams.ExpiresIn, 0);
    if ExpiresIn = 0 then
      Entry.ExpiresAt := MaxDateTime
    else
      Entry.ExpiresAt := IncSecond(Now, ExpiresIn);
  finally
    TokenResponse.Free;
  end;
end;

function TOidcClient.ProcessAuthResponse(const CallbackUrl: string): TAuthResult;
var
  AuthState: TAuthState;
  Response: TClientAuthResponse;
  Entry: TAuthResultEntry;
begin
  CheckProviderInformation;

  // Repeated code from IsValidCallback
  AuthState := FStorage.Get<TAuthState>(AuthStateKey);
  if AuthState = nil then
    raise EOidcClientException.Create(SAuthStateNotExpected);
  if not IsCallbackUri(CallbackUrl, AuthState.State) then
    raise EOidcClientException.Create(SInvalidCallbackUrl);

  try
    // Retrieve the response
    Response := ResponseFromParams(CallbackUrl);
    try
      Entry := TAuthResultEntry.Create;
      try
        ValidateResponse(Entry, Response, AuthState);
      except
        Entry.Free;
        raise;
      end;
    finally
      Response.Free;
    end;

    Result := TAuthResult.Create(Entry);
  finally
    // AuthState finished
    FStorage.Remove(AuthStateKey);
  end;
end;

function TOidcClient.RemoveTrailingSlash(const S: string): string;
begin
  if (S <> '') and (S[Length(S)] = '/') then
    Result := Copy(S, 1, Length(S) - 1)
  else
    Result := S;
end;

function TOidcClient.TruncateResponseBody(const Body: string): string;
begin
  Result := Trim(Body);
  if Result = '' then
    Exit(SEmptyResponseBody);
  if Length(Result) > MaxErrorBodyLength then
    Result := Copy(Result, 1, MaxErrorBodyLength) + '...';
end;

function TOidcClient.RequestToken: ITokenResult;
var
  Entry: TAuthResultEntry;
begin
  Entry := TAuthResultEntry.Create;
  try
    RequestTokenEntry(Entry, Self.Scope);
  except
    Entry.Free;
    raise;
  end;
  Result := TAuthResult.Create(Entry);
end;

procedure TOidcClient.RequestTokenEntry(Entry: TAuthResultEntry; const AScope: string);
var
  Params: TStrings;
  AuthHeader: string;
  RequestParams: string;
  TokenResponse: TJSONObject;
begin
  // build request params
  Params := TStringList.Create;
  try
    Params.Values[TokenRequestParams.GrantType] := GrantTypes.ClientCredentials;
    if AScope <> '' then
      Params.Values[TokenRequestParams.Scope] := AScope;
    if (Self.ClientSecret = '') or ClientSecretInBody then
    begin
      Params.Values[AuthorizeRequestParams.ClientId] := Self.ClientId;
      if Self.ClientSecret <> '' then
        Params.Values[TokenRequestParams.ClientSecret] := Self.ClientSecret;
      AuthHeader := '';
    end
    else
      AuthHeader := BasicAuthHeaderValue(Self.ClientId, Self.ClientSecret);
    RequestParams := BuildQueryParams(Params);
  finally
    Params.Free
  end;

  TokenResponse := HttpRequestToken(RequestParams, AuthHeader);
  try
    ReadTokenResponse(TokenResponse, Entry, AScope);
  finally
    TokenResponse.Free;
  end;
  Entry.ClientId := Self.ClientId;
end;

procedure TOidcClient.ReadTokenResponse(TokenResponse: TJSONObject; Entry: TAuthResultEntry; const AScope: string);
var
  Error: string;
  ExpiresIn: Integer;
begin
  Error := TokenResponse.GetValue(TokenResponseParams.Error, '');
  if Error <> '' then
    raise EOidcClientError.Create(Error, TokenResponse.GetValue(TokenResponseParams.ErrorDescription, ''));

  Entry.AccessToken := TokenResponse.GetValue(TokenResponseParams.AccessToken, '');
  Entry.RefreshToken := TokenResponse.GetValue(TokenResponseParams.RefreshToken, '');
  Entry.IdToken := TokenResponse.GetValue(TokenResponseParams.IdentityToken, '');
  Entry.TokenType := TokenResponse.GetValue(TokenResponseParams.TokenType, '');
  Entry.Scope := TokenResponse.GetValue(TokenResponseParams.Scope, '');
  if Entry.Scope = '' then
    Entry.Scope := AScope;
  ExpiresIn := TokenResponse.GetValue(TokenResponseParams.ExpiresIn, 0);
  if ExpiresIn = 0 then
    Entry.ExpiresAt := MaxDateTime
  else
    Entry.ExpiresAt := IncSecond(Now, ExpiresIn);
end;

procedure TOidcClient.RefreshTokenEntry(Entry: TAuthResultEntry; const ARefreshToken, AScope: string);
var
  Params: TStrings;
  AuthHeader: string;
  RequestParams: string;
begin
  if ARefreshToken = '' then
    raise EOidcClientException.CreateFmt(SMissingParameter, [TokenRequestParams.RefreshToken]);

  Params := TStringList.Create;
  try
    Params.Values[TokenRequestParams.GrantType] := GrantTypes.RefreshToken;
    Params.Values[TokenRequestParams.RefreshToken] := ARefreshToken;
    if AScope <> '' then
      Params.Values[TokenRequestParams.Scope] := AScope;
    if Self.ClientSecret = '' then
    begin
      Params.Values[AuthorizeRequestParams.ClientId] := Self.ClientId;
      AuthHeader := '';
    end
    else
      AuthHeader := BasicAuthHeaderValue(Self.ClientId, Self.ClientSecret);
    RequestParams := BuildQueryParams(Params);
  finally
    Params.Free;
  end;

  var TokenResponse := HttpRequestToken(RequestParams, AuthHeader);
  try
    ReadTokenResponse(TokenResponse, Entry, AScope);
  finally
    TokenResponse.Free;
  end;
  Entry.ClientId := Self.ClientId;
  // Use new refresh token if server issued one (rotation), otherwise keep the original
  if Entry.RefreshToken = '' then
    Entry.RefreshToken := ARefreshToken;
end;

function TOidcClient.RefreshTokens(const ARefreshToken: string; const AScope: string = ''): TAuthResult;
var
  Entry: TAuthResultEntry;
begin
  CheckProviderInformation;
  Entry := TAuthResultEntry.Create;
  try
    RefreshTokenEntry(Entry, ARefreshToken, AScope);
  except
    Entry.Free;
    raise;
  end;
  Result := TAuthResult.Create(Entry);
end;

function TOidcClient.ResponseFromParams(const Url: string): TClientAuthResponse;
var
  Params: TStrings;
begin
  Params := ParamsFromUri(Url);
  try
    Result := TClientAuthResponse.Create;
    try
      Result.State := Params.Values[AuthorizeResponseParams.State];
      Result.IdToken := Params.Values[AuthorizeResponseParams.IdentityToken];
      Result.AccessToken := Params.Values[AuthorizeResponseParams.AccessToken];
      Result.TokenType := Params.Values[AuthorizeResponseParams.TokenType];
      Result.Scope := Params.Values[AuthorizeResponseParams.Scope];
      Result.Code := Params.Values[AuthorizeResponseParams.Code];
      Result.ExpiresIn := ExpiresInFromString(Params.Values[AuthorizeResponseParams.ExpiresIn]);
      Result.Error := Params.Values[AuthorizeResponseParams.Error];
      Result.ErrorDescription := Params.Values[AuthorizeResponseParams.ErrorDescription];
    except
      Result.Free;
      raise;
    end;
  finally
    Params.Free;
  end;
end;

procedure TOidcClient.SetAuthority(const Value: string);
begin
  if FAuthority <> Value then
  begin
    FAuthority := Value;
    FreeAndNil(FValidProviderInfo);
  end;
end;

function TOidcClient.AuthStateKey: string;
begin
  Result := 'st::' + ClientId;
end;

procedure TOidcClient.ValidateIdToken(Entry: TAuthResultEntry; AuthState: TAuthState);
var
  Payload: TJSONObject;
begin
  if AuthState.Nonce = '' then
    raise EOidcClientException.Create(SMissingNonceInState);

  Payload := GetJwtPayload(Entry.IdToken);
  try
    if Payload = nil then
      raise EOidcClientException.Create(SIdTokenInvalidPayload);

    if AuthState.Nonce <> Payload.GetValue(JwtClaimNames.Nonce, '') then
      raise EOidcClientException.CreateFmt(SInvalidClaim, [JwtClaimNames.Nonce, Payload.GetValue(JwtClaimNames.Nonce, '')]);

    ValidateIdTokenJwt(Payload, ValidProviderInfo.Issuer, AuthState.ClientId);
  finally
    Payload.Free;
  end;
end;

procedure TOidcClient.DoValidateIssuer(const ExpectedIssuer, TokenIssuer, TenantId: string);
var
  Valid: Boolean;
  ResolvedIssuer: string;
begin
  // Resolve the {tenantid} placeholder used by multi-tenant providers (Microsoft Entra)
  // with the tenant id carried in the token, so the issuer is validated against the
  // concrete per-tenant value instead of being skipped altogether.
  ResolvedIssuer := ExpectedIssuer;
  if (TenantId <> '') and ContainsText(ResolvedIssuer, IssuerTenantPlaceholder) then
    ResolvedIssuer := ReplaceText(ResolvedIssuer, IssuerTenantPlaceholder, TenantId);

  if not FSkipIssuerValidation then
    Valid := RemoveTrailingSlash(ResolvedIssuer) = RemoveTrailingSlash(TokenIssuer)
  else
    Valid := True;

  if not Valid then
    raise EOidcClientException.CreateFmt(SInvalidClaim, [JwtClaimNames.Issuer, TokenIssuer]);
end;

procedure TOidcClient.ValidateIdTokenJwt(Payload: TJSONObject; const Issuer, Audience: string);
const
  ClockSkew = 5 * 60;  // 5 minutes
var
  LowerNow, UpperNow: NativeInt;
  NowEpoch: NativeInt;
  IssuedAt: NativeInt;
  Expiration: NativeInt;
  NotBefore: NativeInt;
  Found: Boolean;
  AudienceJson: TJSONValue;
  Audiences: TJSONArray;
  I: Integer;
  AudiencesValue: string;
  TenantId: string;
begin
  if Payload.FindValue(JwtClaimNames.Subject) = nil then
    raise EOidcClientException.CreateFmt(SMissingClaim, [JwtClaimNames.Subject]);

  if Payload.FindValue(JwtClaimNames.Issuer) = nil then
    raise EOidcClientException.CreateFmt(SMissingClaim, [JwtClaimNames.Issuer]);

  TenantId := Payload.GetValue(JwtClaimNames.TenantId, '');

  DoValidateIssuer(Issuer, Payload.GetValue<string>(JwtClaimNames.Issuer), TenantId);

  AudienceJSON := Payload.FindValue(JwtClaimNames.Audience);
  if AudienceJSON = nil then
    raise EOidcClientException.CreateFmt(SMissingClaim, [JwtClaimNames.Audience]);

  if AudienceJSON is TJSONString then
  begin
    if Audience <> TJSONString(AudienceJSON).Value then
      raise EOidcClientException.CreateFmt(SInvalidClaim, [JwtClaimNames.Audience, TJSONString(AudienceJSON).Value]);
  end
  else
  if AudienceJSON is TJSONArray then
  begin
    Audiences := AudienceJSON as TJSONArray;
    AudiencesValue := '';
    Found := False;
    for I := 0 to Audiences.Count - 1 do
    begin
      if (Audiences[I] is TJSONString) and (TJSONString(Audiences[I]).Value = Audience) then
      begin
        Found := True;
        Break;
      end;

      if AudiencesValue <> '' then
        AudiencesValue := AudiencesValue + ',';
      AudiencesValue := AudiencesValue + Audiences[I].Value;
    end;
    if not Found then
      raise EOidcClientException.CreateFmt(SInvalidClaim, [JwtClaimNames.Audience, AudiencesValue]);
  end
  else
    raise EOidcClientException.CreateFmt(SMissingClaim, [JwtClaimNames.Audience]);

  if Payload.FindValue(JwtClaimNames.AuthorizedParty) <> nil then
    if Audience <> Payload.GetValue<string>(JwtClaimNames.AuthorizedParty) then
      raise EOidcClientException.CreateFmt(SInvalidClaim, [JwtClaimNames.AuthorizedParty, Payload.GetValue<string>(JwtClaimNames.AuthorizedParty)]);

  // Validate expiration and times
  NowEpoch := DateTimeToUnix(Now, False);
  LowerNow := NowEpoch + ClockSkew;
  UpperNow := NowEpoch - ClockSkew;

  if Payload.FindValue(JwtClaimNames.IssuedAt) = nil then
    raise EOidcClientException.CreateFmt(SMissingClaim, [JwtClaimNames.IssuedAt]);
  IssuedAt := Payload.GetValue<Integer>(JwtClaimNames.IssuedAt);
  if LowerNow < IssuedAt then
    raise EOidcClientException.CreateFmt(SInvalidClaim, [JwtClaimNames.IssuedAt, IntToStr(IssuedAt)]);

  if Payload.FindValue(JwtClaimNames.Expiration) = nil then
    raise EOidcClientException.CreateFmt(SMissingClaim, [JwtClaimNames.Expiration]);
  Expiration := Payload.GetValue<Integer>(JwtClaimNames.Expiration);
  if Expiration < upperNow then
    raise EOidcClientException.Create(SIdTokenExpired);

  if Payload.FindValue(JwtClaimNames.NotBefore) <> nil then
  begin
    NotBefore := Payload.GetValue<Integer>(JwtClaimNames.NotBefore);
    if LowerNow < NotBefore then
      raise EOidcClientException.CreateFmt(SInvalidClaim, [JwtClaimNames.NotBefore, IntToStr(NotBefore)]);
  end;
end;

procedure TOidcClient.ValidateResponse(Entry: TAuthResultEntry; Response: TClientAuthResponse; AuthState: TAuthState);
begin
  if Response.State <> AuthState.State then
    raise EOidcClientException.CreateFmt(SInvalidState, [Response.State]);

  if Response.Error <> '' then
    raise EOidcClientError.Create(Response.Error, Response.ErrorDescription);

  if AuthState.ClientId = '' then
    raise EOidcClientException.Create(Format(SMissingParameter, [AuthorizeRequestParams.ClientId]));

  if AuthState.ClientId <> Self.ClientId then
    raise EOidcClientException.Create(Format(SInvalidParameter, [AuthorizeRequestParams.ClientId, AuthState.ClientId]));

  if Response.Code <> '' then // code flow
    ProcessCodeResponse(Entry, Response, AuthState)
  else
  begin
    if (AuthState.CodeVerifier <> '') then
      raise EOidcClientException.Create(Format(SMissingParameter, [TokenRequestParams.CodeVerifier]));

    Entry.AccessToken := Response.AccessToken;
    Entry.IdToken := Response.IdToken;
    Entry.TokenType := Response.TokenType;
    if Response.Scope <> '' then
      Entry.Scope := Response.Scope
    else
      Entry.Scope := AuthState.Scope;
    if Response.ExpiresIn = 0 then
      Entry.ExpiresAt := MaxDateTime
    else
      Entry.ExpiresAt := IncSecond(Now, Response.ExpiresIn);
  end;
  Entry.ClientId := Self.ClientId;
  Entry.AppState := AuthState.AppState;

  if (AuthState.Nonce <> '') and (Entry.IdToken = '') then
    raise EOidcClientException.Create(Format(SMissingParameter, [AuthorizeResponseParams.IdentityToken]));

  if (AuthState.Nonce = '') and (Entry.IdToken <> '') then
    raise EOidcClientException.Create(Format(SMissingParameter, [AuthorizeRequestParams.Nonce]));

  { Validate tokens }
  if Entry.IdToken <> '' then
    ValidateIdToken(Entry, AuthState);
end;

{ EOidcClientError }

constructor EOidcClientError.Create(const Error, ErrorDescription: string);
begin
  FError := Error;
  FErrorDescription := ErrorDescription;
  if ErrorDescription <> '' then
    inherited CreateFmt(SOAuthError, [Error, ErrorDescription])
  else
    inherited CreateFmt(SOAuthErrorWithoutDescription, [Error]);
end;

end.

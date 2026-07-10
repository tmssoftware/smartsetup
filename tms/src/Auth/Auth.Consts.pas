unit Auth.Consts;

interface

type
  ResponseTypes = class
  public const
    Code = 'code';
  end;

  TokenResponseParams = class
  public const
    AccessToken = 'access_token';
    ExpiresIn = 'expires_in';
    TokenType = 'token_type';
    RefreshToken = 'refresh_token';
    IdentityToken = 'id_token';
    Error = 'error';
    ErrorDescription = 'error_description';
    Scope = 'scope';
  end;

  AuthorizeResponseParams = class
  public const
    Scope = 'scope';
    Code = 'code';
    AccessToken = 'access_token';
    ExpiresIn = 'expires_in';
    TokenType = 'token_type';
    IdentityToken = 'id_token';
    State = 'state';
    Error = 'error';
    ErrorDescription = 'error_description';
  end;

  TokenRequestParams = class
  public const
    GrantType = 'grant_type';
    RedirectUri = 'redirect_uri';
    Code = 'code';
    RefreshToken = 'refresh_token';
    Scope = 'scope';
    CodeVerifier = 'code_verifier';
    ClientSecret = 'client_secret';
  end;

  // OAuth error codes (beyond the RFC 6749 standard ones) that tms.exe reacts to.
  OAuthErrorCodes = class
  public const
    // Sent by the smart setup server when legacy e-mail/code authentication has been
    // turned off. Must match CredentialsAuthDisabledError on the server.
    CredentialsAuthDisabled = 'credentials_auth_disabled';
  end;

  GrantTypes = class
  public const
    AuthorizationCode = 'authorization_code';
    ClientCredentials = 'client_credentials';
    RefreshToken = 'refresh_token';
  end;

  AuthorizeRequestParams = class
  public const
    Scope = 'scope';
    ResponseType = 'response_type';
    ClientId = 'client_id';
    RedirectUri = 'redirect_uri';
    State = 'state';
    ResponseMode = 'response_mode';
    Nonce = 'nonce';
    Prompt = 'prompt';
    CodeChallenge = 'code_challenge';
    CodeChallengeMethod = 'code_challenge_method';
  end;

  JwtClaimNames = class //fi:C104
  public const
    // JWT standard claims (RFC 7519)
    Issuer = 'iss';
    Subject = 'sub';
    Audience = 'aud';
    Expiration = 'exp';
    NotBefore = 'nbf';
    IssuedAt = 'iat';
    JwtId = 'jti';

    // OpenID standard claims (OIDC Core 1.0, section 5.1)
    Name = 'name';
    GivenName = 'given_name';
    FamilyName = 'family_name';
    MiddleName = 'middle_name';
    NickName = 'nickname';
    PreferredUserName = 'preferred_username';
    Profile = 'profile';
    Picture = 'picture';
    WebSite = 'website';
    Email = 'email';
    EmailVerified = 'email_verified';
    Gender = 'gender';
    BirthDate = 'birthdate';
    ZoneInfo = 'zoneinfo';
    Locale = 'locale';
    PhoneNumber = 'phone_number';
    PhoneNumberVerified = 'phone_number_verified';

    // OpenID standard claims (OIDC Core 1.0, section 2)
    Nonce = 'nonce';
    AuthorizedParty = 'azp';

    // Provider-specific claims: Microsoft Entra tenant id, used to resolve the
    // {tenantid} placeholder in the multi-tenant issuer template.
    TenantId = 'tid';
  end;

  CodeChallengeMethods = class
  public const
    Plain = 'plain';
    Sha256 = 'S256';
  end;

  MetadataProperties = class
  public const
    Issuer = 'issuer';
    AuthorizationEndpoint = 'authorization_endpoint';
    TokenEndpoint = 'token_endpoint';
    EndSessionEndpoint = 'end_session_endpoint';
  end;

implementation

end.

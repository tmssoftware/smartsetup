unit Commands.Credentials;

interface

uses
  System.SysUtils, System.IOUtils, UCommandLine, Commands.Logging,
  UCredentials, Commands.GlobalConfig, UConfigFolders;

procedure RegisterCredentialsCommand;

implementation
uses
{$IFDEF MSWINDOWS}
  WinApi.Windows,
{$ENDIF}
  System.JSON, System.DateUtils, UConfigDefinition, Commands.CommonOptions, Commands.Termination,
  UAppTerminated, UMultiLogger, UTmsBuildSystemUtils, UJsonPrinter,
  Auth.Client, Auth.Classes, Auth.Listener;

var
  Print: Boolean = False;
  Check: Boolean = False;
  UseJson: Boolean = False;
  DeleteCreds: Boolean = False;
  NewEmail: string = '';
  NewCode: string = '';
  ServerName: string = 'tms';
  TimeoutSeconds: Integer = 180;


function CredentialsCommandHint(const AServerName: string): string;
begin
  if SameText(AServerName, 'tms') then
    Result := 'tms credentials'
  else
    Result := 'tms credentials -server:' + AServerName;
end;

function ExistingDisplay(const Value: string; ShowOnly: Integer = 0): string;
begin
  if Value = '' then Exit('None');
  if ShowOnly = 0 then
    Result := Value
  else
    Result := '*************' + Copy(Value, Length(Value) - ShowOnly);
end;

{$IFDEF MSWINDOWS}
function ReadPasswordFromConsole: string;
const
  BUF_LEN = 1024;
var
  amt, cmode: cardinal;
  buf: packed array[0..BUF_LEN - 1] of Char;
begin
  GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), cmode);
  SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), cmode and not ENABLE_ECHO_INPUT);
  ReadConsole(GetStdHandle(STD_INPUT_HANDLE), @buf[0], BUF_LEN, amt, nil);
  SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), cmode);
  SetString(Result, PChar(@buf[0]), amt);
  Result := Trim(Result); // remove #13#10, which is added to the end of input
end;
{$ENDIF}

procedure AddCredentials(const Data: TJSONObject; const ServerName: string; const Credentials: TCredentials);
begin
  if Credentials.Email <> '' then
    Data.AddPair('email', Credentials.Email);
  if Credentials.Code <> '' then
    Data.AddPair('code', Credentials.Code);
end;

procedure ReadCredentialsFromConsole(const ServerName: string; Credentials: TCredentials);
begin
  var Value: string;

  // Handle email
  Write(Format(ServerName + ' registration e-mail [%s]: ', [ExistingDisplay(Credentials.Email)]));
  ReadLn(Value);
  if Value <> '' then
    Credentials.Email := Value;

  // Handle code
  Write(Format(ServerName + ' registration code [%s]: ', [ExistingDisplay(Credentials.Code, 2)]));

{$IFDEF MSWINDOWS}
  Value := ReadPasswordFromConsole;
{$ELSE}
  ReadLn(Value);
{$ENDIF}
  if Value <> '' then
    Credentials.Code := Value;
end;

// Classic email/code (client_credentials) servers. Handles -delete, -print and
// setting/updating the stored email and code.
procedure DoUserCodeServer(const Data: TJSONObject; Folders: IBuildFolders; const ServerName, ServerUrl: string);
begin
  var Manager := CreateCredentialsManager(Folders.CredentialsFile(ServerName), FetchOptions, ServerName);
  try
    var Credentials := Manager.ReadCredentials;
    try
      if DeleteCreds then
      begin
        // Clearing email/code deletes the stored credentials (and any cached token).
        Credentials.Email := '';
        Credentials.Code := '';
        Manager.SaveCredentials(Credentials, false);
        if UseJson then
        begin
          var DelData := TJSONObject.Create;
          try
            DelData.AddPair('server', ServerName);
            DelData.AddPair('status', 'removed');
            OutputJson(DelData);
          finally
            DelData.Free;
          end;
        end
        else
          WriteLn(ServerName + ' credentials removed.');
        Exit;
      end;

      if Print then
        AddCredentials(Data, ServerName, Credentials)
      else
      begin
        // if any parameter is passed, we don't ask for any input from the console, and just update the passed parameters
        if (NewEmail <> '') or (NewCode <> '') then
        begin
          if NewEmail <> '' then
            Credentials.Email := NewEmail;
          if NewCode <> '' then
            Credentials.Code := NewCode;
        end
        else
          ReadCredentialsFromConsole(ServerName, Credentials);

        // now update credentials
        begin
          if Check then
            Manager.UpdateAccessToken(Credentials, FetchOptions.RepositoryInfo(ServerUrl).AuthUrl);

          //Only create the folder if we are using TMemIniFile to store it. In windows we are using Credential manager, so no need for it.
          {$IFNDEF MSWINDOWS}
          // Create meta directory here, not inside SaveCredentials. This makes sure that it only works when
          // running credentials command. Otherwise, the meta folder should be created all the time.
          TDirectory_CreateDirectory(TPath.GetDirectoryName(Folders.CredentialsFile(ServerName)));
          {$ENDIF}

          Manager.SaveCredentials(Credentials, false);
        end;
      end;
    finally
      Credentials.Free;
    end;
  finally
    Manager.Free;
  end;

end;

procedure PrintCredentials(const Data: TJsonObject);
begin
  if UseJson then
    OutputJson(Data)
  else
  begin
    for var ServerData in Data do
    begin
      WriteLn(Format('%s %s: %s', [ServerName, ServerData.JsonString.Value, ServerData.JsonValue.Value]));
    end;
    if Data.Count = 0 then
    begin
      var msg := 'No api server is enabled.';
      if ServerName <> '' then msg := 'Server ' + ServerName + ' isn''t defined or is not enabled.';

      raise Exception.Create('There are no credentials to set or show. ' + msg);
    end;

  end;

end;

procedure SaveTokens(const Server: TServerConfig; const Tokens: ITokenResult);
begin
  var Folders: IBuildFolders := ConfigNoCheck.Folders;
  var Manager := CreateCredentialsManager(Folders.CredentialsFile(Server.Name), FetchOptions, Server.Name);
  try
    var Credentials := TCredentials.Create;
    try
      Credentials.AccessToken := Tokens.AccessToken;
      Credentials.RefreshToken := Tokens.RefreshToken;
      Credentials.Expiration := Tokens.Expiration;
      Manager.SaveOidcTokens(Credentials);
      // A successful browser sign-in completes the migration for this user: remove
      // any e-mail/code stored by a previous version so it is no longer grandfathered.
      Manager.ClearLegacyCredentials;
    finally
      Credentials.Free;
    end;
  finally
    Manager.Free;
  end;
end;

// OIDC (authorization code + PKCE) servers: sign in by launching the browser.
procedure OidcSignIn(const Server: TServerConfig);
begin
  // email/code make no sense for browser sign in, and passing them is almost
  // always a script that assumed the old client_credentials flow. Fail clearly
  // instead of silently ignoring them or blocking on a browser that can't be used.
  if (NewEmail <> '') or (NewCode <> '') then
    raise Exception.Create('Server "' + Server.Name + '" uses browser sign in, so it takes no -email/-code. '
      + 'Run "' + CredentialsCommandHint(Server.Name) + '" interactively and sign in through the browser.');

  var Params := TOidcServerParams.Resolve(Server, FetchOptions.RepositoryInfo(Server.Url));

  var Listener := TLoopbackListener.Create;
  try
    Listener.Start;
    var Client := CreateOidcClient(Params, Listener.RedirectUri);
    try
      var AuthState := Client.StartAuthorize;

      // In json mode nothing else can be written to stdout: callers parse the
      // full output as a single JSON value.
      if not UseJson then
        WriteLn('Opening your browser to sign in to the ' + Server.Name + ' server...');
      var BrowserOpened := LaunchFile(AuthState.AuthorizeUrl);
      if not UseJson then
      begin
        if not BrowserOpened then
          WriteLn('Could not open the browser automatically.');
        WriteLn('If the browser did not open, copy this URL into a browser on this machine:');
        WriteLn('  ' + AuthState.AuthorizeUrl);
        WriteLn('Waiting for you to sign in (press Ctrl-C to cancel)...');
      end;

      EnableCtrlCTermination;
      var Callback := Listener.WaitForCallback(TimeoutSeconds,
        function: boolean
        begin
          Result := AppIsTerminated;
        end);

      case Callback.Outcome of
        TLoopbackOutcome.Canceled:
          raise Exception.Create('Sign in was canceled.');
        TLoopbackOutcome.TimedOut:
          raise Exception.Create('oauth2: sign in timed out after ' + IntToStr(TimeoutSeconds)
            + ' seconds. Run "' + CredentialsCommandHint(Server.Name)
            + '" again, and use -timeout to allow more time if needed.');
      end;

      Listener.Stop;

      var Tokens: ITokenResult;
      var Email := '';
      try
        var AuthResult := Client.FinishAuthorize(Callback.CallbackUrl);
        Tokens := AuthResult; // interface reference manages the lifetime
        Email := AuthResult.Profile.Email;
      except
        on E: Exception do
          raise Exception.Create('oauth2: sign in failed. ' + E.Message);
      end;

      SaveTokens(Server, Tokens);

      if (Tokens.RefreshToken = '') and not UseJson then
        Logger.Info('Note: the server did not issue a refresh token. You will need to sign in again when the access token expires.');

      if UseJson then
      begin
        var Data := TJSONObject.Create;
        try
          Data.AddPair('server', Server.Name);
          Data.AddPair('status', 'signed-in');
          if Email <> '' then
            Data.AddPair('email', Email);
          OutputJson(Data);
        finally
          Data.Free;
        end;
      end
      else
      begin
        if Email <> '' then
          WriteLn('Signed in to the ' + Server.Name + ' server as ' + Email + '.')
        else
          WriteLn('Signed in to the ' + Server.Name + ' server.');
      end;
    finally
      Client.Free;
    end;
  finally
    Listener.Free;
  end;
end;

// OIDC servers: -delete removes the locally stored tokens (sign out).
procedure OidcSignOut(const Server: TServerConfig);
begin
  var Folders: IBuildFolders := ConfigNoCheck.Folders;
  var Manager := CreateCredentialsManager(Folders.CredentialsFile(Server.Name), FetchOptions, Server.Name);
  try
    Manager.ClearOidcTokens;
    // Also drop grandfathered e-mail/code credentials: after "-delete" nothing may
    // keep authenticating, and they share the token slot cleared above anyway.
    Manager.ClearLegacyCredentials;
  finally
    Manager.Free;
  end;

  if UseJson then
  begin
    var Data := TJSONObject.Create;
    try
      Data.AddPair('server', Server.Name);
      Data.AddPair('status', 'signed-out');
      OutputJson(Data);
    finally
      Data.Free;
    end;
  end
  else
    WriteLn('Signed out from the ' + Server.Name + ' server.');
end;

// OIDC servers: -print reports whether the user is currently signed in. There is
// no stored email for OIDC servers (only tokens), so only the status is shown.
procedure OidcPrint(const Server: TServerConfig);
begin
  var Folders: IBuildFolders := ConfigNoCheck.Folders;
  var Manager := CreateCredentialsManager(Folders.CredentialsFile(Server.Name), FetchOptions, Server.Name);
  try
    var Credentials := Manager.ReadCredentials;
    try
      // E-mail/code stored by a previous version: still used to authenticate while
      // grandfathered, so surface it (support relies on this to diagnose migrations).
      var HasLegacy := (Credentials.Email <> '') and (Credentials.Code <> '');
      // A bare access token is not proof of a browser session: the grandfathered
      // e-mail/code flow caches its tokens in the same slot. Only a refresh token is
      // exclusive to browser sign-in (or an access token when no legacy creds exist,
      // for servers that issue no refresh token).
      var SignedIn := (Credentials.RefreshToken <> '')
        or ((Credentials.AccessToken <> '') and not HasLegacy);
      if UseJson then
      begin
        var Data := TJSONObject.Create;
        try
          Data.AddPair('server', Server.Name);
          if SignedIn then Data.AddPair('status', 'signed-in')
          else Data.AddPair('status', 'signed-out');
          if SignedIn and (YearOf(Credentials.Expiration) > 1900) then
            Data.AddPair('expiration', DateToISO8601(Credentials.Expiration));
          if HasLegacy then
            Data.AddPair('legacyCredentials', TJSONBool.Create(True));
          OutputJson(Data);
        finally
          Data.Free;
        end;
      end
      else
      begin
        if SignedIn then
          WriteLn(Server.Name + ' status: signed in (browser sign in).')
        else if HasLegacy then
          WriteLn(Server.Name + ' status: using stored e-mail/code credentials (deprecated). Run "'
            + CredentialsCommandHint(Server.Name) + '" to switch to browser sign in.')
        else
          WriteLn(Server.Name + ' status: not signed in. Run "' + CredentialsCommandHint(Server.Name) + '" to sign in.');
      end;
    finally
      Credentials.Free;
    end;
  finally
    Manager.Free;
  end;
end;

procedure RunCredentialsCommand;
begin
  CheckAppAlreadyRunning;

  var Folders: IBuildFolders := ConfigNoCheck.Folders;

  var IsEmpty := true;
  var UserCodeProcessed := false;
  var Data := TJSONObject.Create;
  try
    for var i := 0 to ConfigNoCheck.ServerConfig.ServerCount - 1 do
    begin
      var Server := Config.ServerConfig.GetServer(i);
      if (not Server.Enabled) or (Server.ServerType <> TServerType.Api) then continue;
      if not (ServerName = '') and not SameText(Server.Name, ServerName) then continue;

      IsEmpty := false;

      // TMSSETUP_AUTH_MODE lets support force the command into either mode during
      // the email/code -> browser sign-in migration (e.g. set it to "credentials"
      // to store an email/code on an OIDC server when browser sign-in fails).
      var AuthMode: TServerAuthMode;
      ApplyAuthModeOverride(Server.AuthMode, AuthMode);

      if AuthMode = TServerAuthMode.Oidc then
      begin
        // OIDC servers produce their own output/JSON, so they don't feed the
        // aggregated email/code Data object below.
        if DeleteCreds then
          OidcSignOut(Server)
        else if Print then
          OidcPrint(Server)
        else
          OidcSignIn(Server);
      end
      else
      begin
        DoUserCodeServer(Data, Folders, Server.Name, Server.Url);
        UserCodeProcessed := true;
      end;
    end;

    // If no server matched at all, PrintCredentials raises a helpful error.
    // Otherwise only the classic email/code servers use the aggregated output.
    if IsEmpty then
      PrintCredentials(Data)
    else if Print and UserCodeProcessed then
      PrintCredentials(Data);
  finally
    Data.Free;
  end;
end;

procedure RegisterCredentialsCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('credentials', '', 'set the credentials to access remote repository',
    'This command sets the credentials to access the remote repository. Depending on how the ' + sLineBreak +
    'server is configured, it either asks for your registration e-mail and code, or opens your ' + sLineBreak +
    'browser so you can sign in.' + sLineBreak +
    'More information: https://doc.tmssoftware.com/smartsetup/reference/tms-credentials.html',
    '');

  RegisterRepoOption(cmd);

  var option := cmd.RegisterOption<Boolean>('print', '', 'display current credentials',
    procedure(const Value : Boolean)
    begin
      Print := Value;
    end);
  option.HasValue := False;

  option := cmd.RegisterOption<Boolean>('json', '', 'display the result in JSON format',
    procedure(const Value : Boolean)
    begin
      UseJson := Value;
    end);
  option.HasValue := False;

  option := cmd.RegisterOption<Boolean>('check', '', 'check if credentials are valid before setting',
    procedure(const Value : Boolean)
    begin
      Check := Value;
    end);
  option.HasValue := False;

  option := cmd.RegisterOption<Boolean>('delete', '', 'remove the stored credentials (or sign out, for browser sign in)',
    procedure(const Value : Boolean)
    begin
      DeleteCreds := Value;
    end);
  option.HasValue := False;

  option := cmd.RegisterOption<string>('email', '', 'update credentials with the specified email',
    procedure(const Value: string)
    begin
      NewEmail := Value;
    end);

  option := cmd.RegisterOption<string>('code', '', 'update credentials with the specified code',
    procedure(const Value: string)
    begin
      NewCode := Value;
    end);

  option := cmd.RegisterOption<Integer>('timeout', '', 'for browser sign in, seconds to wait for it to complete (default 180)',
    procedure(const Value: Integer)
    begin
      TimeoutSeconds := Value;
    end);

  option := cmd.RegisterOption<string>('server', '', 'set/get credentials for specific server. If omitted, server "tms" is assumed.',
    procedure(const Value: string)
    begin
      ServerName := Value;
    end);

  AddCommand(cmd.Name, CommandGroups.Config, RunCredentialsCommand);
end;

end.

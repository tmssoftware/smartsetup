unit Commands.Info;

interface

uses
  System.SysUtils, System.IOUtils, UCommandLine;

procedure RegisterInfoCommand;

implementation

uses
  System.JSON, Commands.CommonOptions, Commands.GlobalConfig,
  UConfigDefinition, UConfigFolders, UCredentials, Commands.Logging, UJsonPrinter;

var
  UseJson: Boolean = False;

// Authentication status of the built-in tms server, as reported in the 'auth status'
// json field (consumed by tmsgui to decide its Sign in/Sign out UI):
//   'signed-in'          - browser (OIDC) session
//   'legacy-credentials' - grandfathered e-mail/code from a previous version, still
//                          used to authenticate (see TMSLegacyCredentialsPolicy)
//   'credentials'        - e-mail/code on a server configured for credentials auth
//   'none'               - no way to authenticate; commands that need auth will fail
//   ''                   - tms server disabled or not an api server (field omitted)
function TmsAuthStatus: string;
begin
  Result := '';
  var Folders := ConfigNoCheck.Folders;
  for var i := 0 to Config.ServerConfig.ServerCount - 1 do
  begin
    var Server := Config.ServerConfig.GetServer(i);
    if (not Server.Enabled) or (Server.ServerType <> TServerType.Api) or not SameText(Server.Name, 'tms') then Continue;

    var Manager := CreateCredentialsManager(Folders.CredentialsFile(Server.Name), FetchOptions, Server.Name);
    try
      var Credentials := Manager.ReadCredentials;
      try
        var HasLegacy := (Credentials.Email <> '') and (Credentials.Code <> '');
        var ConfigMode: TServerAuthMode;
        ApplyAuthModeOverride(Server.AuthMode, ConfigMode);
        if ConfigMode = TServerAuthMode.Oidc then
        begin
          // Browser session: a refresh token, or a still-usable access token that was
          // not issued from grandfathered e-mail/code (the legacy flow caches its
          // access token in the same slot).
          if (Credentials.RefreshToken <> '')
            or ((Credentials.AccessToken <> '') and (Now < Credentials.Expiration) and not HasLegacy) then
            Exit('signed-in');
          // Grandfathered e-mail/code count only while they actually authenticate:
          // under TLegacyCredentialsPolicy.Deny, EffectiveAuthMode stops falling
          // back to them and the status becomes 'none', so tmsgui prompts to sign in.
          if HasLegacy and (TCredentialsManager.EffectiveAuthMode(Folders.CredentialsFile(Server.Name),
              FetchOptions, Server) = TServerAuthMode.Credentials) then
            Exit('legacy-credentials');
          Exit('none');
        end;
        if HasLegacy then Exit('credentials');
        Exit('none');
      finally
        Credentials.Free;
      end;
    finally
      Manager.Free;
    end;
  end;
end;

procedure RunInfoCommand;
const
  {$i ../../../Version.inc}
begin
  var Json := TJSONObject.Create;
  try
    Json.AddPair('tms version', TMSVersion);
    Json.AddPair('tms location', ParamStr(0));
    Json.AddPair('working folder', ConfigNoCheck.Folders.RootFolder);
    Json.AddPair('folder initialized', IsValidTMSSetupFolder);
    var AuthStatus := TmsAuthStatus;
    // Deprecated: kept as "can the tms server authenticate at all", which now
    // includes grandfathered e-mail/code credentials on an OIDC server.
    Json.AddPair('has credentials', (AuthStatus <> '') and (AuthStatus <> 'none'));
    if AuthStatus <> '' then
      Json.AddPair('auth status', AuthStatus);
    if TFile.Exists(ConfigFileName) then
      Json.AddPair('config file', ConfigFileName);

    if UseJson then
      OutputJson(Json)
    else
      for var Pair in Json do
        WriteLn(Format('%s: %s', [Pair.JsonString.Value, Pair.JsonValue.Value]));
  finally
    Json.Free;
  end;
end;

procedure RegisterInfoCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('info', '', 'display information about current folder and tms itself',
    'More information: https://doc.tmssoftware.com/smartsetup/reference/tms-info.html',
    'info');

  var option := cmd.RegisterOption<Boolean>('json', '', 'output data in JSON format',
    procedure(const Value: Boolean)
    begin
      UseJson := Value;
    end);
  option.HasValue := False;

  RegisterRepoOption(cmd);

  AddCommand(cmd.Name, CommandGroups.Status, RunInfoCommand);
end;

end.

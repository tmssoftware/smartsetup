unit Commands.Credentials;

interface

uses
  System.SysUtils, System.IOUtils, UCommandLine, Commands.Logging, UCredentials, Commands.GlobalConfig, UConfigFolders;

procedure RegisterCredentialsCommand;

implementation
uses
{$IFDEF MSWINDOWS}
  WinApi.Windows,
{$ENDIF}
  System.JSON, UConfigDefinition, Commands.CommonOptions, UTmsBuildSystemUtils, UJsonPrinter;

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

var
  Print: Boolean = False;
  Check: Boolean = False;
  UseJson: Boolean = False;
  NewEmail: string = '';
  NewCode: string = '';

procedure PrintCredentials(Credentials: TCredentials);
begin
  var Json := TJSONObject.Create;
  try
    if Credentials.Email <> '' then
      Json.AddPair('email', Credentials.Email);
    if Credentials.Code <> '' then
      Json.AddPair('code', Credentials.Code);
    if UseJson then
      OutputJson(Json)
    else
      for var Pair in Json do
        WriteLn(Format('%s: %s', [Pair.JsonString.Value, Pair.JsonValue.Value]));
  finally
    Json.Free;
  end;
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

procedure DoServerCredentials( Folders: IBuildFolders; const ServerName, ServerUrl: string);
begin
  var Manager := CreateCredentialsManager(Folders.CredentialsFile(ServerName), FetchOptions);
  try
    var Credentials := Manager.ReadCredentials;
    try
      if Print then
        PrintCredentials(Credentials)
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

          // Create meta directory here, not inside SaveCredentials. This makes sure that it only works when
          // running credentials command. Otherwise, the meta folder should be created all the time.
          TDirectory_CreateDirectory(TPath.GetDirectoryName(Folders.CredentialsFile(ServerName)));

          Manager.SaveCredentials(Credentials);
        end;
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

  var Folders: IBuildFolders := TBuildFolders.Create(TPath.GetDirectoryName(ConfigFileName));

  if not IsValidTMSSetupFolder then
  begin
    var ServerConfig := TServerConfig.Create('tms', TServerProtocol.Api, '', true);
    DoServerCredentials(Folders, ServerConfig.Name, ServerConfig.Url);
    exit;
  end;

  for var i := 0 to Config.ServerConfig.ServerCount - 1 do
  begin
    var Server := Config.ServerConfig.GetServer(i);
    if (not Server.Enabled) or (Server.Protocol <> TServerProtocol.Api) then continue;
    DoServerCredentials(Folders, Server.Name, Server.Url);
  end;
end;

procedure RegisterCredentialsCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('credentials', '', 'set the credentials to access remote repository',
    'This command asks and updates your credentials to access the remote repository',
    '');

  RegisterRepoOption(cmd);

  var option := cmd.RegisterOption<Boolean>('print', '', 'display current credentials',
    procedure(const Value : Boolean)
    begin
      Print := Value;
    end);
  option.HasValue := False;

  option := cmd.RegisterOption<Boolean>('json', '', 'display credentials in JSON format, if print is specified',
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

  AddCommand(cmd.Name, CommandGroups.Config, RunCredentialsCommand);
end;

end.

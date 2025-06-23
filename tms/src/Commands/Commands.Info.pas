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

function HasCredentials: Boolean;
begin
  Result := false;
  var Folders: IBuildFolders := TBuildFolders.Create(TPath.GetDirectoryName(ConfigFileName));
  for var i := 0 to Config.ServerConfig.ServerCount - 1 do
  begin
    var Server := Config.ServerConfig.GetServer(i);
    if (not Server.Enabled) or (Server.Protocol <> TServerProtocol.Api) then continue;
    var Manager := CreateCredentialsManager(Folders.CredentialsFile(Server.Name), FetchOptions);
    try
      var Credentials := Manager.ReadCredentials;
      try
        if (Credentials.Email <> '') and (Credentials.Code <> '') then exit(true);
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
    Json.AddPair('working folder', TPath.GetDirectoryName(ConfigFileName));
    Json.AddPair('folder initialized', IsValidTMSSetupFolder);
    Json.AddPair('has credentials', HasCredentials);
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
    '',
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

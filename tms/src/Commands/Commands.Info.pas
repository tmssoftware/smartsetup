unit Commands.Info;

interface

uses
  System.SysUtils, System.IOUtils, UCommandLine;

procedure RegisterInfoCommand;

implementation

uses
  System.JSON, Commands.CommonOptions, Commands.GlobalConfig, UConfigFolders, UCredentials, Commands.Logging, UJsonPrinter;

var
  UseJson: Boolean = False;

function HasCredentials: Boolean;
begin
  var Folders: IBuildFolders := TBuildFolders.Create(TPath.GetDirectoryName(ConfigFileName));
  var Manager := CreateCredentialsManager(Folders.CredentialsFile, FetchOptions);
  try
    var Credentials := Manager.ReadCredentials;
    try
      Result := (Credentials.Email <> '') and (Credentials.Code <> '');
    finally
      Credentials.Free;
    end;
  finally
    Manager.Free;
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

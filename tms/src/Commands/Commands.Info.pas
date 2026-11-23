unit Commands.Info;

interface

uses
  System.SysUtils, System.IOUtils, UCommandLine;

procedure RegisterInfoCommand;

implementation

uses
  System.JSON, Commands.CommonOptions, Commands.GlobalConfig,
  UConfigDefinition, UConfigFolders, UCredentials, Commands.Logging, UJsonPrinter, UMultiLogger,
  Deget.CoreTypes, UConfigKeys, UIDEUtils;

// The IDEs we can build for in this machine, using the same check as the build: the IDE is in the registry
// (or the alternate registry key), or there is a compiler path in the config. Paths set only for one product
// don't count. Returns nil if the config can't be loaded, so info still works with a broken config.
function InstalledIDEs: TJSONArray;
begin
  try
    var Cfg := ConfigNoCheck;
    Result := TJSONArray.Create;
    for var dv := Low(TIDEName) to High(TIDEName) do
    begin
      var ErrorMessage: string;
      if IsProductInstalled(dv, GlobalProductId, Cfg, ErrorMessage) then Result.Add(IDEId[dv]);
    end;
  except
    Result := nil;
  end;
end;


function HasCredentials: Boolean;
begin
  try
    Result := false;
    var Folders := ConfigNoCheck.Folders;
    for var i := 0 to Config.ServerConfig.ServerCount - 1 do
    begin
      var Server := Config.ServerConfig.GetServer(i);

      // HasCredentials is deprecated. We will keep the old behavior, which is "HasCredentials" indicates if the
      // server "tms" has its credentials set.
      if (not Server.Enabled) or (Server.ServerType <> TServerType.Api) or not SameText(Server.Name, 'tms') then Continue;
      var Manager := CreateCredentialsManager(Folders.CredentialsFile(Server.Name), FetchOptions, Server.Name);
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
  except
    Result := false; //we assume any error here as "there aren't valid credentials"
  end;
end;

procedure RunInfoCommand;
const
  {$i ../../../Version.inc}
begin
  InitNotFolderBasedCommand;
  var Json := TJSONObject.Create;
  try
    Json.AddPair('tms version', TMSVersion);
    Json.AddPair('tms location', ParamStr(0));
    Json.AddPair('working folder', ConfigNoCheck.Folders.RootFolder);
    Json.AddPair('folder initialized', IsValidTMSSetupFolder);
    Json.AddPair('has credentials', HasCredentials);
    if TFile.Exists(ConfigFileName) then
      Json.AddPair('config file', ConfigFileName);
    var IDEs := InstalledIDEs;
    if IDEs <> nil then Json.AddPair('installed ides', IDEs);

    if Logger.JsonMode then
      OutputJson(Json)
    else
      for var Pair in Json do
      begin
        var Value := Pair.JsonValue.Value;
        if Pair.JsonValue is TJSONArray then
        begin
          var Items: TArray<string> := nil;
          for var Item in TJSONArray(Pair.JsonValue) do Items := Items + [Item.Value];
          Value := string.Join(', ', Items);
        end;
        WriteLn(Format('%s: %s', [Pair.JsonString.Value, Value]));
      end;
  finally
    Json.Free;
  end;
end;

procedure RegisterInfoCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('info', '', 'display information about current folder and tms itself',
    'More information: https://doc.tmssoftware.com/smartsetup/reference/tms-info.html',
    'info');

  RegisterJsonOption(cmd);
  RegisterRepoOption(cmd);

  AddCommand(cmd.Name, CommandGroups.Status, RunInfoCommand);
end;

end.

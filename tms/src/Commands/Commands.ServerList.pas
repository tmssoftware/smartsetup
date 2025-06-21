unit Commands.ServerList;

interface

uses Classes, SysUtils;

procedure RegisterServerListCommand;

implementation
uses UCommandLine, Commands.CommonOptions,
     UConfigWriter, Commands.GlobalConfig, UConfigDefinition,
     System.JSON, UJsonPrinter, Commands.Logging;

var
  EnableLog: Boolean = False;
  UseJson: Boolean = False;

procedure OutputAsJson;
begin
  var Root := TJSONObject.Create;
  try
    for var i := 0 to Config.ServerConfig.ServerCount - 1 do
    begin
      var Item := TJSONObject.Create;
      var Server := Config.ServerConfig.GetServer(i);
      Root.AddPair(Server.Name, Item);

      Item.AddPair('name', Server.Name);
      Item.AddPair('protocol', Server.ProtocolString);
      Item.AddPair('url', Server.Url);
      Item.AddPair('enabled', Server.Enabled);
    end;
    OutputJson(Root);
  finally
    Root.Free;
  end;
end;

procedure OutputAsText;
begin
  for var i := 0 to Config.ServerConfig.ServerCount - 1 do
  begin
    var Server := Config.ServerConfig.GetServer(i);

    var EnabledStr := 'ENABLED';
    if not Server.Enabled then EnabledStr := 'DISABLED';

    if Server.IsReservedName then
    begin
      WriteLn(Format('%s: %s', [Server.Name, EnabledStr]));
    end else
    begin
      WriteLn(Format('%s: %s %s %s', [Server.Name, Server.ProtocolString, Server.Url, EnabledStr]));
    end;
  end;
end;

procedure RunServerListCommand;
begin
  InitFolderBasedCommand(EnableLog);
  if UseJson then
    OutputAsJson
  else
    OutputAsText;
end;

procedure RegisterServerListCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('server-list', '', 'List the servers available to smartsetup.',
    'Provides a list of the servers registered in tms.config.yaml',
    'server-list <name> [-json]');
  cmd.Examples.Add('server-list -json');

  var option := cmd.RegisterOption<Boolean>('log', '', 'enable logging for this command',
    procedure(const Value: Boolean)
    begin
      EnableLog := Value;
    end);
  option.HasValue := False;
  option.Hidden := True;

  option := cmd.RegisterOption<Boolean>('json', '', 'output data in JSON format',
    procedure(const Value: Boolean)
    begin
      UseJson := Value;
    end);
  option.HasValue := False;

  AddCommand(cmd.Name, CommandGroups.Status, RunServerListCommand);
end;


end.

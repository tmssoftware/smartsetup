unit Commands.ServerAdd;

interface

uses Classes, SysUtils;

procedure RegisterServerAddCommand;

implementation
uses UCommandLine, Commands.CommonOptions,
     UConfigWriter, Types, Commands.LocalConfig, UConfigDefinition, Commands.Logging, UConfigSources;

var
  ServerName: string;
  ServerType: TServerType;
  ServerUrl: string;
  ServerEnabled: boolean = true;

procedure RunServerAddCommand;
begin
  InitFolderBasedCommand(false);
  //Reject http:// (and any other non-https/file scheme) at add time.
  //The opt-in for http:// is "allow insecure connections: true" in tms.config.yaml,
  //which can only be set by editing the YAML manually (no CLI flag).
  TServerConfig.ValidateUrlScheme(ServerUrl, ServerType, false, ServerName);

  var Cfg := TSourcedConfig.Create([TConfigSource.Main]);
  try
    var ConfigWriter := TConfigWriter.Create(Cfg.Instance, false);
    try
     Cfg.Instance.EnsureAllProducts;
     if Cfg.Instance.ServerConfig.FindServer(ServerName) >= 0 then raise Exception.Create('Server "' + ServerName + '" is already added in "' + Cfg.FileName + '".');
     Cfg.Instance.ServerConfig.AddServer(TServerConfig.Create(ServerName, ServerType, ServerUrl, ServerEnabled));
     ConfigWriter.Save(Cfg.FileName, Cfg.SingleSource, Cfg.WritingFormat);
    finally
     ConfigWriter.Free;
    end;
  finally
    Cfg.Free;
  end;
end;


procedure RegisterServerAddCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('server-add', '', 'Adds a new server to download libraries.',
    'This command alters the tms.config.yaml file to add a new server. If the server already exists, it will return an error.' + sLineBreak +
    'More information: https://doc.tmssoftware.com/smartsetup/reference/tms-server-add.html',
    'server-add <name> <type> <url> [enable]');
  cmd.Examples.Add('server-add myserver zipfile https://myurl');
  cmd.Examples.Add('server-add myserver zipfile file://c:\myzip.zip');

  var option := cmd.RegisterUnNamedOption<string>('Name of the server to add', 'ServerName',
    procedure(const Value: string)
    begin
      ServerName := Value.Trim;
    end);
  option.Required := True;

  option := cmd.RegisterUnNamedOption<string>('Type of server', 'Type',
    procedure(const Value: string)
    begin
      ServerType := TServerConfig.ServerTypeFromString(Value);
    end);
  option.Required := True;

  option := cmd.RegisterUnNamedOption<string>('Url for the server', 'Url',
    procedure(const Value: string)
    begin
      ServerUrl := Value.Trim;
    end);
  option.Required := True;


  option := cmd.RegisterUnNamedOption<boolean>('Boolean specifying if to enable or disable the server. If ommited, it is assumed true', 'Enable',
    procedure(const Value: boolean)
    begin
      ServerEnabled := Value;
    end);

  RegisterConfigSourceOption(cmd);

  AddCommand(cmd.Name, CommandGroups.Config, RunServerAddCommand);
end;


end.

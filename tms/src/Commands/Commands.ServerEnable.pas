unit Commands.ServerEnable;

interface

uses Classes, SysUtils;

procedure RegisterServerEnableCommand;

implementation
uses UCommandLine, Commands.CommonOptions,
     UConfigWriter, Types, Commands.LocalConfig, UConfigDefinition, Commands.Logging, UConfigSources;

var
  ServerName: string;
  ServerEnabled: boolean = true;

procedure RunServerEnableCommand;
begin
  InitFolderBasedCommand(false);
  var Cfg := TSourcedConfig.Create([TConfigSource.Main]);
  try
    var ConfigWriter := TConfigWriter.Create(Cfg.Instance, false);
    try
     Cfg.Instance.EnsureAllProducts;
     var ServerIndex := Cfg.Instance.ServerConfig.FindServer(ServerName);
     if ServerIndex < 0 then
     begin
       raise Exception.Create('Cannot find the server "' + ServerName + '" in "' + Cfg.FileName + '".');
     end;
     Cfg.Instance.ServerConfig.SetInfo(ServerIndex, procedure(var Server: TServerConfig) begin Server.Enabled := ServerEnabled; end);
     ConfigWriter.Save(Cfg.FileName, Cfg.SingleSource, Cfg.WritingFormat);
    finally
     ConfigWriter.Free;
    end;
  finally
    Cfg.Free;
  end;
end;


procedure RegisterServerEnableCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('server-enable', '', 'Enables or disables an existing server to download libraries.',
    'This command alters the tms.config.yaml file to enable/disable the given server, if it exists.' + sLineBreak +
    'More information: https://doc.tmssoftware.com/smartsetup/reference/tms-server-enable.html',
    'server-enable <name> [enable]');
  cmd.Examples.Add('server-enable community');
  cmd.Examples.Add('server-enable community false');

  var option := cmd.RegisterUnNamedOption<string>('Name of the server to enable', 'ServerName',
    procedure(const Value: string)
    begin
      ServerName := Value.Trim;
    end);
  option.Required := True;

  option := cmd.RegisterUnNamedOption<boolean>('Boolean specifying if to enable or disable the server. If ommited, it is assumed true', 'Enable',
    procedure(const Value: boolean)
    begin
      ServerEnabled := Value;
    end);

  RegisterConfigSourceOption(cmd);

  AddCommand(cmd.Name, CommandGroups.Config, RunServerEnableCommand);
end;


end.

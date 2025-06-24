unit Commands.ServerEnable;

interface

uses Classes, SysUtils;

procedure RegisterServerEnableCommand;

implementation
uses UCommandLine, Commands.CommonOptions,
     UConfigWriter, Types, Commands.GlobalConfig, UConfigDefinition;

var
  ServerName: string;
  ServerEnabled: boolean = true;

procedure RunServerEnableCommand;
begin
  var StandardConfigFileGlobal := TResourceStream.Create(HInstance, 'StandardConfigFileGlobal', RT_RCDATA);
  try
    var StandardConfigFileProduct := TResourceStream.Create(HInstance, 'StandardConfigFileProduct', RT_RCDATA);
    try
       var ConfigWriter := TConfigWriter.Create(false);
       try
         Config.EnsureAllProducts;
         Config.ServerConfig.EnsureAllBuiltInServers;
         var ServerIndex := Config.ServerConfig.FindServer(ServerName);
         if ServerIndex < 0 then
         begin
           raise Exception.Create('Cannot find the server "' + ServerName + '" in tms.config.yaml.');
         end;
         Config.ServerConfig.SetInfo(ServerIndex, procedure(var Server: TServerConfig) begin Server.Enabled := ServerEnabled; end);
         ConfigWriter.Save(Config, StandardConfigFileGlobal, StandardConfigFileProduct, ConfigFileName);
       finally
         ConfigWriter.Free;
       end;
    finally
      StandardConfigFileProduct.Free;
    end;
  finally
    StandardConfigFileGlobal.Free;
  end;
end;


procedure RegisterServerEnableCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('server-enable', '', 'Enables or disables an existing server to download libraries.',
    'This command alters the tms.config.yaml file to enable/disable the given server, if it exists.',
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


  AddCommand(cmd.Name, CommandGroups.Config, RunServerEnableCommand);
end;


end.

unit Commands.ServerAdd;

interface

uses Classes, SysUtils;

procedure RegisterServerAddCommand;

implementation
uses UCommandLine, Commands.CommonOptions,
     UConfigWriter, Types, Commands.GlobalConfig, UConfigDefinition;

var
  ServerName: string;
  ServerType: TServerType;
  ServerUrl: string;
  ServerEnabled: boolean = true;

procedure RunServerAddCommand;
begin
  var StandardConfigFileGlobal := TResourceStream.Create(HInstance, 'StandardConfigFileGlobal', RT_RCDATA);
  try
    var StandardConfigFileProduct := TResourceStream.Create(HInstance, 'StandardConfigFileProduct', RT_RCDATA);
    try
       var ConfigWriter := TConfigWriter.Create(false);
       try
         Config.EnsureAllProducts;
         if Config.ServerConfig.FindServer(ServerName) >= 0 then raise Exception.Create('Server "' + ServerName + '" is already added in tms.config.yaml.');
         Config.ServerConfig.AddServer(TServerConfig.Create(ServerName, ServerType, ServerUrl, ServerEnabled));
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


procedure RegisterServerAddCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('server-add', '', 'Adds a new server to download libraries.',
    'This command alters the tms.config.yaml file to add a new server. If the server already exists, it will return an error.',
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


  AddCommand(cmd.Name, CommandGroups.Config, RunServerAddCommand);
end;


end.

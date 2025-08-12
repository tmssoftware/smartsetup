unit Commands.ServerRemove;

interface

uses Classes, SysUtils;

procedure RegisterServerRemoveCommand;

implementation
uses UCommandLine, Commands.CommonOptions,
     UConfigWriter, Types, Commands.GlobalConfig, UConfigDefinition;

var
  ServerName: string;

procedure RunServerRemoveCommand;
begin
  var StandardConfigFileGlobal := TResourceStream.Create(HInstance, 'StandardConfigFileGlobal', RT_RCDATA);
  try
    var StandardConfigFileProduct := TResourceStream.Create(HInstance, 'StandardConfigFileProduct', RT_RCDATA);
    try
       var ConfigWriter := TConfigWriter.Create(false);
       try
         Config.EnsureAllProducts;
         var ServerIndex := Config.ServerConfig.FindServer(ServerName);
         if ServerIndex < 0 then raise Exception.Create('Server "' + ServerName + '" is not defined in tms.config.yaml.');
         Config.ServerConfig.RemoveServer(ServerIndex);
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


procedure RegisterServerRemoveCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('server-remove', '', 'Removes an existing server from the configuration.',
    'This command alters the tms.config.yaml file to remove an exsting server. If the server doesn''t exist, it will return an error.',
    'server-remove <name>');
  cmd.Examples.Add('server-remove myserver');

  var option := cmd.RegisterUnNamedOption<string>('Name of the server to remove', 'ServerName',
    procedure(const Value: string)
    begin
      ServerName := Value.Trim;
    end);
  option.Required := True;


  AddCommand(cmd.Name, CommandGroups.Config, RunServerRemoveCommand);
end;


end.

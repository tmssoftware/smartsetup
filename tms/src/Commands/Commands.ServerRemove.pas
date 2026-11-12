unit Commands.ServerRemove;

interface

uses Classes, SysUtils;

procedure RegisterServerRemoveCommand;

implementation
uses UCommandLine, Commands.CommonOptions,
     UConfigWriter, Types, Commands.LocalConfig, UConfigDefinition, Commands.Logging, UConfigSources;

var
  ServerName: string;

procedure RunServerRemoveCommand;
begin
  InitFolderBasedCommand(false);
  var Cfg := TSourcedConfig.Create([TConfigSource.Main]);
  try
    var ConfigWriter := TConfigWriter.Create(Cfg.Instance, false);
    try
     Cfg.Instance.EnsureAllProducts;
     var ServerIndex := Cfg.Instance.ServerConfig.FindServer(ServerName);
     if ServerIndex < 0 then raise Exception.Create('Server "' + ServerName + '" is not defined in "' + Cfg.FileName + '".');
     Cfg.Instance.ServerConfig.RemoveServer(ServerIndex);
     ConfigWriter.Save(Cfg.FileName, Cfg.SingleSource, Cfg.WritingFormat);
    finally
     ConfigWriter.Free;
    end;
  finally
    Cfg.Free;
  end;
end;


procedure RegisterServerRemoveCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('server-remove', '', 'Removes an existing server from the configuration.',
    'This command alters the tms.config.yaml file to remove an exsting server. If the server doesn''t exist, it will return an error.' + sLineBreak +
    'More information: https://doc.tmssoftware.com/smartsetup/reference/tms-server-remove.html',
    'server-remove <name>');
  cmd.Examples.Add('server-remove myserver');

  var option := cmd.RegisterUnNamedOption<string>('Name of the server to remove', 'ServerName',
    procedure(const Value: string)
    begin
      ServerName := Value.Trim;
    end);
  option.Required := True;

  RegisterConfigSourceOption(cmd);

  AddCommand(cmd.Name, CommandGroups.Config, RunServerRemoveCommand);
end;


end.

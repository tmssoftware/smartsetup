unit Commands.ConfigWrite;

interface

uses Classes, SysUtils;

procedure RegisterConfigWriteCommand;

implementation
uses UCommandLine, Commands.CommonOptions,
     UConfigWriter, Types, Commands.GlobalConfig;

procedure RunConfigWriteCommand;
begin
  var StandardConfigFileGlobal := TResourceStream.Create(HInstance, 'StandardConfigFileGlobal', RT_RCDATA);
  try
    var StandardConfigFileProduct := TResourceStream.Create(HInstance, 'StandardConfigFileProduct', RT_RCDATA);
    try
       var ConfigWriter := TConfigWriter.Create(false);
       try
         Config.EnsureAllProducts;
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


procedure RegisterConfigWriteCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('config-write', '', 'Saves a configuration to smart-setup configuration file.',
    'Use the -p parameter to change the properties before saving them. IMPORTANT: This command will reformat the config file and remove any manually entered comments.',
    'config-write [<options>]');
  cmd.Examples.Add('config-write -p:configuration-for-all-products:delphi-versions=[delphixe,delphi12] -p:configuration-for-all-products:platforms=[win32intel,win64intel]');
  cmd.Examples.Add('config-write -p:tms-smart-setup-options:git:git-location=c:\git.exe');

  AddCommand(cmd.Name, CommandGroups.Config, RunConfigWriteCommand);
end;


end.

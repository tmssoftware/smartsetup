unit UInstallerRegistry;
{$i ../../tmssetup.inc}

interface
implementation
uses UInstaller
    ,Deget.CoreTypes
    ,SysUtils
    ,UDelphi6Installer
    ,UDelphi7Installer
    ,UDelphi2005Installer
    ,UDelphi2006Installer
    ,UDelphi2007Installer
    ,UDelphi2009Installer
    ,UDelphi2010Installer
    ,UDelphiXEInstaller
    ,UDelphiXE2Installer
    ,UDelphiXE3Installer
    ,UDelphiXE4Installer
    ,UDelphiXE5Installer
    ,UDelphiXE6Installer
    ,UDelphiXE7Installer
    ,UDelphiXE8Installer
    ,UDelphiSeattleInstaller
    ,UDelphiBerlinInstaller
    ,UDelphiTokyoInstaller
    ,UDelphiRioInstaller
    ,UDelphiSydneyInstaller
    ,UDelphi11Installer
    ,ULazarusInstaller
    ,UDelphi12Installer
    ,UDelphi13Installer
;

initialization
 TInstallerFactory.RegisterInstaller(TLazarusInstaller.Create);
 TInstallerFactory.RegisterInstaller(TDelphi6Installer.Create);
 TInstallerFactory.RegisterInstaller(TDelphi7Installer.Create);
 TInstallerFactory.RegisterInstaller(TDelphi2005Installer.Create);
 TInstallerFactory.RegisterInstaller(TDelphi2006Installer.Create);
 TInstallerFactory.RegisterInstaller(TDelphi2007Installer.Create);
 TInstallerFactory.RegisterInstaller(TDelphi2009Installer.Create);
 TInstallerFactory.RegisterInstaller(TDelphi2010Installer.Create);
 TInstallerFactory.RegisterInstaller(TDelphiXEInstaller.Create);
 TInstallerFactory.RegisterInstaller(TDelphiXE2Installer.Create);
 TInstallerFactory.RegisterInstaller(TDelphiXE3Installer.Create);
 TInstallerFactory.RegisterInstaller(TDelphiXE4Installer.Create);
 TInstallerFactory.RegisterInstaller(TDelphiXE5Installer.Create);
 TInstallerFactory.RegisterInstaller(TDelphiXE6Installer.Create);
 TInstallerFactory.RegisterInstaller(TDelphiXE7Installer.Create);
 TInstallerFactory.RegisterInstaller(TDelphiXE8Installer.Create);
 TInstallerFactory.RegisterInstaller(TDelphiSeattleInstaller.Create);
 TInstallerFactory.RegisterInstaller(TDelphiBerlinInstaller.Create);
 TInstallerFactory.RegisterInstaller(TDelphiTokyoInstaller.Create);
 TInstallerFactory.RegisterInstaller(TDelphiRioInstaller.Create);
 TInstallerFactory.RegisterInstaller(TDelphiSydneyInstaller.Create);
 TInstallerFactory.RegisterInstaller(TDelphi11Installer.Create);
 TInstallerFactory.RegisterInstaller(TDelphi12Installer.Create);
 TInstallerFactory.RegisterInstaller(TDelphi13Installer.Create);

for var dv := Low(TIDEName) to High(TIDEName) do
begin
 if TInstallerFactory.GetInstaller(dv) = nil then raise Exception.Create('The installer for ' + IDEId[dv]  + ' isn''t registered. Please modify the file UInstallerRegistry.pas');
end;
end.

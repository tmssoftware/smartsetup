unit UDelphiXE4Installer;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UMsBuildInstaller;

type
  TDelphiXE4Installer = class(TMsBuildInstaller)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
    function SupportsCppBuilder(const platform: TPlatform): boolean; override;
  end;
implementation

{ TDelphiXE4Installer }

function TDelphiXE4Installer.IDEName: TIDEName;
begin
  Result := TIDEName.delphixe4;
end;

function TDelphiXE4Installer.DisplayName: string;
begin
  Result := 'Delphi XE4';
end;

function TDelphiXE4Installer.DllSuffix: string;
begin
  Result := '180';
end;

function TDelphiXE4Installer.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel, TPlatform.win64intel, TPlatform.macos32intel, TPlatform.iossimulator, TPlatform.iosdevice32];
end;

function TDelphiXE4Installer.SupportsCppBuilder(
  const platform: TPlatform): boolean;
begin
  Result := platform in [TPlatform.win32intel, TPlatform.win64intel];
end;

end.

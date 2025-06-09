unit UDelphiXE6Installer;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UMsBuildInstaller;

type
  TDelphiXE6Installer = class(TMsBuildInstaller)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
    function SupportsCppBuilder(const platform: TPlatform): boolean; override;

  end;
implementation

{ TDelphiXE6Installer }

function TDelphiXE6Installer.IDEName: TIDEName;
begin
  Result := TIDEName.delphixe6;
end;

function TDelphiXE6Installer.DisplayName: string;
begin
  Result := 'Delphi XE6';
end;

function TDelphiXE6Installer.DllSuffix: string;
begin
  Result := '200';
end;

function TDelphiXE6Installer.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel, TPlatform.win64intel, TPlatform.macos32intel, TPlatform.iossimulator, TPlatform.iosdevice32, TPlatform.android32];
end;

function TDelphiXE6Installer.SupportsCppBuilder(
  const platform: TPlatform): boolean;
begin
  Result := platform in [TPlatform.win32intel, TPlatform.win64intel, TPlatform.android32];
end;

end.

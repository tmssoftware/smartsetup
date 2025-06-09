unit UDelphiXE7Installer;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UMsBuildInstaller;

type
  TDelphiXE7Installer = class(TMsBuildInstaller)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
    function SupportsCppBuilder(const platform: TPlatform): boolean; override;

  end;
implementation

{ TDelphiXE7Installer }

function TDelphiXE7Installer.IDEName: TIDEName;
begin
  Result := TIDEName.delphixe7;
end;

function TDelphiXE7Installer.DisplayName: string;
begin
  Result := 'Delphi XE7';
end;

function TDelphiXE7Installer.DllSuffix: string;
begin
  Result := '210';
end;

function TDelphiXE7Installer.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel, TPlatform.win64intel, TPlatform.macos32intel, TPlatform.iossimulator, TPlatform.iosdevice32, TPlatform.android32];
end;

function TDelphiXE7Installer.SupportsCppBuilder(
  const platform: TPlatform): boolean;
begin
  Result := platform in [TPlatform.win32intel, TPlatform.win64intel, TPlatform.android32]; //we'll ignore ios32
end;

end.

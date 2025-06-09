unit UDelphiXE8Installer;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UMsBuildInstaller;

type
  TDelphiXE8Installer = class(TMsBuildInstaller)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
    function SupportsCppBuilder(const platform: TPlatform): boolean; override;
  end;
implementation

{ TDelphiXE8Installer }

function TDelphiXE8Installer.IDEName: TIDEName;
begin
  Result := TIDEName.delphixe8;
end;

function TDelphiXE8Installer.DisplayName: string;
begin
  Result := 'Delphi XE8';
end;

function TDelphiXE8Installer.DllSuffix: string;
begin
  Result := '220';
end;

function TDelphiXE8Installer.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel, TPlatform.win64intel, TPlatform.macos32intel, TPlatform.iossimulator, TPlatform.iosdevice32, TPlatform.iosdevice64, TPlatform.android32];
end;

function TDelphiXE8Installer.SupportsCppBuilder(
  const platform: TPlatform): boolean;
begin
  Result := platform in [TPlatform.win32intel, TPlatform.win64intel, TPlatform.android32]; //we'll ignore ios32
end;

end.

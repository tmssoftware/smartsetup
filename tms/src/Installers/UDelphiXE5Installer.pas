unit UDelphiXE5Installer;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UMsBuildInstaller;

type
  TDelphiXE5Installer = class(TMsBuildInstaller)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
    function SupportsCppBuilder(const platform: TPlatform): boolean; override;
  end;
implementation

{ TDelphiXE5Installer }

function TDelphiXE5Installer.IDEName: TIDEName;
begin
  Result := TIDEName.delphixe5;
end;

function TDelphiXE5Installer.DisplayName: string;
begin
  Result := 'Delphi XE5';
end;

function TDelphiXE5Installer.DllSuffix: string;
begin
  Result := '190';
end;

function TDelphiXE5Installer.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel, TPlatform.win64intel, TPlatform.macos32intel, TPlatform.iossimulator, TPlatform.iosdevice32, TPlatform.android32];
end;

function TDelphiXE5Installer.SupportsCppBuilder(
  const platform: TPlatform): boolean;
begin
  Result := platform in [TPlatform.win32intel, TPlatform.win64intel];
end;

end.

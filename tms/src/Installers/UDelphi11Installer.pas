unit UDelphi11Installer;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UMsBuildInstaller;

type
  TDelphi11Installer = class(TMsBuildInstaller)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
    function SupportsCppBuilder(const platform: TPlatform): boolean; override;
  end;
implementation

{ TDelphi11Installer }

function TDelphi11Installer.IDEName: TIDEName;
begin
  Result := TIDEName.delphi11;
end;

function TDelphi11Installer.DisplayName: string;
begin
  Result := 'Delphi 11';
end;

function TDelphi11Installer.DllSuffix: string;
begin
  Result := '280';
end;

function TDelphi11Installer.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel, TPlatform.win64intel, TPlatform.macos64intel,
             TPlatform.macos64arm, TPlatform.iosdevice64, TPlatform.android32, TPlatform.android64, TPlatform.linux64];
end;
function TDelphi11Installer.SupportsCppBuilder(
  const platform: TPlatform): boolean;
begin
  Result := platform in [TPlatform.win32intel, TPlatform.win64intel, TPlatform.android32, TPlatform.iosdevice64];
end;

end.

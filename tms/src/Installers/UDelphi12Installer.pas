unit UDelphi12Installer;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UMsBuildInstaller;

type
  TDelphi12Installer = class(TMsBuildInstaller)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
    function SupportsCppBuilder(const platform: TPlatform): boolean; override;
  end;
implementation

{ TDelphi12Installer }

function TDelphi12Installer.IDEName: TIDEName;
begin
  Result := TIDEName.delphi12;
end;

function TDelphi12Installer.DisplayName: string;
begin
  Result := 'Delphi 12';
end;

function TDelphi12Installer.DllSuffix: string;
begin
  Result := '290';
end;

function TDelphi12Installer.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel, TPlatform.win64intel, TPlatform.macos64intel,
             TPlatform.macos64arm, TPlatform.iosdevice64, TPlatform.android32,
             TPlatform.android64, TPlatform.linux64, TPlatform.iossimulator64arm, TPlatform.win64Xintel];
end;

function TDelphi12Installer.SupportsCppBuilder(
  const platform: TPlatform): boolean;
begin
  Result := platform in [TPlatform.win32intel, TPlatform.win64intel, TPlatform.android32, TPlatform.iosdevice64, win64Xintel];
end;

end.

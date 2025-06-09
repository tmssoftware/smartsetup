unit UDelphiTokyoInstaller;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UMsBuildInstaller;

type
  TDelphiTokyoInstaller = class(TMsBuildInstaller)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
    function SupportsCppBuilder(const platform: TPlatform): boolean; override;
  end;
implementation

{ TDelphiTokyoInstaller }

function TDelphiTokyoInstaller.IDEName: TIDEName;
begin
  Result := TIDEName.delphitokyo;
end;

function TDelphiTokyoInstaller.DisplayName: string;
begin
  Result := 'Delphi 10.2 Tokyo';
end;

function TDelphiTokyoInstaller.DllSuffix: string;
begin
  Result := '250';
end;

function TDelphiTokyoInstaller.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel, TPlatform.win64intel, TPlatform.macos32intel, TPlatform.iossimulator, TPlatform.iosdevice32, TPlatform.iosdevice64, TPlatform.android32, TPlatform.linux64];
end;
function TDelphiTokyoInstaller.SupportsCppBuilder(
  const platform: TPlatform): boolean;
begin
  Result := platform in [TPlatform.win32intel, TPlatform.win64intel, TPlatform.android32]; //we'll ignore ios32
end;

end.

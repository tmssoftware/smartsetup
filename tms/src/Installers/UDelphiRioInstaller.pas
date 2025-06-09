unit UDelphiRioInstaller;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UMsBuildInstaller;

type
  TDelphiRioInstaller = class(TMsBuildInstaller)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
    function SupportsCppBuilder(const platform: TPlatform): boolean; override;

  end;
implementation

{ TDelphiRioInstaller }

function TDelphiRioInstaller.IDEName: TIDEName;
begin
  Result := TIDEName.delphirio;
end;

function TDelphiRioInstaller.DisplayName: string;
begin
  Result := 'Delphi 10.3 Rio';
end;

function TDelphiRioInstaller.DllSuffix: string;
begin
  Result := '260';
end;

function TDelphiRioInstaller.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel, TPlatform.win64intel, TPlatform.macos32intel, TPlatform.macos64intel, TPlatform.iossimulator, TPlatform.iosdevice32, TPlatform.iosdevice64, TPlatform.android32, TPlatform.android64, TPlatform.linux64];
end;
function TDelphiRioInstaller.SupportsCppBuilder(
  const platform: TPlatform): boolean;
begin
  Result := platform in [TPlatform.win32intel, TPlatform.win64intel, TPlatform.android32]; //we'll ignore ios32
end;

end.

unit UDelphiSydneyInstaller;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UMsBuildInstaller;

type
  TDelphiSydneyInstaller = class(TMsBuildInstaller)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
    function SupportsCppBuilder(const platform: TPlatform): boolean; override;
  end;
implementation

{ TDelphiSydneyInstaller }

function TDelphiSydneyInstaller.IDEName: TIDEName;
begin
  Result := TIDEName.delphisydney;
end;

function TDelphiSydneyInstaller.DisplayName: string;
begin
  Result := 'Delphi 10.4 Sydney';
end;

function TDelphiSydneyInstaller.DllSuffix: string;
begin
  Result := '270';
end;

function TDelphiSydneyInstaller.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel, TPlatform.win64intel, TPlatform.macos64intel, TPlatform.iossimulator, TPlatform.iosdevice64, TPlatform.android32, TPlatform.android64, TPlatform.linux64];
end;
function TDelphiSydneyInstaller.SupportsCppBuilder(
  const platform: TPlatform): boolean;
begin
  Result := platform in [TPlatform.win32intel, TPlatform.win64intel, TPlatform.android32]; //we'll ignore ios32
end;

end.

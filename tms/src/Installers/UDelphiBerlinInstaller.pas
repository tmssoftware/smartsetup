unit UDelphiBerlinInstaller;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UMsBuildInstaller;

type
  TDelphiBerlinInstaller = class(TMsBuildInstaller)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
    function SupportsCppBuilder(const platform: TPlatform): boolean; override;
  end;
implementation

{ TDelphiBerlinInstaller }

function TDelphiBerlinInstaller.IDEName: TIDEName;
begin
  Result := TIDEName.delphiberlin;
end;

function TDelphiBerlinInstaller.DisplayName: string;
begin
  Result := 'Delphi 10.1 Berlin';
end;

function TDelphiBerlinInstaller.DllSuffix: string;
begin
  Result := '240';
end;

function TDelphiBerlinInstaller.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel, TPlatform.win64intel, TPlatform.macos32intel, TPlatform.iossimulator, TPlatform.iosdevice32, TPlatform.iosdevice64, TPlatform.android32];
end;
function TDelphiBerlinInstaller.SupportsCppBuilder(
  const platform: TPlatform): boolean;
begin
  Result := platform in [TPlatform.win32intel, TPlatform.win64intel, TPlatform.android32]; //we'll ignore ios32
end;

end.

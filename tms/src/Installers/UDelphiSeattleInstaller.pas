unit UDelphiSeattleInstaller;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UMsBuildInstaller;

type
  TDelphiSeattleInstaller = class(TMsBuildInstaller)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
    function SupportsCppBuilder(const platform: TPlatform): boolean; override;

  end;
implementation

{ TDelphiSeattleInstaller }

function TDelphiSeattleInstaller.IDEName: TIDEName;
begin
  Result := TIDEName.delphiseattle;
end;

function TDelphiSeattleInstaller.DisplayName: string;
begin
  Result := 'Delphi 10 Seattle';
end;

function TDelphiSeattleInstaller.DllSuffix: string;
begin
  Result := '230';
end;

function TDelphiSeattleInstaller.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel, TPlatform.win64intel, TPlatform.macos32intel, TPlatform.iossimulator, TPlatform.iosdevice32, TPlatform.iosdevice64, TPlatform.android32];
end;
function TDelphiSeattleInstaller.SupportsCppBuilder(
  const platform: TPlatform): boolean;
begin
  Result := platform in [TPlatform.win32intel, TPlatform.win64intel, TPlatform.android32]; //we'll ignore ios32
end;

end.

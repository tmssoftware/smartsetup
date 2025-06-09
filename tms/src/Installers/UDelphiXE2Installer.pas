unit UDelphiXE2Installer;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UMsBuildInstaller;

type
  TDelphiXE2Installer = class(TMsBuildInstaller)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
  end;
implementation

{ TDelphiXE2Installer }

function TDelphiXE2Installer.IDEName: TIDEName;
begin
  Result := TIDEName.delphixe2;
end;

function TDelphiXE2Installer.DisplayName: string;
begin
  Result := 'Delphi XE2';
end;

function TDelphiXE2Installer.DllSuffix: string;
begin
  Result := '160';
end;

function TDelphiXE2Installer.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel, TPlatform.win64intel, TPlatform.macos32intel];
end;
end.

unit UDelphiXE3Installer;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UMsBuildInstaller;

type
  TDelphiXE3Installer = class(TMsBuildInstaller)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
  end;
implementation

{ TDelphiXE3Installer }

function TDelphiXE3Installer.IDEName: TIDEName;
begin
  Result := TIDEName.delphixe3;
end;

function TDelphiXE3Installer.DisplayName: string;
begin
  Result := 'Delphi XE3';
end;

function TDelphiXE3Installer.DllSuffix: string;
begin
  Result := '170';
end;

function TDelphiXE3Installer.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel, TPlatform.win64intel, TPlatform.macos32intel];
end;
end.

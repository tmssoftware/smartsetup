unit UDelphi2010Installer;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UMsBuildInstaller;

type
  TDelphi2010Installer = class(TMsBuildInstaller)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
  end;
implementation

{ TDelphi2010Installer }

function TDelphi2010Installer.IDEName: TIDEName;
begin
  Result := TIDEName.delphi2010;
end;

function TDelphi2010Installer.DisplayName: string;
begin
  Result := 'Delphi 2010';
end;

function TDelphi2010Installer.DllSuffix: string;
begin
  Result := '140';
end;

function TDelphi2010Installer.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel];
end;
end.

unit UDelphi2005Installer;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UDcc32Installer;

type
  TDelphi2005Installer = class(TDcc32Installer)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
  end;
implementation

{ TDelphi2005Installer }

function TDelphi2005Installer.IDEName: TIDEName;
begin
  Result := TIDEName.delphi2005;
end;

function TDelphi2005Installer.DisplayName: string;
begin
  Result := 'Delphi 2005';
end;

function TDelphi2005Installer.DllSuffix: string;
begin
  Result := '90';
end;

function TDelphi2005Installer.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel];
end;
end.

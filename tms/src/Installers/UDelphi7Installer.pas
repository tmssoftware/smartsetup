unit UDelphi7Installer;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UDcc32Installer;

type
  TDelphi7Installer = class(TDcc32Installer)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
  end;
implementation

{ TDelphi7Installer }

function TDelphi7Installer.IDEName: TIDEName;
begin
  Result := TIDEName.delphi7;
end;

function TDelphi7Installer.DisplayName: string;
begin
  Result := 'Delphi 7';
end;

function TDelphi7Installer.DllSuffix: string;
begin
  Result := '70';
end;

function TDelphi7Installer.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel];
end;
end.

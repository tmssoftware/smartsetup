unit UDelphi6Installer;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UDcc32Installer;

type
  TDelphi6Installer = class(TDcc32Installer)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
  end;
implementation

{ TDelphi6Installer }

function TDelphi6Installer.IDEName: TIDEName;
begin
  Result := TIDEName.delphi6;
end;

function TDelphi6Installer.DisplayName: string;
begin
  Result := 'Delphi 6';
end;

function TDelphi6Installer.DllSuffix: string;
begin
  Result := '60';
end;

function TDelphi6Installer.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel];
end;
end.

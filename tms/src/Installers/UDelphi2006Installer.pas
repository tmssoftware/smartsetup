unit UDelphi2006Installer;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UDcc32Installer;

type
  TDelphi2006Installer = class(TDcc32Installer)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
  end;
implementation

{ TDelphi2006Installer }

function TDelphi2006Installer.IDEName: TIDEName;
begin
  Result := TIDEName.delphi2006;
end;

function TDelphi2006Installer.DisplayName: string;
begin
  Result := 'Delphi 2006';
end;

function TDelphi2006Installer.DllSuffix: string;
begin
  Result := '100';
end;

function TDelphi2006Installer.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel];
end;
end.

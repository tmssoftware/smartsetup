unit UDelphi2009Installer;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UDcc32Installer;

type
  TDelphi2009Installer = class(TDcc32Installer)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
  end;
implementation

{ TDelphi2009Installer }

function TDelphi2009Installer.IDEName: TIDEName;
begin
  Result := TIDEName.delphi2009;
end;

function TDelphi2009Installer.DisplayName: string;
begin
  Result := 'Delphi 2009';
end;

function TDelphi2009Installer.DllSuffix: string;
begin
  Result := '120';
end;

function TDelphi2009Installer.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel];
end;
end.

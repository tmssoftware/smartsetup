unit UDelphi13Installer;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, Deget.IDETypes, UInstaller, UMsBuildInstaller;

type
  TDelphi13Installer = class(TMsBuildInstaller)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
    function SupportsCppBuilder(const platform: TPlatform): boolean; override;
  end;
implementation

{ TDelphi12Installer }

function TDelphi13Installer.IDEName: TIDEName;
begin
  Result := TIDEName.delphi13;
end;

function TDelphi13Installer.DisplayName: string;
begin
  Result := DelphiNames[IDEName];
end;

function TDelphi13Installer.DllSuffix: string;
begin
  Result := PackageSuffixes[IDEName];
end;

function TDelphi13Installer.PlatformsSupported: TPlatformSet;
begin
  Result := PlatformsInDelphi[IDEName];
end;

function TDelphi13Installer.SupportsCppBuilder(
  const platform: TPlatform): boolean;
begin
  Result := platform in [TPlatform.win32intel, TPlatform.win64intel, TPlatform.android32, TPlatform.iosdevice64, win64Xintel];
end;

end.

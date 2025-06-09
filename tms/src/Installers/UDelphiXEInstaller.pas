unit UDelphiXEInstaller;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UMsBuildInstaller;

type
  TDelphiXEInstaller = class(TMsBuildInstaller)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;
    function DllSuffix: string; override;
    function PlatformsSupported: TPlatformSet; override;
  end;
implementation

{ TDelphiXEInstaller }

function TDelphiXEInstaller.IDEName: TIDEName;
begin
  Result := TIDEName.delphixe;
end;

function TDelphiXEInstaller.DisplayName: string;
begin
  Result := 'Delphi XE';
end;

function TDelphiXEInstaller.DllSuffix: string;
begin
  Result := '150';
end;

function TDelphiXEInstaller.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel];
end;
end.

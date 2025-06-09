unit UDcc32Installer;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UDelphiInstaller;

type
  TDcc32Installer = class(TDelphiInstaller)
  public
    function PackageExtension(const PackageType: TPackageType): TArray<string>; override;
  end;
implementation

{ TDcc32Installer }

function TDcc32Installer.PackageExtension(const PackageType: TPackageType): TArray<string>;
begin
  case PackageType of
    TPackageType.Exe: Result := ['.dpr'];
    TPackageType.Package: Result := ['.dpk', BinprojExtension];
  end;
end;

end.

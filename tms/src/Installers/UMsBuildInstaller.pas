unit UMsBuildInstaller;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UDelphiInstaller;

type
  TMsBuildInstaller = class(TDelphiInstaller)
  public
    function PackageExtension(const PackageType: TPackageType): TArray<string>; override;

  end;
implementation

{ TMsBuildInstaller }

function TMsBuildInstaller.PackageExtension(const PackageType: TPackageType): TArray<string>;
begin
  case PackageType of
    TPackageType.Package: Result := ['.dproj', '.cbproj', BinprojExtension];
    TPackageType.Exe: Result := ['.dproj', '.cbproj'];
  end;
end;

end.


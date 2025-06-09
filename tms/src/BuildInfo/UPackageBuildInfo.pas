unit UPackageBuildInfo;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UProjectDefinition;

type
  TPackageBuildInfo = class
  private
    FPackage: TPackage;
    FPackageFileName: string;
    FPackageNeedsRegistering: boolean;
    FDelphiVersion: TIDEName;
    FPlatform: TPlatform;
    FSupportsCppBuilder: boolean;
  public
    constructor Create(aPackage: TPackage; const aPackageFileName: string; const aPackageNeedsRegistering: boolean; const aDelphiVersion: TIDEName;
                const aPlatform: TPlatform; const aSupportsCppBuilder: boolean);

    property Package: TPackage read FPackage;
    property PackageFileName: string read FPackageFileName;
    property PackageNeedsRegistering: boolean read FPackageNeedsRegistering;


    property DelphiVersion: TIDEName read FDelphiVersion;
    property Platform: TPlatform read FPlatform;
    property SupportsCppBuilder: boolean read FSupportsCppBuilder;
    function PackageExt: string;
  end;

implementation
uses IOUtils;

{ TPackageBuildInfo }

constructor TPackageBuildInfo.Create(aPackage: TPackage; const aPackageFileName: string;
  const aPackageNeedsRegistering: boolean;
  const aDelphiVersion: TIDEName; const aPlatform: TPlatform; const aSupportsCppBuilder: boolean);
begin
  FPackage := aPackage;
  FPackageFileName := aPackageFileName;
  FPackageNeedsRegistering := aPackageNeedsRegistering;
  FDelphiVersion := aDelphiVersion;
  FPlatform := aPlatform;
  FSupportsCppBuilder := aSupportsCppBuilder;
end;

function TPackageBuildInfo.PackageExt: string;
begin
  Result := TPath.GetExtension(FPackageFileName);
end;

end.

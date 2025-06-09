unit UFullBuildInfo;
{$i ../../tmssetup.inc}
interface
uses UProjectBuildInfo, UIDEBuildInfo, UPlatformBuildInfo, UPackageBuildInfo;

type
  TFullBuildInfo = record
  private
    FProject: TProjectBuildInfo;
    FIDE: TIdeBuildInfo;
    FPlatform: TPlatformBuildInfo;
    FPackage: TPackageBuildInfo;
  public
    property Project: TProjectBuildInfo read FProject;
    property IDE: TIdeBuildInfo read FIDE;
    property Platform: TPlatformBuildInfo read FPlatform;
    property Package: TPackageBuildInfo read FPackage;

    constructor Create(const aProject: TProjectBuildInfo);

    function WithIDE(const aIDE: TIDEBuildInfo): TFullBuildInfo;
    function WithPlatform(const aPlatform: TPlatformBuildInfo): TFullBuildInfo;
    function WithPackage(const aPackage: TPackageBuildInfo): TFullBuildInfo;

  end;

implementation

{ TFullBuildInfo }

constructor TFullBuildInfo.Create(const aProject: TProjectBuildInfo);
begin
  FProject := aProject;
  FIDE := nil;
  FPlatform := nil;
  FPackage := nil;
end;

function TFullBuildInfo.WithIDE(const aIDE: TIDEBuildInfo): TFullBuildInfo;
begin
  Result.FProject := FProject;
  Result.FIDE := aIDE;
  Result.FPlatform := FPlatform;
  Result.FPackage := FPackage;
end;

function TFullBuildInfo.WithPlatform(
  const aPlatform: TPlatformBuildInfo): TFullBuildInfo;
begin
  Result.FProject := FProject;
  Result.FIDE := FIDE;
  Result.FPlatform := aPlatform;
  Result.FPackage := FPackage;

end;

function TFullBuildInfo.WithPackage(
  const aPackage: TPackageBuildInfo): TFullBuildInfo;
begin
  Result.FProject := FProject;
  Result.FIDE := FIDE;
  Result.FPlatform := FPlatform;
  Result.FPackage := aPackage;
end;


end.

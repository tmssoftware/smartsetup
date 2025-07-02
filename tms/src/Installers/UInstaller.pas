unit UInstaller;
{$i ../../tmssetup.inc}

interface
uses SysUtils, Generics.Collections, UFullBuildInfo,
     Deget.CoreTypes, UUninstallInfo, Megafolders.Definition;
type
  TInstaller = class
  public
    function IDEName: TIDEName; virtual; abstract;
    function DisplayName: string; virtual; abstract;

    function PlatformsSupported: TPlatformSet; virtual; abstract;
    function PackageExtension(const PackageType: TPackageType): TArray<string>; virtual; abstract;
    function PlatformsForDesign(const ProductId: string): TPlatformSet; virtual; abstract;
    function SupportsCppBuilder(const platform: TPlatform): boolean; virtual; abstract;

    procedure Build(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo); virtual; abstract;
    procedure CleanAllBuildTemporaryFiles(const UninstallInfo: IUninstallInfo); virtual; abstract;

    procedure RegisterAtIDELevel(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo); virtual; abstract;
    procedure RegisterAtPlatformLevel(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo); virtual; abstract;

    procedure UnRegisterAtIDELevel(const UninstallInfo: IUninstallInfo); virtual; abstract;
    procedure UnRegisterAtPlatformLevel(const UninstallInfo: IUninstallInfo); virtual; abstract;

    procedure UpdateProjectsSource(const BuildInfo: TFullBuildInfo); virtual; abstract;

    procedure UpdateMegafolders(const SourceFolder, ProjectId: string;
      const IDEName: TIDEName; const Platform: TPlatform;
      const BuildConfig: TBuildConfig;
      const UsedDcuMegafolders: TUsedMegafolders); virtual; abstract;


    procedure CreateTempProjects(const BuildInfo: TFullBuildInfo); virtual; abstract;
    procedure MoveDataFromTempProjects(const BuildInfo: TFullBuildInfo; const UsedDcuMegafolders: TUsedMegafolders); virtual; abstract;
    procedure RemoveTempProjects(const BuildInfo: TFullBuildInfo); virtual; abstract;

    function ProjectFileSupportsPlatform(const IgnoreDprojPlatforms: boolean; const RootFolder, PackageFileName: string; const dp: TPlatform): boolean; virtual; abstract;

  end;

  TInstallerFactory = class
  private
    class var Installers: array[TIDEName] of TInstaller;
  public
  class destructor Destroy;

  class function GetInstaller(const name: TIDEName): TInstaller;
  class procedure RegisterInstaller(const aInstaller: TInstaller);
  end;

implementation

{ TInstallerFactory }

class destructor TInstallerFactory.Destroy;
begin
  for var i := Low(Installers) to High(Installers) do
  begin
    Installers[i].Free;
  end;
end;

class function TInstallerFactory.GetInstaller(const name: TIDEName): TInstaller;
begin
  Result := Installers[name];
end;


class procedure TInstallerFactory.RegisterInstaller(const aInstaller: TInstaller);
begin
  if (Installers[aInstaller.IDEName] <> nil) then raise Exception.Create('Internal error: More than one installer with the same name.');
  Installers[aInstaller.IDEName] := aInstaller;
end;

end.

unit ULazarusInstaller;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UUninstallInfo, UFullBuildInfo;

type
  TLazarusInstaller = class(TInstaller)
  public
    function IDEName: TIDEName; override;
    function DisplayName: string; override;

    function PlatformsSupported: TPlatformSet; override;
    function PackageExtension(const PackageType: TPackageType): TArray<string>; override;

    function PlatformsForDesign(const ProductId: string): TPlatformSet;override;
    function SupportsCppBuilder(const platform: TPlatform): boolean;override;

    procedure Build(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo);override;
    procedure CleanAllBuildTemporaryFiles(const UninstallInfo: IUninstallInfo);override;

    procedure RegisterAtIDELevel(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo);override;
    procedure RegisterAtPlatformLevel(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo); override;

    procedure UnRegisterAtIDELevel(const UninstallInfo: IUninstallInfo);override;
    procedure UnRegisterAtPlatformLevel(const UninstallInfo: IUninstallInfo);override;

    procedure UpdateProjectsSource(const BuildInfo: TFullBuildInfo); override;

    procedure CreateTempProjects(const BuildInfo: TFullBuildInfo); override;
    procedure MoveDataFromTempProjects(const BuildInfo: TFullBuildInfo); override;
    procedure RemoveTempProjects(const BuildInfo: TFullBuildInfo); override;

    function ProjectFileSupportsPlatform(const IgnoreDprojPlatforms: boolean; const RootFolder, PackageFileName: string; const dp: TPlatform): boolean; override;

  end;
implementation
uses UIDEUtils, UMultiLogger, SysUtils, Deget.CommandLine;

{ TLazarusInstaller }

function TLazarusInstaller.IDEName: TIDEName;
begin
  Result := TIDEName.lazarus;
end;

function TLazarusInstaller.DisplayName: string;
begin
  Result := 'Lazarus';
end;

function TLazarusInstaller.PackageExtension(const PackageType: TPackageType): TArray<string>;
begin
  case PackageType of
    TPackageType.Package: Result := ['.lpk'];
    TPackageType.Exe: Result := ['.lpr'];
  end;

end;

function TLazarusInstaller.PlatformsSupported: TPlatformSet;
begin
  Result := [TPlatform.win32intel];
end;


function TLazarusInstaller.ProjectFileSupportsPlatform(const IgnoreDprojPlatforms: boolean;
  const RootFolder, PackageFileName: string; const dp: TPlatform): boolean;
begin
  Result := true;
end;

procedure TLazarusInstaller.Build(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo);
begin
  var LazBuild := GetCompilerPathAndExecutable(BuildInfo.Project.ProjectId, BuildInfo.IDE.Name, BuildInfo.Project.Config);
  var CompilerParameters := BuildInfo.Project.Config.CompilerParameters(BuildInfo.Project.ProjectId, BuildInfo.IDE.Name);
  
  Logger.Info(Format('Building package %s for "%s.%s".', [BuildInfo.Package.Package.Name, DisplayName,
  PlatformId[BuildInfo.Platform.Name]]));

  if not BuildInfo.Project.DryRun then
  begin
    if not ExecuteCommand(LazBuild + ' --build-all ' + CompilerParameters + ' "' + BuildInfo.Package.PackageFileName + '"')
      then raise Exception.Create('Failed to compile ' + BuildInfo.Package.PackageFileName);

  end;
end;

procedure TLazarusInstaller.CleanAllBuildTemporaryFiles(
  const UninstallInfo: IUninstallInfo);
begin

end;

function TLazarusInstaller.PlatformsForDesign(const ProductId: string): TPlatformSet;
begin
  Result := [];
end;

procedure TLazarusInstaller.RegisterAtIDELevel(
  const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo);
begin
  if BuildInfo.Project.SkipRegistering.Packages then exit;

  var LazBuild := GetCompilerPathAndExecutable(BuildInfo.Project.ProjectId, BuildInfo.IDE.Name, BuildInfo.Project.Config);
  var CompilerParameters := BuildInfo.Project.Config.CompilerParameters(BuildInfo.Project.ProjectId, BuildInfo.IDE.Name);
  Logger.Info('Rebuilding Lazarus...');

  if not BuildInfo.Project.DryRun then
  begin
    if not ExecuteCommand(LazBuild  + ' ' + CompilerParameters + ' --build-ide= ')
    then raise Exception.Create('Failed to rebuild lazarus IDE');
  end;
end;

procedure TLazarusInstaller.RegisterAtPlatformLevel(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo);
begin
  if BuildInfo.Project.SkipRegistering.Packages then exit;

  var LazBuild := GetCompilerPathAndExecutable(BuildInfo.Project.ProjectId, BuildInfo.IDE.Name, BuildInfo.Project.Config);
  var CompilerParameters := BuildInfo.Project.Config.CompilerParameters(BuildInfo.Project.ProjectId, BuildInfo.IDE.Name);
  for var BuildPackage in BuildInfo.Platform.PackagesBuildInfo do
  begin
    if not BuildPackage.Package.IsDesign then continue;

    if not ExecuteCommand(LazBuild + ' ' + CompilerParameters + ' --add-package "' + BuildPackage.PackageFileName + '"')
    then raise Exception.Create('Failed to register package: ' + BuildPackage.PackageFileName);
  end;
  Logger.Progress(Format('%s registered successfully on platform "%s.%s".',
    [BuildInfo.Project.Project.Application.Name, DisplayName, PlatformId[BuildInfo.Platform.Name]]), BuildInfo.Project.Progress);

end;

function TLazarusInstaller.SupportsCppBuilder(
  const platform: TPlatform): boolean;
begin
  Result := false;
end;

procedure TLazarusInstaller.UnRegisterAtIDELevel(
  const UninstallInfo: IUninstallInfo);
begin
end;

procedure TLazarusInstaller.UnRegisterAtPlatformLevel(
  const UninstallInfo: IUninstallInfo);
begin
end;

procedure TLazarusInstaller.UpdateProjectsSource(const BuildInfo: TFullBuildInfo);
begin
end;

procedure TLazarusInstaller.CreateTempProjects(const BuildInfo: TFullBuildInfo);
begin

end;

procedure TLazarusInstaller.MoveDataFromTempProjects(const BuildInfo: TFullBuildInfo);
begin
end;

procedure TLazarusInstaller.RemoveTempProjects(const BuildInfo: TFullBuildInfo);
begin
end;



end.

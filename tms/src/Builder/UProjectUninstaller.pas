unit UProjectUninstaller;
{$i ../../tmssetup.inc}

interface
uses UBuildInfo, UConfigDefinition, Deget.CoreTypes,
     UUninstallInfo, UPersistence, UProjectInstaller, UFileHasher;

type
TProjectUninstaller = class
private
  Config: TConfigDefinition;
  Persist: TPersistence;
  ProjectInstaller: TProjectInstaller;
  FileHasher: TFileHasher;

  procedure UninstallOneProject(const Project: IUninstallInfo);
  procedure UninstallOneIDE(const DryRun: boolean; const ProjectId, IDEId: string);
  procedure UninstallOnePlatform(const DryRun: boolean; const ProjectId, IDEId: string;
            const IdeName: TIDEName; const PlatformId: string);
  procedure UninstallOnePackage(const DryRun: boolean; const ProjectId: string;
            const IdeName: TIDEName; const Platform: TPlatform; Package: string);
public
  constructor Create(const aConfig: TConfigDefinition);
  destructor Destroy; override;
  procedure Uninstall(const aBuildInfo: TBuildInfo);
end;

implementation
uses UProjectInstallerConstants, ULogger, UMultiLogger, UFileSystemPersistence, IOUtils,
     UInstaller, SysUtils;

{ TProjectUninstaller }

constructor TProjectUninstaller.Create(const aConfig: TConfigDefinition);
begin
  Config := aConfig;
  Persist := TFileSystemPersistence.Create(Config.Folders.UninstallFolder, InstallerConstants.UninstallExtension);
  ProjectInstaller := TProjectInstaller.Create(Config);
  FileHasher := TFileHasher.Create(Config);
end;

destructor TProjectUninstaller.Destroy;
begin
  Persist.Free;
  ProjectInstaller.Free;
  FileHasher.Free;
  inherited;
end;

procedure TProjectUninstaller.Uninstall(const aBuildInfo: TBuildInfo);
begin
  for var Project in aBuildInfo.ProjectsToUninstall do
  begin
    UninstallOneProject(Project);
  end;

  for var Project in aBuildInfo.ProjectsToBuild do
  begin
    var IdesFullyUninstalled: TIDENameSet := [];
    for var ideu in Project.IDEsToUninstall do
    begin
      UninstallOneIDE(Project.DryRun, Project.ProjectId, IDEId[ideu.IDE]);
      Include(IdesFullyUninstalled, ideu.IDE);
    end;

    for var ide := Low(TIDEName) to High(TIDEName) do
    begin
      if ide in IdesFullyUninstalled then continue;
      for var platu in Project.PlatformsToUninstall.Get(ide) do
      begin
        UninstallOnePlatform(Project.DryRun, Project.ProjectId, IDEId[ide], ide, PlatformId[platu.Platform]);
      end;
    end;

    // No projects in ProjectsToBuild are to be uninstalled. Those are in ProjectsToUninstall.
  end;


end;

procedure TProjectUninstaller.UninstallOneProject(
  const Project: IUninstallInfo);
begin
    var ProjectName := Project.Value.ReadStr(InstallerConstants.ProjectNameJsonId, Project.ProjectId);
    Logger.Info('Uninstalling Project: ' + ProjectName);

    var IDEs := Persist.List(Project.ProjectId);
    for var ide in IDEs do
    begin
      UninstallOneIDE(Project.DryRun, Project.ProjectId, ide.Id);
    end;

    try
      ProjectInstaller.UnRegisterFullProject(Project.DryRun, Project.ProjectId);
    except on ex: Exception do
      Logger.Info('Error unregistering project: ' + ex.Message);
    end;

end;

procedure TProjectUninstaller.UninstallOneIDE(const DryRun: boolean; const ProjectId: string; const IDEId: string);
begin
  var IdeName := GetIDEName(IDEId);

  var Platforms := Persist.List(ProjectId, IDEId);
  for var Platform in Platforms do
  begin
    UninstallOnePlatform(DryRun, ProjectId, IDEId, IdeName, Platform.Id);
  end;

  if not (IdeName in ValidIDEs) then
  begin
    if not DryRun and (not Config.Unregistering) then Persist.Remove(ProjectId, IDEId);
    exit;
  end;

  try
    ProjectInstaller.UnRegisterAtIDELevel(DryRun, TInstallerFactory.GetInstaller(IdeName), ProjectId, IdeName);
  except on ex: Exception do
    Logger.Info('Error unregistering IDE: ' + ex.Message);
  end;
end;

procedure TProjectUninstaller.UninstallOnePlatform(const DryRun: boolean;
  const ProjectId, IDEId: string; const IdeName: TIDEName; const PlatformId: string);
begin
  var PlatformName := GetPlatformName(PlatformId);
  var HasErrors := true;
  Logger.StartSection(TMessageType.Unregister, 'Uninstalling ' + ProjectId + '.' + IDEId + '.' + PlatformId);
  try
    var Packages := Persist.List(ProjectId, IDEId, PlatformId);
    for var Package in Packages do
    begin
      UninstallOnePackage(DryRun, ProjectId, IdeName, PlatformName, Package.Id);
    end;

    if not (PlatformName in ValidPlatforms) or not (IdeName in ValidIDEs) then
    begin
      if not DryRun then Persist.Remove(ProjectId, IDEId, PlatformId);
      exit;
    end;
    try
      ProjectInstaller.UnRegisterAtPlatformLevel(DryRun, TInstallerFactory.GetInstaller(IdeName), ProjectId, IdeName, PlatformName);
      HasErrors := false;
    except on ex: Exception do
    begin
      Logger.Info('Error unregistering platform: ' + ex.Message);
    end;
    end;
  finally
    Logger.FinishSection(TMessageType.Unregister, HasErrors);
  end;
end;

procedure TProjectUninstaller.UninstallOnePackage(const DryRun: boolean;
  const ProjectId: string; const IdeName: TIDEName;
  const Platform: TPlatform; Package: string);
begin
  if not (Platform in ValidPlatforms) or not (IdeName in ValidIDEs) then
  begin
    if not DryRun and (not Config.Unregistering) then Persist.Remove(ProjectId, IDEId[IDEName], PlatformId[Platform], Package);
    exit;
  end;

  try
    ProjectInstaller.CleanAllBuildTemporaryFiles(DryRun, TInstallerFactory.GetInstaller(IdeName), FileHasher, ProjectId, IdeName, Platform, Package);
  except on ex: Exception do
    Logger.Info('Error unregistering platform: ' + ex.Message);
  end;
end;

end.

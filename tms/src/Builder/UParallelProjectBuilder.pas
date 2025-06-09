unit UParallelProjectBuilder;
{$i ../../tmssetup.inc}

interface
uses SysUtils, UConfigDefinition, UProjectDefinition, UBuildInfo, UProjectBuilderInterface, Deget.CoreTypes,
     UProjectBuildInfo, UIDEBuildInfo, UPlatformBuildInfo, UPackageBuildInfo, UProjectList,
     UInstaller, UFullBuildInfo, UFileHasher, UProjectInstaller,
     Threading, UParallelProjectInfo, UAppTerminated;

type
  TParallelProjectBuilder = class(TInterfacedObject, IProjectBuilder)
  private
    Config: TConfigDefinition;
    FileHasher: TFileHasher;
    ProjectInstaller: TProjectInstaller;
    ProcessedPackages, TotalPackages: integer;
    ShowProcessedPackagesProgress: boolean;
    ParallelProjects: TParallelProjectInfoList;

    procedure SetupLogger(const BuildInfo: TBuildInfo);

    procedure PreBuildOneProject(const BuildInfo: TFullBuildInfo);
    procedure DoBuildOneProject(const BuildInfo: TFullBuildInfo);
    procedure PostBuildOneProject(const BuildInfo: TFullBuildInfo);

    procedure PreBuildOneIDE(const BuildInfo: TFullBuildInfo);
    procedure DoBuildOneIDE(const BuildInfo: TFullBuildInfo);
    procedure PostBuildOneIDE(const BuildInfo: TFullBuildInfo);

    procedure BuildOnePlatform(const Installer: TInstaller; const BuildInfo: TFullBuildInfo);

    procedure BuildOnePackage(const Installer: TInstaller; const BuildInfo: TFullBuildInfo; var ProcessedInPlatform: integer);
    function IsPartialBuild(const BuildInfo: TFullBuildInfo): boolean;
    function DependenciesDontSupport(const Project: TProjectBuildInfo;
      const IDE: TIDEName; const Platform: TPlatform;
      out UnsupportedDependency: string): boolean;

  public
    constructor Create(const aConfig: TConfigDefinition; const aFileHasher: TFileHasher);
    destructor Destroy; override;
    procedure Build(BuildInfo: TBuildInfo);
  end;

implementation
uses ULogger, UMultiLogger, UDefines, UConfigKeys, UCoreTypes, UParallelDependencies,
     Generics.Collections, UBuildSummary, Deget.IDEInfo, Deget.DelphiInfo;

{ TDelphiBuilder }

constructor TParallelProjectBuilder.Create(const aConfig: TConfigDefinition; const aFileHasher: TFileHasher);
begin
  inherited Create;
  Config := aConfig;
  FileHasher := aFileHasher;
  ProjectInstaller := TProjectInstaller.Create(aConfig);
  ParallelProjects := TParallelProjectInfoList.Create;
end;

destructor TParallelProjectBuilder.Destroy;
begin
  Logger.ResetPercentAction;
  ProjectInstaller.Free;
  ParallelProjects.Free;
  inherited;
end;

procedure TParallelProjectBuilder.SetupLogger(const BuildInfo: TBuildInfo);
begin
  TotalPackages := BuildInfo.AllPackages;
  Logger.SetPercentAction(
  function : integer
  begin
     if (TotalPackages = 0)
     or (ProcessedPackages > TotalPackages)
     or (not ShowProcessedPackagesProgress)then exit(-1);

     Result := (ProcessedPackages * 100) div TotalPackages;
  end);
end;

procedure TParallelProjectBuilder.Build(BuildInfo: TBuildInfo);
begin
  ResolveDependencies(BuildInfo, ParallelProjects);
  ShowProcessedPackagesProgress := false;
  SetupLogger(BuildInfo);
  try
    for var SkippedProject in BuildInfo.ProjectsSkippedByConfig do
    begin
      Logger.Info('Skipping ' + SkippedProject.Application.NameAndVersion + '. Ignored in command line or the config file');
    end;

    for var BuildProject in BuildInfo.ProjectsToBuild do
    begin
      if (BuildProject.IDEsBuildInfo.Count = 0) then
      begin
        if BuildProject.Skipped.Empty then
        begin
          Logger.Info('Skipping ' + BuildProject.Project.Application.NameAndVersion + '. There is nothing to build from configuration.');
        end else
        begin
          Logger.Info('Skipping ' + BuildProject.Project.Application.NameAndVersion + '. This component has not been modified.');
        end;
      end;
    end;

    ShowProcessedPackagesProgress := true;
    try
      for var BuildProject in BuildInfo.ProjectsToBuild do
      begin
        if (BuildProject.IDEsBuildInfo.Count = 0) then continue;
        BuildProject.Progress.SetTotalPackages(BuildProject.AllPackages(true));
        var FullBuildInfo := TFullBuildInfo.Create(BuildProject);
        PreBuildOneProject(FullBuildInfo);
      end;

      for var BuildProject in BuildInfo.ProjectsToBuild do
      begin
        if (BuildProject.IDEsBuildInfo.Count = 0) or (BuildProject.PreBuildFailed) then continue;

        var FullBuildInfo := TFullBuildInfo.Create(BuildProject);
        DoBuildOneProject(FullBuildInfo);
      end;

      ParallelProjects.Run;

      for var BuildProject in BuildInfo.ProjectsToBuild do
      begin
        if (BuildProject.IDEsBuildInfo.Count = 0) or (BuildProject.PreBuildFailed) then continue;

        var FullBuildInfo := TFullBuildInfo.Create(BuildProject);
        PostBuildOneProject(FullBuildInfo);
      end;

      Logger.Info('All projects built.');
    finally
      ShowProcessedPackagesProgress := false;
    end;
  finally
    Logger.ResetPercentAction;
  end;
  LogSkipped(BuildInfo);
end;

procedure TParallelProjectBuilder.PreBuildOneProject(const BuildInfo: TFullBuildInfo);
begin
  Logger.StartSection(TMessageType.Unregister, 'Unregistering ' + BuildInfo.Project.ProjectId);
  try
    try
      if not BuildInfo.Project.DryRun then CreateDefinesInclude(Config, BuildInfo.Project.Project);
    except on ex: Exception do
      begin
        BuildInfo.Project.PreBuildFailed := true;
        AtomicIncrement(ProcessedPackages, BuildInfo.Project.AllPackages);
        Logger.Error('Error Creating Defines for project "' + BuildInfo.Project.Project.Application.NameAndVersion + '": ' + ex.Message);
        exit;
      end;
    end;


    ProjectInstaller.UnRegisterFullProject(BuildInfo.Project.DryRun, BuildInfo.Project.ProjectId);

    for var BuildIDE in BuildInfo.Project.IDEsBuildInfo do
    begin
      PreBuildOneIDE(BuildInfo.WithIDE(BuildIDE));
    end;
  finally
    Logger.FinishSection(TMessageType.Unregister);
  end;
end;

procedure TParallelProjectBuilder.DoBuildOneProject(const BuildInfo: TFullBuildInfo);
begin
  for var BuildIDE in BuildInfo.Project.IDEsBuildInfo do
  begin
    DoBuildOneIDE(BuildInfo.WithIDE(BuildIDE));
  end;
end;

procedure TParallelProjectBuilder.PostBuildOneProject(const BuildInfo: TFullBuildInfo);
begin
  Logger.StartSection(TMessageType.Register, 'Registering ' + BuildInfo.Project.ProjectId);
  try
    try
      for var BuildIDE in BuildInfo.Project.IDEsBuildInfo do
      begin
        PostBuildOneIDE(BuildInfo.WithIDE(BuildIDE));
      end;
      ProjectInstaller.RegisterFullProject(BuildInfo.Project);
    except on ex: Exception do
      begin
        BuildInfo.Project.PostBuildFailed := true;
        Logger.Error('Error installing project "' + BuildInfo.Project.Project.Application.NameAndVersion + '": ' + ex.Message);
      end;
    end;
  finally
    Logger.FinishSection(TMessageType.Register);
  end;
end;

procedure TParallelProjectBuilder.PreBuildOneIDE(const BuildInfo: TFullBuildInfo);
begin
  var Installer := TInstallerFactory.GetInstaller(BuildInfo.IDE.Name);
  ProjectInstaller.UnRegisterAtIDELevel(BuildInfo.Project.DryRun, Installer, BuildInfo.Project.ProjectId, BuildInfo.IDE.Name);
end;

function TParallelProjectBuilder.DependenciesDontSupport(const Project: TProjectBuildInfo; const IDE: TIDEName; const Platform: TPlatform; out UnsupportedDependency: string): boolean;
begin
  for var dep in Project.Dependencies do
  begin
    if not Dep.ContainsIDEAndPlatform(IDE, Platform) then begin; UnsupportedDependency := Dep.ProjectId; exit(true); end;

  end;
  UnsupportedDependency := '';
  Result := false;
end;

procedure TParallelProjectBuilder.DoBuildOneIDE(const BuildInfo: TFullBuildInfo);
begin
  var Installer := TInstallerFactory.GetInstaller(BuildInfo.IDE.Name);

  for var BuildPlatform in BuildInfo.IDE.PlatformsBuildInfo do
  begin
    if not BuildInfo.Project.DryRun and not BuildInfo.Project.Project.IsExe and Config.ModifySources(BuildInfo.Project.ProjectId) then
    begin
      //This will install many times the same project, but see https://github.com/tmssoftware/tms-smartsetup/issues/167
      Installer.UpdateProjectsSource(BuildInfo.WithPlatform(BuildPlatform));
    end;
    BuildOnePlatform(Installer, BuildInfo.WithPlatform(BuildPlatform));
  end;
end;

procedure TParallelProjectBuilder.PostBuildOneIDE(const BuildInfo: TFullBuildInfo);
begin
  var Installer := TInstallerFactory.GetInstaller(BuildInfo.IDE.Name);
  try
    ProjectInstaller.RegisterAtIDELevel(Installer, BuildInfo);
  except on ex: Exception do
    begin
      BuildInfo.IDE.PostBuildFailed := true;
      Logger.Error('Error installing project "' + BuildInfo.Project.Project.Application.NameAndVersion
                  + '" in IDE ' + IDEId[BuildInfo.IDE.Name] + ': ' + ex.Message);
    end;
  end;
end;

procedure TParallelProjectBuilder.BuildOnePlatform(const Installer: TInstaller; const BuildInfo: TFullBuildInfo);
begin
  ParallelProjects.SetRun(BuildInfo.Project.ProjectId, IDEId[BuildInfo.IDE.Name],
      PlatformID[BuildInfo.Platform.Name],  procedure
  begin
    Logger.StartSection(TMessageType.Build, BuildInfo.Project.ProjectId + '->' + IDEId[BuildInfo.IDE.Name] + '.' + PlatformID[BuildInfo.Platform.Name]);
    ProjectInstaller.UnRegisterAtPlatformLevel(BuildInfo.Project.DryRun, Installer, BuildInfo.Project.ProjectId, BuildInfo.IDE.Name, BuildInfo.Platform.Name);
    var ProcessedInPlatform := 0;
    try
      if not IsPartialBuild(BuildInfo) then
      begin
        try
          ProjectInstaller.CleanAllBuildTemporaryFilesInPlat(BuildInfo.Project.DryRun, Installer, FileHasher, BuildInfo.Project.ProjectId, BuildInfo.IDE.Name, BuildInfo.Platform.Name);
        except on ex: Exception do
          begin
            Logger.Info('Error removing tmp files for project ' + BuildInfo.Project.Project.Application.Name
                  + ', ' + IDEId[BuildInfo.IDE.Name] + ', ' + PlatformId[BuildInfo.Platform.Name]
                  + ': ' + ex.Message);

          end;
        end;
        ProjectInstaller.CreateTempProjects(Installer, BuildInfo)

      end;

      var UnsupportedDependency: string;
      if DependenciesDontSupport(BuildInfo.Project, BuildInfo.IDE.Name, BuildInfo.Platform.Name, UnsupportedDependency) then
      begin
        Logger.Info('Skipped installing ' + BuildInfo.Project.ProjectId + ' in ' + IDEId[BuildInfo.IDE.Name]
          + '.' + PlatformId[BuildInfo.Platform.Name] + ' because ' + UnsupportedDependency + ' is not set to be installed in that configuration.');
        BuildInfo.Project.Notes.Add(BuildInfo.Project.ProjectId + ' in ' + IDEId[BuildInfo.IDE.Name]
          + '.' + PlatformId[BuildInfo.Platform.Name] + ' (' + UnsupportedDependency + ' not set to be installed in that configuration)' , BuildInfo.IDE.Name, TNoteType.SkippedProduct);
        AtomicIncrement(ProcessedPackages, BuildInfo.Platform.PackagesBuildInfo.Count);
      end
      else
      begin
        for var BuildPackage in BuildInfo.Platform.PackagesBuildInfo do
        begin
          CheckAppTerminated;
          BuildOnePackage(Installer, BuildInfo.WithPackage(BuildPackage), ProcessedInPlatform);
        end;
        ProjectInstaller.MoveDataFromTempProjects(Installer, BuildInfo);
        ProjectInstaller.RemoveTempProjects(Installer, BuildInfo);
        ProjectInstaller.RegisterAtPlatformLevel(Installer, BuildInfo);
        //When we analyze, we analyze up to platform level. This is the hash that matters.
      end;


      var BinaryHash := '';
      {$IFDEF MSWINDOWS}
      var IDEInfo: IDelphiIDEInfo := TDelphiIDEInfo.Create(BuildInfo.IDE.Name, BuildInfo.Project.AlternateRegistryKey, BuildInfo.IDE.PathToCompiler);
      BinaryHash := FileHasher.GenerateBinaryHash(BuildInfo.Project.Project, BuildInfo.Platform.PackagesFolder, IDEInfo.GetPlatform(BuildInfo.Platform.Name));
      {$ELSE}
        {$message warn 'We need to review this for lazarus, even laz in windows. If we don''t put a binary hash, we can have https://github.com/tmssoftware/tms-smartsetup/issues/148'}
      {$ENDIF}
      var ProductHash := TProductHash.Create(BuildInfo.Project.SourceCodeHash, BinaryHash);
      if not BuildInfo.Project.DryRun then FileHasher.SaveHash(BuildInfo.Project.Project, ProductHash, BuildInfo.IDE.Name, BuildInfo.Platform.Name, '');
      BuildInfo.Platform.Ok := true;


    except on ex: Exception do
      begin
        BuildInfo.Platform.Ok := false;
        if ProcessedInPlatform < BuildInfo.Platform.PackagesBuildInfo.Count then
        begin
          AtomicIncrement(ProcessedPackages, BuildInfo.Platform.PackagesBuildInfo.Count - ProcessedInPlatform);
        end;
        Logger.Error('Error building project "' + BuildInfo.Project.Project.Application.NameAndVersion
                    + '" in IDE ' + IDEId[BuildInfo.IDE.Name]
                    + ' in Platform ' + PlatformId[BuildInfo.Platform.Name]
                    + ': ' + ex.Message);
      end;
    end;
    Logger.FinishSection(TMessageType.Build, not BuildInfo.Platform.Ok);
  end);
end;

procedure TParallelProjectBuilder.BuildOnePackage(const Installer: TInstaller; const BuildInfo: TFullBuildInfo; var ProcessedInPlatform: Integer);
begin
  //We don't care about binary hash here, that is only checked at platform level.
  var ProductHash := TProductHash.Create(BuildInfo.Project.SourceCodeHash, '');

  //This hash here is an optimization to not recompile projects that didn't fail in a failed platform. We aren't currently using that optimization, as PartialBuild always returns false.
  if FileHasher.ProductModified(ProductHash, BuildInfo.Project.Project, Config, BuildInfo.IDE.Name, BuildInfo.Platform.Name, BuildInfo.Package.Package.Name) then
  begin
    ProjectInstaller.Build(Installer, BuildInfo);
    if not BuildInfo.Project.DryRun then FileHasher.SaveHash(BuildInfo.Project.Project, ProductHash, BuildInfo.IDE.Name, BuildInfo.Platform.Name, BuildInfo.Package.Package.Name);
  end else
  begin
    Logger.Trace('Skipped building package "'+ BuildInfo.Package.Package.Name + '" for "' +  BuildInfo.Project.Project.Application.NameAndVersion
                    + '" in ' + IDEId[BuildInfo.IDE.Name]
                    + '.' + PlatformId[BuildInfo.Platform.Name] +' because it was already built.');
  end;


  AtomicIncrement(ProcessedPackages);
  Inc(ProcessedInPlatform); //no need for atomic here, this is not going to run in parallel.
end;

function TParallelProjectBuilder.IsPartialBuild(const BuildInfo: TFullBuildInfo): boolean;
begin
  //After some consideration I think this is not worth doing. The commented code below should work,
  //but it is not safe to assume that some unintended thing didn't happen and we shouldn't retry everything.
  //The advantage we get by allowing partial builds is that if, say you already compiled FlexCel.Core and it failed at FlexCel.XlsAdapter, the
  //next run we will skip FlexCel.Core. But something unexpected happened in FlexCel.XlsAdapter, so it is better to retry everything anyway.
  //And in many cases like the one at https://github.com/tmssoftware/tms-smartsetup/issues/136
  //This will just complicate things. In normal use, there should be no need for partial build anyway.
  if not Config.PartialBuilds(BuildInfo.Project.ProjectId) then exit(false);

   exit( true);
   //Review the cases below. They should be fixed with a hash that includes the dependencies. Also every hash
   //for every package should be different, by now they are all the same.

  {//We can't support partial builds in projects that have dependencies, since the logic gets too complex.
  //We could have failed say package 3 of 5 (and 1 and 2 are ok), but 1 and 2 still need to be recompiled as a dependency deep down changed.
  if BuildInfo.Project.Dependencies.Count > 0 then exit(false);

  //If the hash for the plaform is present, this was completed. If there are no hashes, it never started.
  //To be a partial hash it has to have some hashes, but not all.
  Result := FileHasher.ContainsHash(BuildInfo.Project.SourceCodeHash, BuildInfo.Project.Project, BuildInfo.IDE.Name, BuildInfo.Platform.Name)
        and (FileHasher.GetStoredHash(BuildInfo.Project.Project, BuildInfo.IDE.Name, BuildInfo.Platform.Name, '').Empty);
  }
end;
end.

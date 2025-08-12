unit UProjectAnalyzer;
{$i ../../tmssetup.inc}

interface
uses Classes, System.Generics.Collections, UConfigDefinition, UProjectDefinition, UFileHasher, UCoreTypes, Deget.CoreTypes,
     UIDEUtils, SysUtils, UBuildInfo, UProjectList, UInstaller, UPackageCache,
     UUninstallInfo, UMultiLogger;

type
  TIDENameArray = Array[TIDEName] of TIDEName;

  TProjectAnalyzer = class
  private
    var
    FBuildInfo: TBuildInfo;
    Config: TConfigDefinition;
    ProjectList: TProjectList;
    FProjectFinder: THashSet<string>;
    FileHasher: TFileHasher;

    procedure AnalyzeOneProject(const Project: TProjectDefinition);
    procedure AnalyzePackages(const PackageCache: TPackageCache; const Project: TProjectDefinition; const DepsCompiled: TPlatsCompiled; const BasePackagesFolder: string);
    procedure AnalyzeOnePackage(const PackageCache: TPackageCache; const Project: TProjectDefinition; const dv: TIDEName; const dp: TPlatform;
              const Package: TPackage; const DepsCompiled: TPlatsCompiled; const ProductHash: TProductHash; const CppBuilderSupport: boolean; const PackagesFolder: string);
    function DependenciesRebuilt(
        const Config: TConfigDefinition;
        const Project: TProjectDefinition): TPlatsCompiled;

    function PackageSupportsPlatform(const dv: TIDEName; const dp: TPlatform; const Project: TProjectDefinition; const Package: TPackage): boolean;
    function PackageSupportsCppBuilder(const dv: TIDEName; const dp: TPlatform; const Project: TProjectDefinition; const Package: TPackage): boolean;
    procedure Validate(const Projects: TProjectDefinitionList);
    function GetPackagesFolder(const PackageCache: TPackageCache;
       const AppFolder: string; const exts: TArray<string>; const Packages: TListOfPackages; const IsExe: boolean; const PackageFolders: TPackageFolders): string;
    procedure AnalyzeProjectsToInclude(const Projects: TProjectDefinitionList);
    procedure AddInstalled(const Dict: TDictionary<string, boolean>);
    function FindMissingWeakDependencies(const dv: TIDEName;
      const dp: TPlatform; const Project: TProjectDefinition;
      const Package: TPackage): string;
    procedure DependencyInclude(const Map: TDictionary<string, TProjectDefinition>; const Project: TProjectDefinition; const Dependencies: TObjectList<TDependency>; var IncludedCount: integer; const IsWeak: boolean);
    function GetUninstallList: TObjectList<TProjectDefinition>;
    function BackToFolder(const StartFolder: string;
      const PackageFolders: TPackageFolders): string;
    function GetIDEs(const Project: TProjectDefinition): TIDENameArray;

  public
    constructor Create(const aConfig: TConfigDefinition; const aProjectList: TProjectList; const aFileHasher: TFileHasher);
    destructor Destroy; override;

    procedure Analyze(const Projects: TProjectDefinitionList);
    procedure CheckAllDependenciesExist(const Projects: TProjectDefinitionList);
    procedure AnalyzeUnusedPackages;
    property BuildInfo: TBuildInfo read FBuildInfo;
    property ProjectFinder: THashSet<string> read FProjectFinder;
  end;

implementation
uses UPackageFinder, UProjectBuildInfo, UIDEBuildInfo,
     UPlatformBuildInfo, UPackageBuildInfo, Generics.Collections, UProjectInstaller,
     UAppTerminated, IOUtils, Deget.IDEInfo, Deget.DelphiInfo, UTmsBuildSystemUtils;

{ TProjectAnalyzer }

procedure TProjectAnalyzer.DependencyInclude(const Map: TDictionary<string, TProjectDefinition>; const Project: TProjectDefinition; const Dependencies: TObjectList<TDependency>; var IncludedCount: integer; const IsWeak: boolean);
begin
  for var dep in Dependencies do
  begin
    var ProjDep: TProjectDefinition;
    if not Map.TryGetValue(dep.Id, ProjDep) then
    begin
      if IsWeak then continue;
      raise Exception.Create('The product "' + Project.Application.Name + '" requires the product "' + dep.Description + '" to build, and it isn''t present. Add product "' + dep.Id + '" to the list of installed products.' );
    end;

    if Project.IncludeInBuild then
    begin
      if not ProjDep.IncludeInBuild then
      begin
        ProjDep.IncludeInBuild := true;
        Inc(IncludedCount);
      end
    end
    else
    begin
      if ProjDep.IncludeInBuild then
      begin
        Project.IncludeInBuild := true;
        Inc(IncludedCount);
      end
    end;
  end;

end;

procedure TProjectAnalyzer.AnalyzeProjectsToInclude(const Projects: TProjectDefinitionList);
begin
  var IncludedCount := 0;
  for var Project in Projects do
  begin
    Project.IncludeInBuild := Config.IsIncluded(Project.Application.Id);
    if Project.IncludeInBuild then Inc(IncludedCount);
  end;
  if (IncludedCount = Projects.Count) or (IncludedCount = 0) then exit; //no need to analyze further.


  var Map := TDictionary<string, TProjectDefinition>.Create;
  try
    ProjectList.LoadDeps(Map);

    var FullyAnalyzed := THashSet<string>.Create;
    try
      var OldIncludedCount:= -1;
      while (IncludedCount <> OldIncludedCount) and (IncludedCount < Projects.Count) do
      begin
        OldIncludedCount := IncludedCount;
        for var i := Projects.Count - 1 downto 0 do //It works forwards and backwards, but going backwards should be faster.
        begin
          var Project := Projects[i];
          if FullyAnalyzed.Contains(Project.Application.Id) then continue;
          if Project.IncludeInBuild then FullyAnalyzed.Add(Project.Application.Id);

          DependencyInclude(Map, Project, Project.Dependencies, IncludedCount, false);
          DependencyInclude(Map, Project, Project.WeakDependencies, IncludedCount, true);

        end;
      end;
    finally
      FullyAnalyzed.Free;
    end;
  finally
    Map.Free;
  end;
end;

procedure TProjectAnalyzer.CheckAllDependenciesExist(
  const Projects: TProjectDefinitionList);
begin
  var Map := TDictionary<string, TProjectDefinition>.Create;
  try
    ProjectList.LoadDeps(Map);

    for var Project in Projects do
    begin
      if not Project.IncludeInBuild then continue;
      for var dep in Project.Dependencies do
      begin
        if not Map.ContainsKey(dep.Id) then
        begin
          raise Exception.Create('The product "' + Project.Application.Name + '" requires the product "' + dep.Description + '" to build, and it isn''t present. Add product "' + dep.Id + '" to the list of installed products.' );
        end;

      end;
    end;
  finally
    Map.Free;
  end;
end;


procedure TProjectAnalyzer.Analyze(const Projects: TProjectDefinitionList);
begin
  Validate(Projects);
  AnalyzeProjectsToInclude(Projects);
  CheckAllDependenciesExist(Projects);
  for var Project in Projects do Project.NeedsCompiling.Clear;
  for var Project in Projects do AnalyzeOneProject(Project);

  BuildInfo.ResolveDependencies();
end;

function IsProjectPackage(const FileName: string;  const Packages: TListOfPackages): boolean;
begin
  for var Package in Packages do
  begin
    if SameText(Package.Name, FileName) then exit(true);
  end;
  Result := false;
end;

function TProjectAnalyzer.BackToFolder(const StartFolder: string; const PackageFolders: TPackageFolders): string;
begin
  for var PackageFolder in PackageFolders do
  begin
    if PackageFolder = '' then continue;
    if TPackageFinder.EndsInFolder(StartFolder, PackageFolder, Result) then exit;

  end;

  Result := TPath.GetDirectoryName(StartFolder);
end;

function TProjectAnalyzer.GetPackagesFolder(const PackageCache: TPackageCache; const AppFolder: string; const exts: TArray<string>; const Packages: TListOfPackages; const IsExe: boolean; const PackageFolders: TPackageFolders): string;
begin
  Result := '';
  var Pkgs := PackageCache.GetFiles(AppFolder, exts);

  for var ExtPkgs in Pkgs do
  begin
    for var Pkg in ExtPkgs.Values do
    begin
      for var P in Pkg do
      begin
        if P = '' then continue;

        var PkgFolderIDE := TPath.GetDirectoryName(P);
        if not IsProjectPackage(TPath.GetFileNameWithoutExtension(P), Packages) then continue;

        var PkgFolder := PkgFolderIDE; //for exes.
        if not IsExe then
        begin
          PkgFolder := BackToFolder(PkgFolderIDE, PackageFolders);
        end;
        if Result = '' then Result := PkgFolder else if Result <> PkgFolder then raise Exception.Create('Packages in a project can''t be in more than one folder. ' + AppFolder + ' has folders in ' + Result + ' and ' + PkgFolder);
      end;
    end;
  end;

  if Result = '' then raise Exception.Create('Can''t find any packages in the project ' + AppFolder);

end;

procedure TProjectAnalyzer.AnalyzeOneProject(const Project: TProjectDefinition);
begin
  CheckAppTerminated;

  Project.Validate;
  if not Project.IncludeInBuild then
  begin
    BuildInfo.ProjectsSkippedByConfig.Add(Project);
    exit;
  end;

  BuildInfo.ProjectsToBuild.Add(TProjectBuildInfo.Create(Config, Project));
  BuildInfo.ProjectToProjectBuildInfo.Add(Project.Application.Id, BuildInfo.CurrentProject);
  var PackageCache := TPackageCache.Create;
  try
    BuildInfo.CurrentProject.SourceCodeHash := FileHasher.GenerateSourceCodeHash(PackageCache, Project);
    var PackagesFolder :=  TPath.GetDirectoryName(Project.FullPath);
    if Project.RootPackageFolder <> '' then
    begin
      PackagesFolder := TPath.GetFullPath(TPath.Combine(PackagesFolder, Project.RootPackageFolder));
      if FolderIsOutside(PackagesFolder, [Project.RootFolder])
        then raise Exception.Create('The base package folder can''t be outside the root folder of the project. Current base package folder is: "' + PackagesFolder + '" and the root folder is "' + Project.RootFolder + '"');

      BuildInfo.CurrentProject.BasePackagesFolder := PackagesFolder;
    end else
    begin
      BuildInfo.CurrentProject.BasePackagesFolder := GetPackagesFolder(PackageCache, PackagesFolder, Project.FileNameExtension, Project.Packages, Project.IsExe, Project.PackageFolders);
    end;
    var DepsCompiled := DependenciesRebuilt(Config, Project);

    AnalyzePackages(PackageCache, Project, DepsCompiled, BuildInfo.CurrentProject.BasePackagesFolder);
  finally
    PackageCache.Free;
  end;
end;

function TProjectAnalyzer.PackageSupportsPlatform(const dv: TIDEName; const dp: TPlatform; const Project: TProjectDefinition; const Package: TPackage): boolean;
begin
  for var fr := Low(TFramework) to High(TFramework) do
  begin
    if not (fr in Package.Frameworks) then continue;

    if Project.FrameworkSupportsPlatform(dv, dp, fr) then exit(true);
  end;

  Result := false;
end;

function TProjectAnalyzer.FindMissingWeakDependencies(const dv: TIDEName; const dp: TPlatform; const Project: TProjectDefinition; const Package: TPackage): string;
begin
  Result := '';
  for var fr := Low(TFramework) to High(TFramework) do
  begin
    if not (fr in Package.Frameworks) then continue;
    if not Project.FrameworkSupportsPlatform(dv, dp, fr) then continue;

    Result := Project.FrameworkHasAllWeakDependencies(ProjectFinder, dv, dp, fr);
    if Result = '' then exit;
  end;

end;

procedure TProjectAnalyzer.AddInstalled(const Dict: TDictionary<string, boolean>);
begin
  var Uninstaller := TProjectInstaller.Create(Config);
  try
    for var proj: IUninstallInfo in Uninstaller.GetAllProjects do
    begin
      Dict.AddOrSetValue(proj.ProjectId, true);
    end;
  finally
    Uninstaller.Free;
  end;
end;

procedure TProjectAnalyzer.Validate(const Projects: TProjectDefinitionList);
begin
  var Dict := TDictionary<string, boolean>.Create;
  try
    for var p in Projects do Dict.AddOrSetValue(p.Application.Id, true);
    AddInstalled(Dict);
    Config.Validate(Dict);
  finally
    Dict.Free;
  end;
end;

function TProjectAnalyzer.PackageSupportsCppBuilder(const dv: TIDEName; const dp: TPlatform; const Project: TProjectDefinition; const Package: TPackage): boolean;
begin
  if not TInstallerFactory.GetInstaller(dv).SupportsCppBuilder(dp) then exit(false);
  for var fr := Low(TFramework) to High(TFramework) do
  begin
    if not (fr in Package.Frameworks) then continue;

    if Project.FrameworkSupportsPlatform(dv, dp, fr) then
    begin
      var Framework := Project.GetFramework(Project.GetFrameworkName(fr));
      if Framework.SupportsCppBuilder then exit(true);
    end;
  end;

  Result := false;
end;

function TProjectAnalyzer.GetIDEs(const Project: TProjectDefinition): TIDENameArray;
begin
  for var dv := Low(TIDEName) to High(TIDEName) do
  begin
    var dvr := dv;
    if Project.IsExe and (Project.ExeOptions.CompileWith = TExeCompileWith.Latest) then dvr := TIDEName(Integer(High(TIDEName)) + Integer(Low(TIDEName)) - Integer(dv));
    Result[dvr] := dv;
  end;

end;

procedure TProjectAnalyzer.AnalyzePackages(const PackageCache: TPackageCache; const Project: TProjectDefinition; const DepsCompiled: TPlatsCompiled; const BasePackagesFolder: string);
var
  ErrorMessage: string;
begin
  var SupportedIDENames := Config.GetIDENames(Project.Application.Id);
  var AlreadyProcessed := -1;
  for var dv in GetIDES(Project) do
  begin
    if not (dv in SupportedIDENames) then continue;

    var Naming := Config.GetNaming(Project.Naming, Project.FullPath);
    if not Project.IsExe then
    begin
      if not (TPackageFinder.PackagesExist(BasePackagesFolder, dv, Naming, false, Project.PackageFolders[dv])) then continue; //This is for the case user didn't specify "ide until". If the package doesn't exist, we ignore it.
    end;


    if (not Project.SupportsIDE(dv)) then continue; //no need for log, this is expected.

    var Installer := TInstallerFactory.GetInstaller(dv);
    if not IsProductInstalled(dv, Project.Application.Id, Config, ErrorMessage) then
    begin
      BuildInfo.CurrentProject.Notes.Add('Skipped ' + Installer.DisplayName + '. ' + ErrorMessage, Installer.IDEName, TNoteType.SkippedIDE);
      continue;
    end;

    if (AlreadyProcessed > 0) and Project.IsExe and (Project.ExeOptions.CompileWith <> TExeCompileWith.All) then
    begin
      BuildInfo.CurrentProject.Notes.Add('Skipped ' + Project.Application.Id + ' for ' + Installer.DisplayName + '. It was compiled with ' + IDEId[TIDEName(AlreadyProcessed)] + '.', Installer.IDEName, TNoteType.SkippedIDE);
      continue;
    end;
    AlreadyProcessed := Ord(dv);
    var plats := Config.GetPlatforms(dv, Project.Application.Id);
    for var dp in plats do
    begin
      if not Project.SupportsPlatform(dp) then continue;
      if not (dp in Installer.PlatformsSupported) then continue;
      if (dp = TPlatform.win64Xintel) and (not Project.SupportsCpp) then continue; //No need to compile Win64x if C++ builder is not supported by the component.

      if not IsPlatformInstalled(BuildInfo.CurrentProject.ProjectId, Config, dv, dp, ErrorMessage) then
      begin
        BuildInfo.CurrentProject.Notes.Add('Skipped ' + Installer.DisplayName + ', ' + PlatformId[dp] + '. ' + ErrorMessage, Installer.IDEName, TNoteType.SkippedPlatform);
        continue;
      end;

      if not IsSDKInstalled(BuildInfo.CurrentProject.ProjectId, Config, dv, dp) then
      begin
        BuildInfo.CurrentProject.Notes.Add('Skipped ' + Installer.DisplayName + ', ' + PlatformId[dp] + ' because the SDKs are not available.', Installer.IDEName, TNoteType.MissingSDK);
        continue;
      end;

      var PackagesFolder := TPackageFinder.PackagesFolder(BasePackagesFolder, dv, Naming, Project.IsExe, Project.PackageFolders[dv]);
      var BinaryHash := '';
      {$IFDEF MSWINDOWS}
      var IDEInfo: IDelphiIDEInfo := TDelphiIDEInfo.Create(dv, BuildInfo.CurrentProject.AlternateRegistryKey, '');
      BinaryHash := FileHasher.GenerateBinaryHash(Project, PackagesFolder, IDEInfo.GetPlatform(dp));
      {$ELSE}
      {$message warn 'We need to review this for lazarus, even laz in windows. If we don''t put a binary hash, we can have https://github.com/tmssoftware/tms-smartsetup/issues/148'}
      {$ENDIF}
      var ProductHash := TProductHash.Create(BuildInfo.CurrentProject.SourceCodeHash, BinaryHash);

      for var Package in Project.Packages do
      begin
        if not PackageSupportsPlatform(dv, dp, Project, Package) then continue;
        var MissingDependency := FindMissingWeakDependencies(dv, dp, Project, Package);
        if MissingDependency <> '' then
        begin
          if MissingDependency.StartsWith('~') then Logger.Trace('Package "' + Package.Name + '" for ' + IDEId[dv] + '.' + PlatformId[dp] + ' SKIPPED because the project ' + MissingDependency.Substring(1) + ' is installed.')
          else Logger.Trace('Package "' + Package.Name + '" for '  + IDEId[dv] + '.' + PlatformId[dp] + ' SKIPPED because the project ' + MissingDependency + ' is not installed.');
          continue;
        end;

        var CppBuilderSupport := PackageSupportsCppBuilder(dv, dp, Project, Package);
        if (dp = TPlatform.win64Xintel) and not CppBuilderSupport then Continue;

        AnalyzeOnePackage(PackageCache, Project, dv, dp, Package, DepsCompiled, ProductHash, CppBuilderSupport, PackagesFolder);

      end;
    end;
  end;
end;

function TProjectAnalyzer.GetUninstallList: TObjectList<TProjectDefinition>;
begin
  //BuildInfo.ProjectsToUninstall isn't yet loaded here. And we can't change the order to call AnalyzeUnusedPackages
  //first, since AnalyzeUnusedPackages needs info that is loaded here. We will just read it twice, it is fast.
  Result := TObjectList<TProjectDefinition>.Create;

  var Uninstaller := TProjectInstaller.Create(Config);
  try
    var ExistingProjects := TDictionary<string, boolean>.Create;
    try
      for var p in BuildInfo.ProjectList.All do
      begin
        ExistingProjects.AddOrSetValue(p.Application.Id, true);
      end;

      for var proj: IUninstallInfo in Uninstaller.GetAllProjects do
      begin
        if not ExistingProjects.ContainsKey(proj.ProjectId) then
        begin
          var TmpProj := TProjectDefinition.Create('');
          TmpProj.Application.Id := proj.ProjectId;
          TmpProj.NeedsCompiling := TPlatsCompiled.All;
          Result.Add(TmpProj);
        end;
      end;
    finally
      ExistingProjects.Free;
    end;
  finally
    Uninstaller.Free;
  end;
end;

function TProjectAnalyzer.DependenciesRebuilt(const Config: TConfigDefinition;
   const Project: TProjectDefinition): TPlatsCompiled;
begin
  // When we arrive here, all dependencies have been analyzed. We can safely check if they were compiled.
  var ProjectsToUninstall := GetUninstallList;
  try
    var NeedsRebuild := TPlatsCompiled.Create;
    ProjectList.LoopDependencies(
    procedure (aProj: TProjectDefinition)
    begin
      NeedsRebuild.AddCompiled(aProj.NeedsCompiling);
    end
    , Project, ProjectsToUninstall);

    Result := NeedsRebuild;
  finally
    ProjectsToUninstall.Free;
  end;
end;

procedure TProjectAnalyzer.AnalyzeOnePackage(const PackageCache: TPackageCache; const Project: TProjectDefinition;
              const dv: TIDEName; const dp: TPlatform; const Package: TPackage;
              const DepsCompiled: TPlatsCompiled; const ProductHash: TProductHash; const CppBuilderSupport: boolean;
              const PackagesFolder: string);
begin
  var PackageFileName := TPackageFinder.GetProjectToBuild(PackageCache, dv, Project, Package,
        Config.GetNaming(Project.Naming, Project.FullPath), true, nil);
  if (TPath.GetExtension(PackageFileName).ToLowerInvariant = '.cbproj') and not CppBuilderSupport then exit;

  if not TInstallerFactory.GetInstaller(dv).ProjectFileSupportsPlatform(Project.IgnoreDprojPlatforms, TPath.GetDirectoryName(Project.FullPath), PackageFileName, dp) then exit;

  //See https://github.com/tmssoftware/tms-smartsetup/issues/225#issuecomment-2607769203
  // Basically, we can have "Design", "Runtime" and "Design-runtime" packages
  // Thus, current logic is:
  //  * For compiling: If package **is** flagged for design-time, and **is not** flagged for runtime (a real design package)
  //    neither is an exe (unlikely combination), then do not compile for platforms where the IDE isn't available.
  //  * For registering: If package **is** flagged for design-time, and is not an exe, and it is one of the plaforms the IDE
  //    is available, then register it.

  var PackageNeedsRegistering := false;
  if Package.IsDesign and (Package.PackageType <> TPackageType.Exe) then
  begin
    if (dp in TInstallerFactory.GetInstaller(dv).PlatformsForDesign(Project.Application.Id)) then
    begin
      PackageNeedsRegistering := true;
    end
    else if not Package.IsRuntime then exit;
  end;

  if FileHasher.ProductModified(ProductHash, BuildInfo.CurrentProject.Project, Config, dv, dp, '')
    or (dp in DepsCompiled[dv]) then //we check at project level, not package.
  begin
    BuildInfo.CurrentProject.AddBuildInfo(dv, TPackageBuildInfo.Create(
        Package,
        PackageFileName,
        PackageNeedsRegistering,
        dv,
        dp,
        CppBuilderSupport));
    BuildInfo.CurrentProject.Project.NeedsCompiling.Add(dv, dp);
  end else
  begin
    BuildInfo.CurrentProject.Skipped.Add(dv, dp);
  end;
end;

constructor TProjectAnalyzer.Create(const aConfig: TConfigDefinition; const aProjectList: TProjectList; const aFileHasher: TFileHasher);
begin
  inherited Create;
  FBuildInfo := TBuildInfo.Create(aProjectList);
  Config := aConfig;
  ProjectList := aProjectList;
  FileHasher := aFileHasher;
  FProjectFinder := THashSet<string>.Create;
  for var Project in aProjectList.All do FProjectFinder.Add(Project.Application.Id);

end;

destructor TProjectAnalyzer.Destroy;
begin
  FProjectFinder.Free;
  BuildInfo.Free;
  inherited;
end;

procedure TProjectAnalyzer.AnalyzeUnusedPackages;
begin
  var Uninstaller := TProjectInstaller.Create(Config);
  try
    var ExistingProjects := TDictionary<string, boolean>.Create;
    try
      for var p in BuildInfo.ProjectList.All do
      begin
        ExistingProjects.AddOrSetValue(p.Application.Id, true);
      end;

      for var proj: IUninstallInfo in Uninstaller.GetAllProjects do
      begin
        if not ExistingProjects.ContainsKey(proj.ProjectId) then
        begin
          BuildInfo.ProjectsToUninstall.Add(proj);
          continue;
        end;

        for var ide: IUninstallInfo in Uninstaller.GetAllIDEs(proj.ProjectId) do
        begin
          var ProjectBI: TProjectBuildInfo;
          if not BuildInfo.ProjectToProjectBuildInfo.TryGetValue(proj.ProjectId, ProjectBI) then break;
          var IDEBI: TIDEBuildInfo;
          var HasSkipped: boolean;
          if not ProjectBI.ContainsIDE(ide.IDE, IDEBI, HasSkipped) then
          begin
            if not HasSkipped then ProjectBI.IDEsToUninstall.Add(ide);
          end;

          for var platform: IUninstallInfo in Uninstaller.GetAllPlatforms(proj.ProjectId, ide.ide) do
          begin
            var PlatformBI: TPlatformBuildInfo;
            if (IDEBI = nil) or not IDEBI.ContainsPlatform(platform.Platform, PlatformBI) then
            begin
              if not ProjectBI.Skipped.ContainsPlatform(ide.IDE, platform.Platform)
                 then ProjectBI.PlatformsToUninstall.Add(ide.IDE, platform);
            end;

            //see packages.
          end;
        end;
      end;
    finally
      ExistingProjects.Free;
    end;
  finally
    Uninstaller.Free;
  end;
end;

end.

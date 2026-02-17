unit Deget.Compilation.DelphiSelector;
{$IFDEF MSWINDOWS}

interface
uses
  Classes, SysUtils,
  Deget.CoreTypes,
  Deget.Compilation,
  UProjectDefinition,
  UConfigDefinition,
  UProjectBuildInfo,
  Deget.IDEInfo,
  Deget.PackageConfig;


  // Most straightforward and flexible way to compile a Delphi application
// Based on the type of Settings (either tms build or TDcc32 compilation settings), it will
// compile the Delphi .dproj (or .dpk) using the property compiler (dcc32 or msbuid).
procedure DelphiCompile(const ProjectFile: string; IDEName: TIDEName; Settings: TCompilationSettings; const ATempFolder: string = '');


function CreateDegetPackageCompilationSettings(BuildInfo: TProjectBuildInfo; PlatformInfo: IDelphiPlatformInfo;
  PackageInfo: IDelphiPackageInfo; IDEName: TIDEName; const BuildConfig: string; CBuilder: boolean): TCompilationSettings;


implementation
uses
  IOUtils,
  Deget.Compilation.Delphi,
  Deget.Compilation.Precompiled,
  Commands.GlobalConfig,
  Testing.Globals,
  Deget.IDEUtils;

function GetLibraryPaths(const RootFolder: string; const ExtraPaths: TCompilerPaths; const Plat: TPlatform): string;
begin
  var Paths := TCompilerPathsPerPlatform.Create;
  try
    for var ex in ExtraPaths.LibraryPathsBuildAndRegister do
    begin
      Paths.LibraryPathsBuildAndRegister.Add(TProjectBuildInfo.GetPlatformPaths(RootFolder, ex));
    end;
    for var ex in ExtraPaths.LibraryPathsBuildOnly do
    begin
      Paths.LibraryPathsBuildOnly.Add(TProjectBuildInfo.GetPlatformPaths(RootFolder, ex));
    end;

    Result := Paths.GetLibraryPaths(Plat, true);
  finally
    Paths.Free;
  end;

end;

function InvalidCompilers(const FileName: string): boolean;
begin
{$IFDEF DEBUG}
  if TestParameters.DelphiCE then exit(true);

{$ENDIF}
  //We could check if running the cmd returns the message that command line compiling is not available too.
  //But that would have to take in account language translations.
  //What we can't check is the error code, as it is always 0.
  Result := TFile.GetSize(FileName) < 100000;

  //Check for IDE Fix Pack: https://github.com/tmssoftware/tms-smartsetup/issues/305
  if Result then
  begin
    var fastdcc := (TPath.Combine(TPath.GetDirectoryName(FileName), TPath.GetFileNameWithoutExtension(FileName) + 'compiler' + TPath.GetExtension(FileName)));
    if TFile.Exists(fastdcc)
      then exit(false);
  end;
end;

function CreateDegetPackageCompilationSettings(BuildInfo: TProjectBuildInfo;
  PlatformInfo: IDelphiPlatformInfo; PackageInfo: IDelphiPackageInfo; IDEName: TIDEName;
  const BuildConfig: string; CBuilder: boolean): TCompilationSettings;
var
  Dcc32: TDCC32CompilationSettings;
  MsBuild: TMsBuildCompilationSettings;
//  Dep: TDependency;
  ExtraPath: string;
//  DependencyProject: TProjectDefinition;
  DepPackInfo: IDelphiPackageInfo;
  Dependency: TProjectBuildInfo;
  Project: TProjectDefinition;
begin
  if PackageInfo.IsPrecompiled then
  begin
    var Precompiled := TPrecompiledCompilationSettings.Create;

    //Hardcoded for now. We could make this configurable in the yaml, but I am not sure why we would need to.
    Precompiled.IDEName := PlatformInfo.IDEInfo.IDEName;
    Precompiled.Platform := PlatformInfo.PlatType;
    Precompiled.PrecompiledSource := TPath.Combine(TPath.GetDirectoryName(BuildInfo.Project.FullPath), 'BinPackages');
    Precompiled.HasMultiIDEPackages := BuildInfo.Project.HasMultiIDEPackages;
    Result := Precompiled;
  end else
  begin
    if IDEName <= delphi2009 then
    begin
      Dcc32 := TDcc32CompilationSettings.Create;
      Dcc32.IsDpr := BuildInfo.Project.IsExe;
      Dcc32.SearchPathMode := TSearchPathMode.DelphiLib;
      Dcc32.BplOutputDir := TPath.GetDirectoryName(PackageInfo.BinaryTempPackageFileName(BuildInfo.ProjectId, Config.Folders.ParallelFolder, BuildConfig));
      Dcc32.DcuOutputDir := PackageInfo.ExpandedTempDcuOutputDir(BuildInfo.ProjectId, Config.Folders.ParallelFolder, BuildConfig);
      Dcc32.DcpOutputDir := Dcc32.DcuOutputDir;
      Dcc32.ObjOutputDir := Dcc32.DcuOutputDir;
      Dcc32.HppOutputDir := Dcc32.DcuOutputDir;
      Dcc32.ExeOutputDir := PackageInfo.ExpandedTempExeOutputDir(BuildInfo.ProjectId, Config.Folders.ParallelFolder, BuildConfig);
      Dcc32.OriginalDpkPath := PackageInfo.OrigPackageDirectory;
      if CBuilder then
        Dcc32.CBuilderOutput := true;

      Result := Dcc32;
    end
    else
    begin
      if InvalidCompilers(PlatformInfo.IDEInfo.Dcc32File) then //Community edition. Has dcc32, but they are invalid.
      begin
        Result := TBdsCompilationSettings.Create;
        TBdsCompilationSettings(Result).BplFolder := BuildInfo.BplFolder;
      end
      else
      begin
        MsBuild := TMsBuildCompilationSettings.Create;
        MsBuild.SearchPathMode := TSearchPathMode.DelphiLib;
        MsBuild.PackageLibraryPath := PackageInfo.LibraryPath(BuildInfo.ProjectId, Config.Folders.ParallelFolder,  BuildConfig);
        MsBuild.IgnoreDefines := true; //to avoid undefining existing ones. See https://github.com/tmssoftware/tms-smartsetup/issues/261
        Result := MsBuild;
      end;
    end;
  end;

  Result.TargetConfig := BuildConfig;

  if Result is TDelphiCompilationSettings then
  begin
    TDelphiCompilationSettings(Result).TargetPlatform := PlatformInfo;
  end;

  // We need to add our own dcp output dir to search path so we can compile packages that depend on each other.
  // We also need to do the same for library dependencies.
  // (for example, a design-time package "dclproduct" that depends on runtime package "product",
  // or, for example, xdata.dproj package which depends on aurelius.dproj package)
  ExtraPath := '';

  for Dependency in BuildInfo.Dependencies do
  begin
    Project := Dependency.Project;
    var DepBuildConfig := BuildConfig;
    if not Dependency.DebugDCUs then DepBuildConfig := BuildConfigs[TBuildConfig.Release];


    // There is no need to loop over all packages here. They all have the same package folder, which is what we want to find out.
    DepPackInfo := TPackageConfig.Create('DummyPackName', PlatformInfo, Dependency.PackagesFolder(PlatformInfo.IDEInfo.IDEName), Project.IsExe, '.dummyext', Project.LibSuffixes, Project.HasMultiIDEPackages);

    //Dependencies have already been moved to their final destinations.
    ExtraPath := AddPaths(ExtraPath, TPath.GetDirectoryName(DepPackInfo.ExpandedDcpFileName(DepBuildConfig)));

    // if Linux or macOS64 then we must also add the directory where compiled bpl is located
    if PlatformInfo.PlatType in [Linux64, macOS64Intel] then
      ExtraPath := AddPaths(ExtraPath, TPath.GetDirectoryName(DepPackInfo.BinaryPackageFileName(DepBuildConfig)));

    ExtraPath := AddPaths(ExtraPath, GetLibraryPaths(Project.RootFolder, Project.ExtraPaths, PlatformInfo.PlatType));

  end;

  ExtraPath := AddPaths(ExtraPath, BuildInfo.ExtraPaths.GetLibraryPaths(PlatformInfo.PlatType, true));
  Project := BuildInfo.Project;

  DepPackInfo := TPackageConfig.Create('DummyPackName'
  , PlatformInfo, PackageInfo.OrigPackageDirectory, Project.IsExe, '.dummyext', Project.LibSuffixes, Project.HasMultiIDEPackages);

  ExtraPath := AddPaths(ExtraPath, TPath.GetDirectoryName(DepPackInfo.ExpandedTempDcpFileName(Project.Application.Id, Config.Folders.ParallelFolder, BuildConfig)));

  // if Linux or macOS64 then we must also add the directory where compiled bpl is located
  if PlatformInfo.PlatType in [Linux64, macOS64Intel] then
    ExtraPath := AddPaths(ExtraPath, TPath.GetDirectoryName(DepPackInfo.BinaryTempPackageFileName(Project.Application.Id, Config.Folders.ParallelFolder, BuildConfig)));

  Result.ExtraSearchPath := ExtraPath;
  Result.SearchPathsToPreserve := Project.SearchPathsToPreserve;
  Result.ExtraCompilerParameters := Config.CompilerParameters(Project.Application.Id, IDEName);
end;

procedure DelphiCompile(const ProjectFile: string; IDEName: TIDEName; Settings: TCompilationSettings;
  const ATempFolder: string = '');
begin
  if Settings is TMsBuildCompilationSettings then
    TMsBuildCompiler.Build(ProjectFile, IDEName, Settings as TMsBuildCompilationSettings, ATempFolder)
  else
  if Settings is TDcc32CompilationSettings then
    TDcc32Compiler.Build(ProjectFile, IDEName, Settings as TDcc32CompilationSettings, ATempFolder)
  else
  if Settings is TBdsCompilationSettings then
    TBdsCompiler.Build(ProjectFile, IDEName, Settings as TBdsCompilationSettings, ATempFolder)
  else
  if Settings is TPrecompiledCompilationSettings then
    TPrecompiledCompiler.Build(ProjectFile, IDEName, Settings as TPrecompiledCompilationSettings)

  else raise Exception.CreateFmt('Compilation settings "%s" not supported', [Settings.ClassName]);
end;

{$ELSE}
interface
implementation
{$ENDIF}
end.

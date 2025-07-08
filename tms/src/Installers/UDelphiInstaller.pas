unit UDelphiInstaller;
{$i ../../tmssetup.inc}

interface

uses
  UPlatformBuildInfo, UPackageBuildInfo, Deget.CoreTypes,
  UConfigDefinition, UFullBuildInfo, ULogger, UMultiLogger, UProjectDefinition,
{$IFDEF MSWINDOWS}
  Windows, SysUtils, TypInfo, JSON, IOUtils, Generics.Collections,
  Deget.IDETypes, Deget.Compilation, Deget.IDEInfo, Deget.PackageConfig,
{$ENDIF}
  UUninstallInfo, UInstaller, Classes, Megafolders.Definition;

{$IFDEF MSWINDOWS}
type
  TPackagesConsolidation = record
    DebugDcuOutputDir: string;
    DebugDcpOutputDir: string;
    DcuOutputDir: string;
    DcpOutputDir: string;
    BplOutputDir: string;
    CppBpiOutputDir: string;
    CppHppOutputDir: string;
    CppObjOutputDir: string;
    BrowsingPath: string;
    DoCBuilder: Boolean;
    class function Init: TPackagesConsolidation; static;
  end;

  TDelphiInstaller = class(TInstaller)
  strict private
    function CreateIDEInfo(BuildInfo: TFullBuildInfo): IDelphiIDEInfo; overload;
    function CreateIDEInfo(UninstallInfo: IUninstallInfo): IDelphiIDEInfo; overload;
    function ConsolidatePackages(BuildInfo: TFullBuildInfo; PlatformInfo: IDelphiPlatformInfo; const InMegafolder: boolean): TPackagesConsolidation;
    procedure AppendConsolidation(PackageInfo :IDelphiPackageInfo; var Consolidation: TPackagesConsolidation; DebugDCUs, SupportsCppBuilder, InMegafolder: Boolean);
    procedure RegisterPackage(BuildInfo: TFullBuildInfo; PackBuildInfo: TPackageBuildInfo; PackUninstall: TJSONObject);
    procedure LinkPackageFile(const ProductId, LinkedFileName, BinaryPackageFileName: string; PackUninstall: TJSONObject);
    function AddPathOverride(IDEInfo: IDelphiIDEInfo; const AlternateRegistryKey, Folder: string): boolean;
    procedure CopyProjects(const Orig, Temp, OutputDir, ExeOutputDir: string; IDEName: TIDEName; const IsExe: boolean;
              const Application: TApplicationDefinition; const AddLibSuffix: boolean; const Brcc32Path: string);
    procedure UpdatePackageSource(const Orig, OutputDir: string; IDEName: TIDEName);
    procedure RegisterHelp(const ProductName, HelpFile: string; IDEName: TIDEName; const Config: TConfigDefinition; const UninstallInfo: IUninstallInfo);
    function UnRegisterHelp(const UninstallInfo: IUninstallInfo): boolean;
    procedure CopyProjectRes(const SourceProj, TargetFolder: string;
      const IDEName: TIDEName; const Application: TApplicationDefinition; const Brcc32Path: string);
    procedure _BuildPackage(const BuildConfig: string;
      const BuildInfo: TFullBuildInfo; UninstallInfo: IUninstallInfo;
      PackageInfo: IDelphiPackageInfo; const Package: TPackage; PlatformInfo: IDelphiPlatformInfo; const DoCBuilder: boolean);
    function CaptureBuild(BuildConfig: TBuildConfig; Proc: TProc<TBuildConfig>): TProc;
  private
    function IsXMLProj(const FileName: string): boolean;
    function HasDesignInfo64(const ProductId: string): boolean;
  public
    destructor Destroy; override;
    function DllSuffix: string; virtual; abstract; //Used in the registry, doesn't matter if LibSuffix in a package is different.
    function PlatformsForDesign(const ProductId: string): TPlatformSet; override;
    function SupportsCppBuilder(const platform: TPlatform): boolean; override;

    procedure Build(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo); override;
    procedure CleanAllBuildTemporaryFiles(const UninstallInfo: IUninstallInfo); override;

    procedure RegisterAtIDELevel(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo); override;
    procedure RegisterAtPlatformLevel(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo); override;

    procedure UnRegisterAtIDELevel(const UninstallInfo: IUninstallInfo); override;
    procedure UnRegisterAtPlatformLevel(const UninstallInfo: IUninstallInfo); override;

    procedure UpdateProjectsSource(const BuildInfo: TFullBuildInfo); override;

    procedure UpdateMegafolders(const SourceFolder, ProjectId: string;
      const IDEName: TIDEName; const Platform: TPlatform;
      const BuildConfig: TBuildConfig;
      const UsedDcuMegafolders: TUsedMegafolders); override;

    procedure CreateTempProjects(const BuildInfo: TFullBuildInfo); override;
    procedure MoveDataFromTempProjects(const BuildInfo: TFullBuildInfo; const UsedDcuMegafolders: TUsedMegafolders); override;
    procedure RemoveTempProjects(const BuildInfo: TFullBuildInfo); override;
    function ProjectFileSupportsPlatform(const IgnoreDprojPlatforms: boolean; const RootFolder, PackageFileName: string; const dp: TPlatform): boolean; override;
  end;

  TDelphiIDEPaths = array[TDelphiPathType] of string;

  UninstallConsts = class
  public const
    // Platform level
    AlternateRegistryKey = 'AlternateRegistryKey';
    Packages = 'Packages';
    IDEPaths: TDelphiIDEPaths = (
      'LibraryPath',      // ptLibraryPath,
      'BrowsingPath',     // ptBrowsingPath,
      'DebugDCUPath',     // ptDebugDcuPath,
      'CppLibraryPath',   // ptCppLibraryPath
      'CppIncludePath',   // ptCppIncludePath
      'CppBrowsingPath',   // ptCppBrowsingPath
      'CppClang32LibraryPath',   // ptCppClang32LibraryPath
      'CppClang32IncludePath',   // ptCppClang32IncludePath
      'CppClang32BrowsingPath'   // ptCppClang32BrowsingPath
    );

    // Package level
    LinkedPackageFile = 'LinkedPackageFile';
    Registered = 'Registered';

    // Build level
    FilesToDelete = 'FilesToDelete';
    FileMasksToDelete = 'FileMasksToDelete';
    RootFolder = 'RootFolder';

    //IDE Level
    HelpRegistryKeyName = 'HelpRegistryKeyName';
    HelpRegistryValueName = 'HelpRegistryValueName';
  end;
{$ELSE}
type
  TDelphiInstaller = class(TInstaller)
  public
    function DllSuffix: string; virtual; abstract;
    procedure Build(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo); override;
    procedure CleanAllBuildTemporaryFiles(const UninstallInfo: IUninstallInfo); override;
    function PlatformsForDesign(const ProductId: string): TPlatformSet; override;
    function SupportsCppBuilder(const platform: TPlatform): boolean; override;

    procedure RegisterAtIDELevel(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo); override;
    procedure RegisterAtPlatformLevel(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo); override;

    procedure UnRegisterAtIDELevel(const UninstallInfo: IUninstallInfo); override;
    procedure UnRegisterAtPlatformLevel(const UninstallInfo: IUninstallInfo); override;
    procedure UpdateProjectsSource(const BuildInfo: TFullBuildInfo); override;
    procedure CreateTempProjects(const BuildInfo: TFullBuildInfo); override;
    procedure RemoveTempProjects(const BuildInfo: TFullBuildInfo); override;
    function ProjectFileSupportsPlatform(const IgnoreDprojPlatforms: boolean; const RootFolder, PackageFileName: string; const dp: TPlatform): boolean; override;
    procedure MoveDataFromTempProjects(const BuildInfo: TFullBuildInfo; const UsedDcuMegafolders: TUsedMegafolders); override;


  end;
{$ENDIF}

implementation
{$IFDEF MSWINDOWS}
uses
  Deget.DelphiInfo, Deget.IDEUtils, Deget.FileUtils, Deget.RCFileFormat,
  Deget.Filer.DprojFile, Deget.Filer.DpkFile, Deget.Filer.DprFile, UIDEUtils,
  Deget.Filer.ProjectFactory, UEnvironmentPath, Megafolders.Manager,
  UWindowsPath, UTmsBuildSystemUtils, Commands.GlobalConfig, Deget.ResFile, Threading, UOSFileLinks, ULoggerTask;

{ TDelphiInstaller }

function TDelphiInstaller.AddPathOverride(IDEInfo: IDelphiIDEInfo;
  const AlternateRegistryKey, Folder: string): boolean;
begin
  var Lock := GetEnvironmentOptionsLock;
  try
    Lock.Acquire;
    try
      Result := false;
      //see https://github.com/tmssoftware/tms-smartsetup/issues/217
      var SystemPath := IDEInfo.GetPathOverride;

      // check if $(PATH) is already in PATH system environment variable.
      // If it is, we don't touch it, as we modified the Windows Path anyway.
      if SystemPath <> AddPaths(SystemPath, '$(PATH)') then
      begin
        if SystemPath <> AddPaths(SystemPath, Folder) then
        begin
          StoreDelphiEnviromnentOverrides(IDEInfo.IDEName, AlternateRegistryKey, Folder);
          IDEInfo.AddFolderToPathOverride(Folder);
          Result := true;
        end;
      end;
    finally
      Lock.Release;
    end;
  finally
    Lock.Free;
  end;
end;


procedure TDelphiInstaller.AppendConsolidation(PackageInfo: IDelphiPackageInfo; var Consolidation: TPackagesConsolidation;
  DebugDCUs, SupportsCppBuilder, InMegafolder: boolean);
begin
  Consolidation.BplOutputDir := AddPaths(Consolidation.BplOutputDir, TPath.GetDirectoryName(PackageInfo.BinaryPackageFileName('Release')));
  Consolidation.BrowsingPath := AddPaths(Consolidation.BrowsingPath, PackageInfo.SourcePathFromDpk);

  if InMegafolder then exit;

  if DebugDCUs then
  begin
    Consolidation.DebugDcuOutputDir := AddPaths(Consolidation.DebugDcuOutputDir, PackageInfo.ExpandedDcuOutputDir('Debug'));
    Consolidation.DebugDcpOutputDir := AddPaths(Consolidation.DebugDcpOutputDir, TPath.GetDirectoryName(PackageInfo.ExpandedDcpFileName('Debug')));
  end;

  Consolidation.DcuOutputDir := AddPaths(Consolidation.DcuOutputDir, PackageInfo.ExpandedDcuOutputDir('Release'));
  Consolidation.DcpOutputDir := AddPaths(Consolidation.DcpOutputDir, TPath.GetDirectoryName(PackageInfo.ExpandedDcpFileName('Release')));

  if SupportsCppBuilder then
  begin
    Consolidation.DoCBuilder := True;
    Consolidation.CppBpiOutputDir := AddPaths(Consolidation.CppBpiOutputDir, PackageInfo.ExpandedCppBpiOutputDir('Release'));
    Consolidation.CppHppOutputDir := AddPaths(Consolidation.CppHppOutputDir, PackageInfo.ExpandedCppHppOutputDir('Release'));
    Consolidation.CppObjOutputDir := AddPaths(Consolidation.CppObjOutputDir, PackageInfo.ExpandedCppObjOutputDir('Release'));
  end;
end;

function ExcludeDefine(const Define: string; const PackageExtraDefines: TList<string>): boolean;
begin
  Result := false;
  for var d in PackageExtraDefines do
  begin
    if not d.StartsWith('-') then continue;
    if (SameText(Define.Trim(), d.Substring(1).Trim)) then exit(true);
  end;
end;

function ProcessDefines(const DefineString: string; const PackageExtraDefines: TList<string>): string;
begin
  if PackageExtraDefines = nil then exit(DefineString); //most common case.
  var SplitDefines := DefineString.Split([';'], TStringSplitOptions.ExcludeEmpty);
  Result := '';
  for var Extra in PackageExtraDefines do
  begin
    if Extra.StartsWith('-') then continue;
    if Result <> '' then Result := Result + ';';
    Result := Result + Extra;
  end;
  for var Def in SplitDefines do
  begin
    if ExcludeDefine(Def, PackageExtraDefines) then continue;
    Result := Result + ';' + Def;
  end;
end;

procedure TDelphiInstaller._BuildPackage(const BuildConfig: string; const BuildInfo: TFullBuildInfo;
     UninstallInfo: IUninstallInfo; PackageInfo: IDelphiPackageInfo; const Package: TPackage;
     PlatformInfo: IDelphiPlatformInfo; const DoCBuilder: boolean);
begin
  try
  //    if (Lib.InstallMode = TLibraryInstallMode.BuildSource) and not AddSourceCodeToLibraryPath then Exit;

    //This block of code is mainly needed so tms build can build itself. But it doesn't hurt that we try to remove the binary
    //before compiling. It might have been locked because we started delphi after we deleted the files before.
    var DummyNeedsToRestartIDE := false;

    //The precompiled compiler, as it is now, will copy all the files once the first time it is called.
    //If we remove the package here, it will keep being removed.
    if not PackageInfo.IsPrecompiled then
    begin
      DeleteFileOrMoveToLocked(UninstallInfo.LockedFilesFolder ,
        PackageInfo.BinaryTempPackageFileName(BuildInfo.Project.ProjectId, Config.Folders.ParallelFolder, BuildConfig),
        UninstallInfo.DryRun, procedure (s: string) begin Logger.Trace(s); end, DummyNeedsToRestartIDE);
    end;
    if DummyNeedsToRestartIDE and not BuildInfo.Project.Project.IsExe then NeedsToRestartIDE := true;


    Logger.Progress(Format('Building package %s for "%s.%s" with %s config.', [Package.Name, DisplayName, PlatformId[PlatformInfo.PlatType], BuildConfig]), BuildInfo.Project.Progress);
    var Settings := CreateDegetPackageCompilationSettings(BuildInfo.Project, PlatformInfo, PackageInfo, IDEName, BuildConfig, DoCBuilder);
    try
      Settings.Defines := String.Join(';', BuildInfo.Project.Defines);
      var PkgDefines := PackageInfo.Defines(BuildConfig);
      if PkgDefines <> '' then Settings.Defines := ProcessDefines(PkgDefines, BuildInfo.Project.Project.PackageExtraDefines) + ';' + Settings.Defines;
      

      if not BuildInfo.Project.DryRun then
      begin
        var TempProjFile := PackageInfo.TempPackageFileName(BuildInfo.Project.ProjectId, Config.Folders.ParallelFolder, BuildConfig);
        DelphiCompile(TempProjFile, IDEName, Settings, BuildInfo.Project.CompileTempFolder);
      end;
    finally
      Settings.Free;
    end;
    Logger.Trace(Format('Package %s built successfully.', [Package.Name]));
  finally
    BuildInfo.Project.Progress.DoAtomicIncrement;
  end;
end;

function TDelphiInstaller.CaptureBuild(BuildConfig: TBuildConfig; Proc: TProc<TBuildConfig>): TProc;
begin
  Result := procedure begin Proc(BuildConfig); end;
end;

procedure TDelphiInstaller.Build(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo);
var
  PackageInfo: IDelphiPackageInfo;
  DoCBuilder: boolean;
  Package: TPackage;
  PlatformInfo: IDelphiPlatformInfo;


begin
  PlatformInfo := CreateIDEInfo(BuildInfo).GetPlatform(BuildInfo.Platform.Name);
  Package := BuildInfo.Package.Package;
  PackageInfo := TPackageConfig.Create(Package.Name, PlatformInfo, BuildInfo.Platform.PackagesFolder, BuildInfo.Project.Project.IsExe,  BuildInfo.Package.PackageExt, BuildInfo.Project.Project.LibSuffixes, BuildInfo.Project.Project.HasMultiIDEPackages);
  DoCBuilder := BuildInfo.Package.SupportsCppBuilder;

  // Build binaries
  if BuildInfo.Project.DebugDCUs then
  begin
    var Configs: array[TBuildConfig] of ITask;

    for var BuildConfig := Low(TBuildConfig) to High(TBuildConfig) do
    begin
      Configs[BuildConfig] := RunTask(
      CaptureBuild(BuildConfig,
      procedure (BC: TBuildConfig)
      begin
        _BuildPackage(BuildConfigs[BC], BuildInfo, UninstallInfo, PackageInfo, Package, PlatformInfo, DoCBuilder);
      end));
    end;

    TTask.WaitForAll(Configs);
  end else
  begin
    _BuildPackage(BuildConfigs[TBuildConfig.Release], BuildInfo, UninstallInfo, PackageInfo, Package, PlatformInfo, DoCBuilder);
  end;



  var HasErrors := true;
  Logger.StartSection(TMessageType.Register, 'Copy to output folder');
  try
    // Copy resources to output dir
    var PackageDirs := TPackagesConsolidation.Init;
    AppendConsolidation(PackageInfo, PackageDirs, BuildInfo.Project.DebugDCUs, BuildInfo.Package.SupportsCppBuilder, false);

    var FilesToDelete: TArray<string>;
    var FileMasksToDelete: TArray<string>;

    // Copy resources (.dfm, .fmx, .res) to output folder so compiler finds it.
    // If we are adding the source code to library path, then we don't need to do this
    if not BuildInfo.Project.Project.IsExe and not BuildInfo.Project.AddSourceCodeToLibraryPath then
    begin
      //var DestinationDir := PackageDirs.DcuOutputDir;
      var TempFolder := PackageInfo.TempPackageDirectory(BuildInfo.Project.ProjectId, Config.Folders.ParallelFolder, BuildConfigs[TBuildConfig.Release]);
      var DestinationDir := TPath.Combine(TempFolder, PlatformInfo.PlatformMacroValue, BuildConfigs[TBuildConfig.Release]);

      var ExtraBrowsingPath := BuildInfo.Project.ExtraPaths.GetBrowsingPaths(PlatformInfo.PlatType);
      for var ResourceFileMask in ['*.dfm', '*.fmx', '*.res'] do
      begin
        for var SourceDir in GetPaths(AddPaths(PackageDirs.BrowsingPath, ExtraBrowsingPath)) do
        begin
          if SourceDir = DestinationDir then Continue;
          var SourceFiles := TPath.Combine(SourceDir, ResourceFileMask);
          Logger.Trace(Format('Copying "%s" to "%s".', [SourceFiles, DestinationDir]));
          if not BuildInfo.Project.DryRun then
            CopyFilesToDirectory(SourceFiles, DestinationDir);
        end;
        if not BuildInfo.Project.DryRun then
          FileMasksToDelete := FileMasksToDelete + [TPath.Combine(PackageDirs.DcuOutputDir, ResourceFileMask)]; //we will remove them from the final folder, not the temp.
      end;
    end;

    // Save uninstall info
  //  if (Lib.InstallMode = TLibraryInstallMode.BuildSource) and not AddSourceCodeToLibraryPath then
    if not BuildInfo.Project.DryRun then
    begin
      for var BuildConfig := Low(TBuildConfig) to High(TBuildConfig) do
      begin
        FilesToDelete := FilesToDelete + [PackageInfo.BinaryPackageFileName(BuildConfigs[BuildConfig])];
        FilesToDelete := FilesToDelete + [TPath.ChangeExtension(PackageInfo.BinaryPackageFileName(BuildConfigs[BuildConfig]), '.rsm')];
        FilesToDelete := FilesToDelete + [PackageInfo.ExpandedDcpFileName(BuildConfigs[BuildConfig])];
      end;

      FileMasksToDelete := FileMasksToDelete + [TPath.Combine(PackageDirs.DcuOutputDir, '*.dcu')];
      FileMasksToDelete := FileMasksToDelete + [TPath.Combine(PackageDirs.DebugDcuOutputDir, '*.dcu')];
      FileMasksToDelete := FileMasksToDelete + [TPath.Combine(PackageDirs.DebugDcuOutputDir, '*.res')];

      //Even in packages not directly supporting C++ builder, like FMX_FlexCel_Core
      // bpi and .a files can be produced. We will remove them anyway.
      //if DoCBuilder then
      begin
        for var BuildConfig := Low(TBuildConfig) to High(TBuildConfig) do
        begin
          FilesToDelete := FilesToDelete + [TPath.ChangeExtension(PackageInfo.ExpandedDcpFileName(BuildConfigs[BuildConfig]), '.bpi')];
          FilesToDelete := FilesToDelete + [PackageInfo.ExpandedLibFileName(BuildConfigs[BuildConfig])];
        end;

        FileMasksToDelete := FileMasksToDelete + [TPath.Combine(PackageDirs.CppObjOutputDir, '*.obj')];
        FileMasksToDelete := FileMasksToDelete + [TPath.Combine(PackageDirs.CppObjOutputDir, '*.o')];
        FileMasksToDelete := FileMasksToDelete + [TPath.Combine(PackageDirs.CppHppOutputDir, '*.hpp')];
        FileMasksToDelete := FileMasksToDelete + [TPath.Combine(PackageDirs.CppHppOutputDir, '*.h')];

        // Todo: this should be "CppHppDebugOutputDir" and "CppObjDebugOutputDir"
        FileMasksToDelete := FileMasksToDelete + [TPath.Combine(PackageDirs.DebugDcuOutputDir, '*.hpp')];
      end;

      var Info := UninstallInfo.Value;
      var JFilesToDelete := TJSONArray.Create;
      var JFileMasksToDelete := TJSONArray.Create;

      Info.WriteStr(UninstallConsts.RootFolder, BuildInfo.Project.Project.RootFolder);
      Info.AddPair(UninstallConsts.FilesToDelete, JFilesToDelete);
      Info.AddPair(UninstallConsts.FileMasksToDelete, JFileMasksToDelete);
      for var FileToDelete in FilesToDelete do
        JFilesToDelete.Add(FileToDelete);
      for var FileMaskToDelete in FileMasksToDelete do
        JFileMasksToDelete.Add(FileMaskToDelete);
    end;
    HasErrors := false;
  finally
    Logger.FinishSection(TMessageType.Register, HasErrors);
  end;

end;

procedure TDelphiInstaller.CleanAllBuildTemporaryFiles(
  const UninstallInfo: IUninstallInfo);

  procedure _DeleteFileMask(const FileMask: string);
  begin
    if FileMask = '' then Exit;
    var Dir := TPath.GetDirectoryName(FileMask);
    var Mask := TPath.GetFileName(FileMask);

    if TDirectory.Exists(Dir) then
    begin
      if not UninstallInfo.DryRun then
        DeleteFilesFromDirectory(Dir, Mask, Config.Folders.LockedFilesFolder);
      Logger.Trace(Format('Deleting "%s" files from "%s".', [Mask, Dir]));
    end;
  end;

  procedure _DeleteDir(Dir, RootFolder: string);
  begin
    if Dir = '' then Exit;
    Dir := TPath.GetFullPath(Dir);
    while TDirectory.Exists(Dir) and TDirectory.IsEmpty(Dir) and Dir.StartsWith(RootFolder) do
    begin
      if not UninstallInfo.DryRun then
        TDirectory.Delete(Dir);
      Logger.Trace(Format('Directory "%s" deleted.', [Dir]));
      Dir := TPath.GetFullPath(TDirectory.GetParent(Dir));
    end;
  end;

begin
  Logger.StartSection(TMessageType.Unregister, 'Clean Temp Files - ' + UninstallInfo.Package);
  try
    var Info := UninstallInfo.Value;
    var JValue: TJSONValue;
    var FoldersToRemove: TArray<string>;

    var RootFolder := Info.ReadStr(UninstallConsts.RootFolder);

    JValue := Info.GetValue(UninstallConsts.FilesToDelete);
    if (JValue <> nil) and (JValue is TJSONArray) then
      for var Item in TJSONArray(JValue) do
      begin
        DeleteFileOrMoveToLocked(UninstallInfo.LockedFilesFolder, Item.Value, UninstallInfo.DryRun, procedure (s: string) begin Logger.Trace(s); end, NeedsToRestartIDE);
        FoldersToRemove := FoldersToRemove + [TPath.GetDirectoryName(Item.Value)];
      end;

    JValue := Info.GetValue(UninstallConsts.FileMasksToDelete);
    if (JValue <> nil) and (JValue is TJSONArray) then
      for var Item in TJSONArray(JValue) do
      begin
        _DeleteFileMask(Item.Value);
        FoldersToRemove := FoldersToRemove + [TPath.GetDirectoryName(Item.Value)];
      end;

    for var Item in FoldersToRemove do
      _DeleteDir(Item, RootFolder);

    // Delete files generated by the build
  (*  for each package:
      RemoveSymLink
      WindowsPathDir
      end; *)
  finally
    Logger.FinishSection(TMessageType.Unregister, false);
  end;
end;

destructor TDelphiInstaller.Destroy;
begin
  inherited;
end;

procedure TDelphiInstaller.LinkPackageFile(const ProductId, LinkedFileName, BinaryPackageFileName: string; PackUninstall: TJSONObject);
begin
  CreateFileLink(Config.Folders.LockedFilesFolder, BinaryPackageFileName, LinkedFileName,
    Config.FileLinkType(ProductId), NeedsToRestartIDE);
end;

function TDelphiInstaller.ConsolidatePackages(BuildInfo: TFullBuildInfo; PlatformInfo: IDelphiPlatformInfo; const InMegafolder: boolean): TPackagesConsolidation;
var
  PackBuildInfo: TPackageBuildInfo;
  Package: TPackage;
  PackageInfo:  IDelphiPackageInfo;
begin
  Result := TPackagesConsolidation.Init;

  { Gather all output directories for all packages }
  for PackBuildInfo in BuildInfo.Platform.PackagesBuildInfo do
  begin
    Package := PackBuildInfo.Package;
    PackageInfo := TPackageConfig.Create(Package.Name, PlatformInfo, BuildInfo.Platform.PackagesFolder, BuildInfo.Project.Project.IsExe, PackBuildInfo.PackageExt, BuildInfo.Project.Project.LibSuffixes, BuildInfo.Project.Project.HasMultiIDEPackages);
    AppendConsolidation(PackageInfo, Result, BuildInfo.Project.DebugDCUs, PackBuildInfo.SupportsCppBuilder, InMegafolder);
  end;
end;

function TDelphiInstaller.CreateIDEInfo(BuildInfo: TFullBuildInfo): IDelphiIDEInfo;
begin
  Result := TDelphiIDEInfo.Create(IDEName, BuildInfo.Project.AlternateRegistryKey, BuildInfo.IDE.PathToCompiler);
end;

function TDelphiInstaller.CreateIDEInfo(UninstallInfo: IUninstallInfo): IDelphiIDEInfo;
begin
  Result := TDelphiIDEInfo.Create(IDEName,
    UninstallInfo.Value.ReadStr(UninstallConsts.AlternateRegistryKey),
    UninstallInfo.PathToCompiler);
end;

function TDelphiInstaller.HasDesignInfo64(const ProductId: string): boolean;
begin
  var DesignInfoFile := TPath.Combine(GetCompilerPath(ProductId, IDEName, Config), TPath.Combine('lib', 'win64', 'release', 'designide.dcp'));
  Result := TFile.Exists(DesignInfoFile);
  //if not Result then Logger.Trace('Skipped win64 ide because ' + DesignInfoFile + ' was not found.')
  //else Logger.Trace(DesignInfoFile + ' found, building for win64 IDE.');
end;

function TDelphiInstaller.PlatformsForDesign(const ProductId: string): TPlatformSet;
begin
  if (IDEName < TIDEName.delphi12) then exit([TPlatform.win32intel]);

  // Delphi 12 is an special case. It might be 12.2, and not support win64, or 12.3 and support it.
  // It doesn't matter if the user installed or not the Win64IDE, in 12.3 it is possible to compile win64 design packages.
  if (IDEName = TIDEName.delphi12) and not HasDesignInfo64(ProductId) then exit([TPlatform.win32intel]);


  Result := [TPlatform.win32intel, TPlatform.win64intel];
end;

procedure TDelphiInstaller.RegisterAtIDELevel(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo);
begin
  UninstallInfo.Value.WriteStr(UninstallConsts.AlternateRegistryKey, BuildInfo.Project.AlternateRegistryKey);
  if not BuildInfo.Project.SkipRegistering.Help then
  begin
    RegisterHelp(BuildInfo.Project.Project.Application.Name + ' Help', BuildInfo.IDE.HelpFile, BuildInfo.IDE.Name, BuildInfo.Project.Config, UninstallInfo);
    CreateIDEInfo(BuildInfo).ForceIDEUpdate;
  end;
end;

procedure TDelphiInstaller.RegisterPackage(BuildInfo: TFullBuildInfo;
  PackBuildInfo: TPackageBuildInfo; PackUninstall: TJSONObject);
var
  PackageInfo: IDelphiPackageInfo;
  BinaryPackageFileName: string;
  Package: TPackage;
  PlatformInfo: IDelphiPlatformInfo;
  IDEInfo: IDelphiIDEInfo;
begin
  IDEInfo := CreateIDEInfo(BuildInfo);
  PlatformInfo := IDEInfo.GetPlatform(BuildInfo.Platform.Name);
  Package := PackBuildInfo.Package;
  PackageInfo := TPackageConfig.Create(Package.Name, PlatformInfo, BuildInfo.Platform.PackagesFolder, BuildInfo.Project.Project.IsExe, PackBuildInfo.PackageExt, BuildInfo.Project.Project.LibSuffixes, BuildInfo.Project.Project.HasMultiIDEPackages);

  BinaryPackageFileName := PackageInfo.BinaryPackageFileName('Release');
//  if not BuildInfo.Project.DryRun then
//    PackUninstall.WriteStr(UninstallConsts.BinaryPackageFile, BinaryPackageFileName);

  // additional task for windows packages (make it reachable from Windows apps and IDE registration)
  if PlatformInfo.PlatType in [Win32Intel, Win64Intel] then   //Win64xIntel is not added to the Windows path.
  begin
    var LinkedFolder := TPath.Combine(BuildInfo.Project.BplFolder, PlatformInfo.PlatformMacroValue);
    var LinkedFileName := TPath.Combine(LinkedFolder, TPath.GetFileName(BinaryPackageFileName));
    if not TFile.Exists(BinaryPackageFileName) then
    begin
      raise Exception.Create('Can''t find the package ' + BinaryPackageFileName + '. It likely has wrong or no LIBSUFFIX.');
    end;

    if not BuildInfo.Project.DryRun then
    begin
      LinkPackageFile(BuildInfo.Project.ProjectId, LinkedFileName, BinaryPackageFileName, PackUninstall);
      PackUninstall.WriteStr(UninstallConsts.LinkedPackageFile, LinkedFileName);

      // Add to the PATH override environment variable (specific to Delphi IDE) if $(PATH) isn't there.
      // See  issue 217. Only for Win32 (as this is only for the IDE)
      if not BuildInfo.Project.SkipRegistering.WindowsPath and (PlatformInfo.PlatType in PlatformsForDesign(BuildInfo.Project.ProjectId)) then
      begin
        if AddPathOverride(IDEInfo, BuildInfo.Project.AlternateRegistryKey, LinkedFolder)
          then Logger.Info('Adding "' + LinkedFolder + '" to ' + IDEId[IDEInfo.IDEName] + ' path override, since $(PATH) isn''t there.');
      end;

      //Add to the windows path to win32 and win64, so we can use runtime packages.
      //Also other apps/dlls might be linked to this folder (like the dll of FlexCel dll),
      //So we need to add it to the path.
      if not BuildInfo.Project.SkipRegistering.WindowsPath then
      begin
        if AddToWindowsPath(LinkedFolder) then Logger.Info('Added "' + LinkedFolder + '" to the Windows Path');
      end;
    end
    else
      Logger.Trace(Format('Link should be created from "%s" to "%s"', [LinkedFileName, BinaryPackageFileName]));

    // register the package in Delphi IDE if it's a design-time package
    if PackBuildInfo.PackageNeedsRegistering then
    begin
      if not BuildInfo.Project.DryRun then
      begin
        PackUninstall.WriteBool(UninstallConsts.Registered, True);
        IDEInfo.RegisterPackage(LinkedFileName, PlatformInfo.PlatType);
      end;
      Logger.Trace(Format('Package file %s registered in %s.', [BinaryPackageFileName, DisplayName]));
      Logger.Progress(Format('Package %s registered successfully in %s.', [Package.Name, DisplayName]), BuildInfo.Project.Progress);
    end;
  end;
end;


procedure TDelphiInstaller.RegisterAtPlatformLevel(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo);
var
  PlatformInfo: IDelphiPlatformInfo;
  Consolidation: TPackagesConsolidation;
  IDEInfo: IDelphiIDEInfo;

  procedure AppendIDEPath(PathType: TDelphiPathType; const PathsToAdd: array of string);
  var
    FinalPaths: string;
    ExistingPaths: string;
  begin
    FinalPaths := '';
    for var PathToAdd in PathsToAdd do
      FinalPaths := AddPaths(FinalPaths, PathToAdd);
    if FinalPaths <> '' then
    begin
      if not BuildInfo.Project.DryRun then
      begin
        ExistingPaths := UninstallInfo.Value.ReadStr(UninstallConsts.IDEPaths[PathType]);
        FinalPaths := AddPaths(ExistingPaths, FinalPaths);
        UninstallInfo.Value.WriteStr(UninstallConsts.IDEPaths[PathType], FinalPaths);
        PlatformInfo.AddIDEPath(PathType, FinalPaths);
      end;
      Logger.Trace(Format('%s added to platform "%s.%s": %s', [
        UninstallConsts.IDEPaths[PathType],
        DisplayName,
        PlatformId[PlatformInfo.PlatType],
        FinalPaths
      ]));
    end;
  end;

begin
  if BuildInfo.Project.SkipRegistering.Packages then exit;
  var InMegafolder := Config.DcuMegafolders.Match(BuildInfo.Project.ProjectId) <> '';

  // Initialize IDE and Platform helper interfaces
  IDEInfo := CreateIDEInfo(BuildInfo);
  PlatformInfo := IDEInfo.GetPlatform(BuildInfo.Platform.Name);

  // Get consolidated (merged) folders from all packages in platform
  Consolidation := ConsolidatePackages(BuildInfo, PlatformInfo, InMegafolder);

  // Save alternate registry key to uninstall info so next uninstall removes data from correct registry
  if not BuildInfo.Project.DryRun then
    UninstallInfo.Value.WriteStr(UninstallConsts.AlternateRegistryKey, BuildInfo.Project.AlternateRegistryKey);

  // Register Browsing Path
  var ExtraBrowsingPath := BuildInfo.Project.ExtraPaths.GetBrowsingPaths(PlatformInfo.PlatType);
  var BrowsingPath := AddPaths(Consolidation.BrowsingPath, ExtraBrowsingPath);

  if not BuildInfo.Project.AddSourceCodeToLibraryPath {and not BinaryInstall} then
    AppendIDEPath(ptBrowsingPath, [BrowsingPath]);

  // Register Library Path
  if BuildInfo.Project.AddSourceCodeToLibraryPath {and not BinaryInstall} then
    AppendIDEPath(ptLibraryPath, [BrowsingPath]);
  var ExtraLibraryPath := BuildInfo.Project.ExtraPaths.GetLibraryPaths(PlatformInfo.PlatType, false);
  if PlatformInfo.PlatType in [Linux64, macOS64Intel] then
    AppendIDEPath(ptLibraryPath, [Consolidation.DcuOutputDir, Consolidation.BplOutputDir, ExtraLibraryPath])
  else
    AppendIDEPath(ptLibraryPath, [Consolidation.DcuOutputDir, ExtraLibraryPath]);

  // Register Debug DCU Path
  var ExtraDebugDcuPath :=  BuildInfo.Project.ExtraPaths.GetDebugDCUPaths(PlatformInfo.PlatType);
  AppendIDEPath(ptDebugDcuPath, [Consolidation.DebugDcuOutputDir, ExtraDebugDcuPath]);

  // Add C++ Builder paths
  if Consolidation.DoCBuilder then
  begin
    AppendIDEPath(ptCppLibraryPath, [Consolidation.CppObjOutputDir, Consolidation.CppBpiOutputDir]);
    AppendIDEPath(ptCppIncludePath, [Consolidation.CppHppOutputDir]);
    AppendIDEPath(ptCppBrowsingPath, [Consolidation.BrowsingPath]);

    if (IDEInfo.IDEName >= TIDEName.delphiseattle) and (PlatformInfo.PlatType = TPlatform.win32intel) then
    begin
      AppendIDEPath(ptCppClang32LibraryPath, [Consolidation.CppObjOutputDir, Consolidation.CppBpiOutputDir]);
      AppendIDEPath(ptCppClang32IncludePath, [Consolidation.CppHppOutputDir]);
      AppendIDEPath(ptCppClang32BrowsingPath, [Consolidation.BrowsingPath]);
    end;
  end;

  // Register packages
  for var PackBuildInfo in BuildInfo.Platform.PackagesBuildInfo do
  begin
    var PackUninstall: TJSONObject;
    PackUninstall := UninstallInfo.Value.GetObject(UninstallConsts.Packages).GetObject(PackBuildInfo.Package.Name);
    RegisterPackage(BuildInfo, PackBuildInfo, PackUninstall);
  end;

  Logger.Progress(Format('%s registered successfully on platform "%s.%s".',
    [BuildInfo.Project.Project.Application.Name, DisplayName, PlatformId[PlatformInfo.PlatType]]), BuildInfo.Project.Progress);
end;

procedure TDelphiInstaller.RegisterHelp(const ProductName, HelpFile: string; IDEName: TIDEName; const Config: TConfigDefinition; const UninstallInfo: IUninstallInfo);
begin
  if HelpFile.Trim = '' then exit;
  if (IDEName < TIDEName.DelphiXE8) then exit; //XE8 switched to chm help

  Logger.Info('Registering help file for ' + DisplayName + ': "' + HelpFile + '"');
  if UninstallInfo.DryRun then exit;

  var HelpRegKeyName := GetHelpRegistryKeyName(IDEName, Config);
  var HelpRegValueName := ProductName;
  UninstallInfo.Value.WriteStr(UninstallConsts.HelpRegistryKeyName, HelpRegKeyName);
  UninstallInfo.Value.WriteStr(UninstallConsts.HelpRegistryValueName, HelpRegValueName);
  RegisterHelpInIDE(HelpRegKeyName, HelpRegValueName, HelpFile);

end;

function TDelphiInstaller.UnRegisterHelp(const UninstallInfo: IUninstallInfo): boolean;
begin
  Result := false;
  var HelpRegKeyName := UninstallInfo.Value.ReadStr(UninstallConsts.HelpRegistryKeyName, '');
  var HelpRegValueName := UninstallInfo.Value.ReadStr(UninstallConsts.HelpRegistryValueName, '');
  if HelpRegKeyName.Trim = '' then Exit;
  if HelpRegValueName.Trim = '' then Exit;

  Logger.Info(Format('Unregistering help file for %s from %s', [UninstallInfo.ProjectId, DisplayName]));
  if UninstallInfo.DryRun then exit;

  UnRegisterHelpInIDE(HelpRegKeyName, HelpRegValueName);
  Result := true;

end;

function TDelphiInstaller.IsXMLProj(const FileName: string): boolean;
begin
  Result := SameText(TPath.GetExtension(FileName), '.dproj') or SameText(TPath.GetExtension(FileName), '.cbproj');
end;

procedure TDelphiInstaller.UpdatePackageSource(const Orig, OutputDir: string; IDEName: TIDEName);
begin
  // Modify the existing local dproj so that building from the IDE also works
  if IsXMLProj(Orig) then
  begin
    // Update output directories in dproj
    var Writer := TDprojWriter.Create(Orig, IDEName);
    try
      Writer.UpdateBplOutput(OutputDir);
      Writer.UpdateDcpOutput(OutputDir);
      Writer.UpdateExeOutput(OutputDir);
      Writer.UpdateDcuOutput(OutputDir);
      Writer.UpdateBrccOutput(OutputDir);
      Writer.UpdateBpiOutput(OutputDir);
      Writer.UpdateHppOutput(OutputDir);
      Writer.UpdateObjOutput(OutputDir);
      Writer.Flush(true);
    finally
      Writer.Free;
    end;
  end;
end;

procedure TDelphiInstaller.UpdateProjectsSource(const BuildInfo: TFullBuildInfo);
begin
  for var PackBuildInfo in BuildInfo.Platform.PackagesBuildInfo do
  begin
    var PlatformInfo: IDelphiPlatformInfo := CreateIDEInfo(BuildInfo).GetPlatform(BuildInfo.Platform.Name);
    var PackageInfo: IDelphiPackageInfo := TPackageConfig.Create(PackBuildInfo.Package.Name, PlatformInfo, BuildInfo.Platform.PackagesFolder, BuildInfo.Project.Project.IsExe, PackBuildInfo.PackageExt, BuildInfo.Project.Project.LibSuffixes, BuildInfo.Project.Project.HasMultiIDEPackages);
    var OrigPackage := PackageInfo.OrigPackageFileName;

    UpdatePackageSource(OrigPackage, PackageInfo.UnexpandedOutputDir, BuildInfo.IDE.Name);
  end;
end;

function TDelphiInstaller.SupportsCppBuilder(
  const platform: TPlatform): boolean;
begin
   Result := (Platform = TPlatform.win32intel) and (IDEName >= TIDEName.delphi2006);
end;

procedure TDelphiInstaller.UnRegisterAtIDELevel(const UninstallInfo: IUninstallInfo);
begin
  var Updated := UnregisterHelp(UninstallInfo);
  if Updated then CreateIDEInfo(UninstallInfo).ForceIDEUpdate;
end;

procedure UnregisterAllLibraryPaths(const Root: string);
begin
  exit;
  //Remove everything that is registered in our path.
  for var IDEName := Low(TIDEName) to High(TIDEName) do
  begin
    var IDEInfo: IDelphiIDEInfo := TDelphiIDEInfo.Create(IDEName, Config.AlternateRegistryKey);
    for var Platform := Low(TPlatform) to High(TPlatform) do
    begin
      var PlatformInfo: IDelphiPlatformInfo := IDEInfo.GetPlatform(Platform);
      for var PlatType := Low(TDelphiPathType) to High(TDelphiPathType) do
      begin
        PlatformInfo.RemoveIDEPath(PlatType, Root);
      end;
    end;
  end;
end;

procedure RegisterMegafolders;
begin
  //Unregister all existing megafolders, just in case
  UnregisterAllLibraryPaths(Config.Folders.DcuMegafolder);
end;

procedure TDelphiInstaller.UnRegisterAtPlatformLevel(const UninstallInfo: IUninstallInfo);
var
  IDEInfo: IDelphiIDEInfo;
  PlatformInfo: IDelphiPlatformInfo;

  procedure RemoveIDEPath(PathType: TDelphiPathType);
  var
    PathsToRemove: string;
  begin
    PathsToRemove := UninstallInfo.Value.ReadStr(UninstallConsts.IDEPaths[PathType], '');
    if PathsToRemove = '' then Exit;

    if not UninstallInfo.DryRun then
      PlatformInfo.RemoveIDEPath(PathType, PathsToRemove);
    Logger.Trace(Format('Removed from %s in platform "%s.%s": %s', [
      UninstallConsts.IDEPaths[PathType],
      DisplayName,
      PlatformId[PlatformInfo.PlatType],
      PathsToRemove
    ]));
  end;

begin
  IDEInfo := CreateIDEInfo(UninstallInfo);
  PlatformInfo := IDEInfo.GetPlatform(UninstallInfo.Platform);

  // Remove IDE Paths from IDE
  for var PathType := Low(TDelphiPathType) to High(TDelphiPathType) do
    RemoveIDEPath(PathType);

  // Unregister packages
  for var Pair in UninstallInfo.Value.GetObject(UninstallConsts.Packages) do
    if Pair.JsonValue is TJSONObject then
    begin
      var PackUninstall := TJSONObject(Pair.JsonValue);
      var LinkedPackageFile := PackUninstall.ReadStr(UninstallConsts.LinkedPackageFile);
      //We will try to uninstall them all, even if we didn't register them
      //See https://github.com/tmssoftware/tms-smartsetup/issues/186
      //var Registered := PackUninstall.ReadBool(UninstallConsts.Registered);

      if LinkedPackageFile <> '' then
      begin
        //if Registered then
        begin
          if not UninstallInfo.DryRun then
          begin
            IDEInfo.UnregisterPackage(LinkedPackageFile, PlatformInfo.PlatType);
          end;
          Logger.Trace(Format('Package file "%s" unregistered from %s.', [LinkedPackageFile, DisplayName]));
        end;

        if TFile.Exists(LinkedPackageFile) then
        begin
          if not UninstallInfo.DryRun then
            DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, LinkedPackageFile);
          Logger.Trace(Format('Linked file "%s" deleted.', [LinkedPackageFile]));
        end
        else
          Logger.Trace(Format('Skipped deletion of linked file "%s": file does not exist.', [LinkedPackageFile]));
      end;
    end;
end;

{ TPackagesConsolidation }

class function TPackagesConsolidation.Init: TPackagesConsolidation;
begin
  Result.DebugDcuOutputDir := '';
  Result.DebugDcpOutputDir := '';
  Result.DcuOutputDir := '';
  Result.DcpOutputDir := '';
  Result.BplOutputDir := '';
  Result.CppBpiOutputDir := '';
  Result.CppHppOutputDir := '';
  Result.CppObjOutputDir := '';
  Result.BrowsingPath := '';
  Result.DoCBuilder := False;
end;

procedure TDelphiInstaller.CreateTempProjects(const BuildInfo: TFullBuildInfo);
begin
  Logger.StartSection(TMessageType.Register, 'Creating temp projects');
  try
    RemoveTempProjects(BuildInfo);
    for var PackBuildInfo in BuildInfo.Platform.PackagesBuildInfo do
    begin
      var PlatformInfo: IDelphiPlatformInfo := CreateIDEInfo(BuildInfo).GetPlatform(BuildInfo.Platform.Name);
      var PackageInfo: IDelphiPackageInfo := TPackageConfig.Create(PackBuildInfo.Package.Name, PlatformInfo, BuildInfo.Platform.PackagesFolder, BuildInfo.Project.Project.IsExe, PackBuildInfo.PackageExt, BuildInfo.Project.Project.LibSuffixes, BuildInfo.Project.Project.HasMultiIDEPackages);

      var OrigPackage := PackageInfo.OrigPackageFileName;
      for var BuildConfigIndex := Low(TBuildConfig) to High(TBuildConfig) do
      begin
        if (BuildConfigIndex = TBuildConfig.Debug) and not BuildInfo.Project.DebugDCUs then continue;

        var TempPackage := PackageInfo.TempPackageFileName(BuildInfo.Project.ProjectId, Config.Folders.ParallelFolder, BuildConfigs[BuildConfigIndex]);
        TDirectory_CreateDirectory(TPath.GetDirectoryName(TempPackage));
        var ExeOutputDir := PackageInfo.UnexpandedOutputDir;
        //Only calculate this for exes, as it might imply reading the dproj, and we don't need it for packages.
        if BuildInfo.Project.Project.IsExe then ExeOutputDir := PackageInfo.UnexpandedTempExeOutputDir(BuildConfigs[BuildConfigIndex]);

        CopyProjects(OrigPackage, TempPackage, PackageInfo.UnexpandedOutputDir,
                     ExeOutputDir,
                     BuildInfo.IDE.Name, BuildInfo.Project.Project.IsExe,
                     BuildInfo.Project.Project.Application, BuildInfo.Project.Project.AddLibSuffix,
                     CreateIDEInfo(BuildInfo).Brcc32File);
      end;
    end;
  finally
    Logger.FinishSection(TMessageType.Register, false);
  end;
end;

procedure TDelphiInstaller.UpdateMegafolders(const SourceFolder, ProjectId: string; const IDEName: TIDEName; const Platform: TPlatform; const BuildConfig: TBuildConfig;
  const UsedDcuMegafolders: TUsedMegafolders);
begin
  var Megafolder := Config.DcuMegafolders.Match(ProjectId);
  if Megafolder <> '' then
  begin
    Logger.StartSection(TMessageType.Megafolder, 'Update DCU Megafolders');
    try
      var RelMegafolderPath := TMegafolderManager.GetFolderName(Megafolder, IDEName, Platform, BuildConfig);
      if UsedDcuMegafolders <> nil then UsedDcuMegafolders.Add(RelMegafolderPath);
      Logger.Trace('Updating ' + ProjectId + ' in the DCU Megafolder ' + RelMegafolderPath + '...');
      var MegafolderCount := 0;
      var MegaPath := TPath.Combine(Config.Folders.DcuMegafolder, RelMegafolderPath);
      TDirectory_CreateDirectory(MegaPath);
      TMegafolderManager.UpdateFolder(SourceFolder, MegaPath, MegafolderCount);

      if (MegafolderCount = 0) then Logger.Trace('DCU Megafolder up to date')
      else Logger.Trace('Updated ' + IntToStr(MegafolderCount) + ' files in the Megafolder.');
    finally
      Logger.FinishSection(TMessageType.Megafolder);
    end;
  end;
end;

procedure TDelphiInstaller.MoveDataFromTempProjects(const BuildInfo: TFullBuildInfo; const UsedDcuMegafolders: TUsedMegafolders);
begin
  var PlatformInfo := CreateIDEInfo(BuildInfo).GetPlatform(BuildInfo.Platform.Name);

  for var BuildConfigIndex := Low(TBuildConfig) to High(TBuildConfig) do
  begin
    var BuildConfig := BuildConfigs[BuildConfigIndex];

    var TempProjFolder := TempProjectDirectory(BuildInfo.Project.ProjectId, Config.Folders.ParallelFolder, BuildConfig, BuildInfo.IDE.Name, BuildInfo.Platform.Name);
    var TempPackageInfo: IDelphiPackageInfo := TPackageConfig.Create('DummyPackName', PlatformInfo, TempProjFolder, BuildInfo.Project.Project.IsExe, 'dummyext', BuildInfo.Project.Project.LibSuffixes, false);
    var TempOutputFolder := TempPackageInfo.ExpandedDcuOutputDir(BuildConfig);

    var OutputPackageInfo: IDelphiPackageInfo := TPackageConfig.Create('DummyPackName', PlatformInfo, BuildInfo.Platform.PackagesFolder, BuildInfo.Project.Project.IsExe, 'dummyext', BuildInfo.Project.Project.LibSuffixes, BuildInfo.Project.Project.HasMultiIDEPackages);
    var FinalOutputFolder := OutputPackageInfo.ExpandedDcuOutputDir(BuildConfig);
    FinalOutputFolder := TPath.GetFullPath(FinalOutputFolder);
    if FolderIsOutside(FinalOutputFolder, Config.GetAllRootFolders) then raise Exception.Create('For security, we can''t generate files in folder "' + FinalOutputFolder + '". The files must be inside a root folder for smartsetup.');

    if TDirectory.Exists(TempOutputFolder) then
    begin
      DeleteFolderMovingToLocked(Config.Folders.LockedFilesFolder, FinalOutputFolder, true);
      RenameAndCheckFolder(TempOutputFolder, FinalOutputFolder);

      if (not BuildInfo.Project.SkipRegistering.Packages)
        then UpdateMegafolders(FinalOutputFolder, BuildInfo.Project.ProjectId, PlatformInfo.IDEInfo.IDEName, PlatformInfo.PlatType, BuildConfigIndex, UsedDcuMegafolders);
    end;

    if BuildInfo.Project.Project.IsExe then
    begin
    for var PackBuildInfo in BuildInfo.Platform.PackagesBuildInfo do //there should be just 1 for exes
      begin
        var PackageInfo: IDelphiPackageInfo := TPackageConfig.Create(PackBuildInfo.Package.Name, PlatformInfo, BuildInfo.Platform.PackagesFolder, BuildInfo.Project.Project.IsExe, PackBuildInfo.PackageExt, BuildInfo.Project.Project.LibSuffixes, BuildInfo.Project.Project.HasMultiIDEPackages);

        var ExeTempOutputFolder := PackageInfo.ExpandedTempExeOutputDir(BuildInfo.Project.ProjectId, Config.Folders.ParallelFolder, BuildConfig);
        if (ExeTempOutputFolder <> TempOutputFolder) then
        begin
          if TDirectory.Exists(ExeTempOutputFolder) then
          begin
            var ExeFinalOutputFolder := PackageInfo.ExpandedExeOutputDir(BuildConfig);
            if FolderIsOutside(ExeFinalOutputFolder, Config.GetAllRootFolders) then raise Exception.Create('For security, we can''t generate files in folder "' + ExeFinalOutputFolder + '". The files must be inside a root folder for smartsetup.');
            //Only move the files, don't remove the folder. This could be going to a folder where there is other stuff.
            //The dcu folder can be removed as it is under our control.
            for var FileDest in TDirectory.GetFiles(ExeTempOutputFolder) do
            begin
              TDirectory_CreateDirectory(ExeFinalOutputFolder);
              var FileTarget := TPath.Combine(ExeFinalOutputFolder, TPath.GetFileName(FileDest));
              DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, FileTarget);
              RenameAndCheck(FileDest, FileTarget);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TDelphiInstaller.RemoveTempProjects(const BuildInfo: TFullBuildInfo);
begin
  var PlatformInfo: IDelphiPlatformInfo := CreateIDEInfo(BuildInfo).GetPlatform(BuildInfo.Platform.Name);
  var PackageInfo: IDelphiPackageInfo := TPackageConfig.Create('', PlatformInfo, BuildInfo.Platform.PackagesFolder, BuildInfo.Project.Project.IsExe, '', BuildInfo.Project.Project.LibSuffixes, BuildInfo.Project.Project.HasMultiIDEPackages);

  for var BuildConfigIndex := Low(TBuildConfig) to High(TBuildConfig) do
  begin
    if (BuildConfigIndex = TBuildConfig.Debug) and not BuildInfo.Project.DebugDCUs then continue;

    var Folder := PackageInfo.TempPackageDirectory(BuildInfo.Project.ProjectId, Config.Folders.ParallelFolder, BuildConfigs[BuildConfigIndex]);
    if not (TPath.GetFileName(Folder).Contains(PlatformId[BuildInfo.Platform.Name])) then
    begin
      Logger.Info('Can''t remove folder: "' + Folder + '". It is the wrong platform.');
      continue;
    end;
    if not SameFolder(TPath.GetDirectoryName(Folder), Config.Folders.ParallelFolder) then
    begin
      Logger.Info('Can''t remove folder: "' + Folder + '". It is should be inside a ' + Config.Folders.ParallelFolder + '.');
      continue;
    end;

    try
      if not TDirectory.Exists(Folder) then continue;

      // Force delete of folder recursively. I hope this is safe, as we did a few checks above (we will not delete a folder
      // that is not inside the parallel meta folder
      // This was changed to be recursive because due to some misterious reason, some folders are created in the temporary one.
      // For example, if the folder are copied packages to compile for Win32, Delphi laves a remaining Win32/Release folder
      // inside the temp one. This behavior didn't happen in my development machine in Windows 11, but it did happen in my
      // test machine (Windows 10).
      if not Config.KeepParallelFolders(BuildInfo.Project.ProjectId) then
      begin
        TDirectory.Delete(Folder, True);
        Logger.Trace('Temporary parallel directory deleted successfully: ' + Folder);
      end;

    except on ex: Exception do
      begin
        Logger.Info('Can''t remove folder: "' + Folder + '". ' + ex.Message);
        continue;
      end;
    end;
  end;
end;

function TDelphiInstaller.ProjectFileSupportsPlatform(const IgnoreDprojPlatforms: boolean;
  const RootFolder, PackageFileName: string; const dp: TPlatform): boolean;
begin
  if SameText(TPath.GetExtension(PackageFileName), BinprojExtension) then
  begin
    exit(TPrecompiledCompiler.SupportsPlatform(RootFolder, IDEName, dp));
  end;

  if not SameText(TPath.GetExtension(PackageFileName), '.dproj') and not SameText(TPath.GetExtension(PackageFileName), '.cbproj') then
    Exit(dp = TPlatform.win32intel);

  var PackageMetadata := TDelphiProjectFactory.GetPackageReadData(PackageFileName, IDEName);
  if PackageMetadata = nil then exit(true);

  if IgnoreDprojPlatforms then exit(true);

  var AllPlats := PackageMetadata.TargetedPlatforms;
  if (AllPlats = '') then exit(true); //XE doesn't have TargetedPlatforms. We will just assume the platform is supported.

  var AllPlatsNumber := StrToInt(AllPlats);
  Result := TDprojReader.PlatformIsSupported(AllPlatsNumber, dp);
end;

procedure TDelphiInstaller.CopyProjectRes(const SourceProj, TargetFolder: string; const IDEName: TIDEName; const Application: TApplicationDefinition; const Brcc32Path: string);
begin
  if IDEName > DelphiXE then exit;
  var SourceFile := TPath.ChangeExtension(SourceProj, '.res');
  var TargetFile := TPath.Combine(TargetFolder, TPath.GetFileName(SourceFile));

  //We could copy those resources if they exist, or create them if they don't, but all the resources
  //I've seen, from all groups of tms, just include the standard D7 to DXE Icons and a fileversion of 1.0.
  //Ok, FlexCel dll does include an icon and the correct file version. I don't even remember when I coded that, but
  //I also think it is worth just for it, and only for DXE or older.
  //So instead of using those res which even if they exist they are likely wrong, we will always create new ones.
  //Also this is only for XE or older, so it won't change and we won't be getting extra things in those res files for XE or older.

  //if TFile.Exists(SourceFile) then TFile.Copy(SourceFile, TargetFile, true);

  CreateResFile(TargetFile, Application, Brcc32Path);
end;


function ConvertPathToTarget(const SourceDir, TargetDir, FileName: string): string;
begin
  if FileName.StartsWith('$')   //Some macro like $(BDS). Shouldn't be adapted.
    then exit(FileName);

  Result := ExtractRelativePath(
    IncludeTrailingPathDelimiter(TargetDir),
    TPath.GetFullPath(TPath.Combine(SourceDir, FileName)));
end;

function ConvertAllPaths(const SourceDir, TargetDir, PathStr: string): string;
begin
  Result := '';
  var Paths := PathStr.Split([';']);
  var First := true;
  for var i := 0 to Length(Paths) - 1 do
  begin
    if Paths[i].Trim = '' then continue;

    if not First then Result := Result + ';';
    First := false;
    Result := Result + ConvertPathToTarget(SourceDir, TargetDir, Paths[i]);
  end;

end;

function ConvertRCPath(const FileName: string): string;
begin
  //There should be no need to put them in folders, since they have to be named differently
  //or they would crash when generated in Platform\Release.
  Result := TPath.GetFileName(FileName);
end;

procedure TDelphiInstaller.CopyProjects(const Orig, Temp, OutputDir, ExeOutputDir: string;
    IDEName: TIDEName; const IsExe: boolean;
    const Application: TApplicationDefinition; const AddLibSuffix: boolean; const Brcc32Path: string);
var
  SourceDir, TargetDir: string;

  function ConvertPath(const FileName: string): string;
  begin
    Result := ConvertPathToTarget(SourceDir, TargetDir, FileName);
  end;

  procedure EnsureRootFolderInLinkPath(const DProjModifier: TDProjModifier);
  begin
    var BaseNodeForSearch: IInterface := nil;
    try
      DProjModifier.LoopOverAllNodes(
        function (NodeName, NodeText: string; PropGroupIsBase: boolean): string
        begin
          Result := NodeText;
          if (NodeName = '/#document/Project/PropertyGroup/ILINK_LibraryPath/#text') then
          begin;
            if PropGroupIsBase then
            begin
              BaseNodeForSearch := nil;
              exit(ConvertPathToTarget(SourceDir, TargetDir, '.') + ';' + NodeText);
            end;
          end;
        end,
        procedure(BasePropertyGroup: IInterface)
        begin
          BaseNodeForSearch := BasePropertyGroup;
        end);

      if BaseNodeForSearch <> nil then DProjModifier.AddChildNode(BaseNodeForSearch, 'ILINK_LibraryPath', ConvertPathToTarget(SourceDir, TargetDir, '.'));

    finally
      BaseNodeForSearch := nil;
    end;
  end;

  procedure AddMissingOutputs(const DProjModifier: TDProjModifier; const ISCPP: boolean; const ExistingOutputs: THashSet<string>; const OutputDir, ExeOutputDir: string);
  begin
    for var OutputNode in DProjModifier.AllOutputNodes do
    begin
      if ExistingOutputs.Contains(OutputNode) then continue;
      if not ISCPP and DProjModifier.ISCPPOnlyNode(OutputNode) then continue;

      var ActualOutputDir := OutputDir;
      if DProjModifier.IsExe(OutputNode) then ActualOutputDir := ExeOutputDir;

      DProjModifier.AddBaseProjectNode(OutputNode, ActualOutputDir);
    end;
  end;


begin
  TargetDir := TPath.GetDirectoryName(Temp);
  SourceDir := TPath.GetDirectoryName(Orig);
  var ExistingOutputs := THashSet<string>.Create;
  try
    CopyProjectRes(Orig, TargetDir, IDEName, Application, Brcc32Path);
    // Copy and modify dproj
    if IsXMLProj(Orig) then
    begin
      // copy dproj file to the temporary destination for parallel compilation
      begin
        var DProjModifier := TDprojModifier.Create(Orig, IDEName);
        try
          DProjModifier.AdaptAllPaths(
           function (s, NodeName: string; NodeIsOutput, NodeIsExe, NodeIsBase: boolean): string
           begin;
             if TPath.GetExtension(s).ToLower = '.rc' then //rc must be moved because BRCC creates temp files. See #97.
             begin
               var RC := ConvertRCPath(s);
               TRCModifier.AdaptAllPaths(TPath.Combine(SourceDir, s), TPath.Combine(TargetDir, RC),
                   function (s2: string): string begin;Result := ConvertPathToTarget(TPath.GetDirectoryName(TPath.Combine(SourceDir, s)), TargetDir, s2).Replace('\\','/').Replace('\','/'); end
                   );

               exit(RC);
             end;

             var OriginalPath := s;
             //see https://github.com/tmssoftware/tms-smartsetup/issues/201
             //if IsOutput and not IsExe then OriginalPath := '.\$(Platform)\$(Config)';
             if NodeIsOutput then
             begin
               if NodeIsBase then ExistingOutputs.Add(NodeName);
               if IsExe and NodeIsExe then exit(ExeOutputDir) else exit(OutputDir);
             end;

             Result := ConvertPathToTarget(SourceDir, TargetDir, OriginalPath);
           end,
           function (s, NodeName: string; NodeIsOutput, NodeIsExe, NodeIsBase: boolean): string
           begin
             Result := ConvertAllPaths(SourceDir, TargetDir, s);
           end
           );

          var ISCPP := TPath.GetExtension(Orig) = '.cbproj';
          AddMissingOutputs(DProjModifier, ISCPP, ExistingOutputs, OutputDir, ExeOutputDir);
          if (IsCPP) then EnsureRootFolderInLinkPath(DProjModifier);

          DProjModifier.Save(Temp);
        finally
          DProjModifier.Free;
        end;
      end;
    end;
  finally
    ExistingOutputs.Free;
  end;

  // Copy and modify .dpk
  if SameText(TPath.GetExtension(Orig), '.cbproj') then
  begin
    var SourceCpp := TPath.ChangeExtension(Orig, '.cpp');
    var TargetCpp := TPath.ChangeExtension(Temp, '.cpp');
    TFile.Copy(SourceCpp, TargetCpp, True);
  end else
  if SameText(TPath.GetExtension(Orig), BinprojExtension) then
  begin
  end else
  begin
    if IsExe then
    begin
      var SourceDpr := TPath.ChangeExtension(Orig, '.dpr');
      var TargetDpr := TPath.ChangeExtension(Temp, '.dpr');

      ChangePath(SourceDpr, TargetDpr,
        function (s: string): string begin;Result := ConvertPathToTarget(SourceDir, TargetDir, s); end,
        function (s: string): string begin;Result := ConvertRCPath(s); end
      );
    end
    else
    begin
      var SourceDpk := TPath.ChangeExtension(Orig, '.dpk');
      var TargetDpk := TPath.ChangeExtension(Temp, '.dpk');
      TFile.Copy(SourceDpk, TargetDpk, True);
      // Read existing data
      var PackData := TDpkData.Create;
      try
        var Reader := TDpkReader.Create(TargetDpk, IDEName);
        try
          Reader.ReadData(PackData);
        finally
          Reader.Free;
        end;

        // convert relative paths
        for var Include in PackData.PasFiles do
          Include.FileName := ConvertPath(Include.FileName);
        for var Include in PackData.DcrFiles do
          Include.FileName := ConvertPath(Include.FileName);

        // now update dpk with new information
        var Writer := TDpkWriter.Create(TargetDpk, IDEName);
        try
          // Package files
          Writer.UpdateDcrFiles(PackData.DcrFiles);
          Writer.UpdateRequires(PackData.Requires);
          Writer.UpdatePasFiles(PackData.PasFiles);
          if AddLibSuffix then Writer.UpdateDllSuffix(GetLibSuffix(IDEName, false));
          Writer.Flush;
        finally
          Writer.Free;
        end;
      finally
        PackData.Free;
      end;
    end;
  end;
end;

{$ELSE}
{$region 'Not windows'}
{ TDelphiInstaller }

procedure TDelphiInstaller.Build(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo);
begin
end;

procedure TDelphiInstaller.CleanAllBuildTemporaryFiles(
  const UninstallInfo: IUninstallInfo);
begin
end;

procedure TDelphiInstaller.CreateTempProjects(const BuildInfo: TFullBuildInfo);
begin
end;

procedure TDelphiInstaller.MoveDataFromTempProjects(
  const BuildInfo: TFullBuildInfo; const UsedDcuMegafolders: TUsedMegafolders);
begin
end;

procedure TDelphiInstaller.RegisterAtIDELevel(
  const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo);
begin
end;

procedure TDelphiInstaller.RegisterAtPlatformLevel(const BuildInfo: TFullBuildInfo;
  const UninstallInfo: IUninstallInfo);
begin
end;

procedure TDelphiInstaller.RemoveTempProjects(const BuildInfo: TFullBuildInfo);
begin
end;

function TDelphiInstaller.ProjectFileSupportsPlatform(const IgnoreDprojPlatforms: boolean;
  const RootFolder, PackageFileName: string; const dp: TPlatform): boolean;
begin
  Result := true;
end;

procedure TDelphiInstaller.UnRegisterAtIDELevel(
  const UninstallInfo: IUninstallInfo);
begin
end;

procedure TDelphiInstaller.UnRegisterAtPlatformLevel(
  const UninstallInfo: IUninstallInfo);
begin
end;

procedure TDelphiInstaller.UpdateProjectsSource(
  const BuildInfo: TFullBuildInfo);
begin

end;

function TDelphiInstaller.PlatformsForDesign(const ProductId: string): TPlatformSet;
begin
  Result := [TPlatform.win32intel];
end;

function TDelphiInstaller.SupportsCppBuilder(
  const platform: TPlatform): boolean;
begin
   Result := (Platform = TPlatform.win32intel) and (IDEName >= TIDEName.delphi2006);
end;

{$endregion}

{$ENDIF}

end.

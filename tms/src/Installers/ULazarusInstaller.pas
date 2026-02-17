unit ULazarusInstaller;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, UInstaller, UUninstallInfo, UFullBuildInfo, Megafolders.Definition, Generics.Collections,
     UProjectBuildInfo;

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
    procedure RegisterMegafolders(const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo); override;

    procedure UnRegisterAtIDELevel(const UninstallInfo: IUninstallInfo);override;
    procedure UnRegisterAtPlatformLevel(const UninstallInfo: IUninstallInfo);override;
    procedure UnregisterMegafolders(const UninstallInfo: IUninstallInfo; const OtherEntries: TArray<string>); override;

    procedure UpdateProjectsSource(const BuildInfo: TFullBuildInfo); override;

    procedure UpdateMegafolders(const SourceFolder, ProjectId: string;
      const IDEName: TIDEName; const Platform: TPlatform;
      const BuildConfig: TBuildConfig;
      const UsedDcuMegafolders: TUsedMegafolders); override;


    procedure SetupIDE(const IDEName: TIDEName; var HasErrors: boolean); override;

    procedure CreateTempProjects(const BuildInfo: TFullBuildInfo); override;
    procedure MoveDataFromTempProjects(const BuildInfo: TFullBuildInfo; const UsedDcuMegafolders: TUsedMegafolders); override;
    procedure RemoveTempProjects(const BuildInfo: TFullBuildInfo); override;

    function ProjectFileSupportsPlatform(const IgnoreDprojPlatforms: boolean; const RootFolder, PackageFileName: string; const dp: TPlatform): boolean; override;

  end;
implementation
uses UIDEUtils, UMultiLogger, SysUtils, IOUtils, Generics.Defaults,
     Xml.XMLDoc, Xml.XMLIntf, UTmsBuildSystemUtils,
     {$IFDEF MSWINDOWS}
     Windows,
     ActiveX,
     {$ENDIF}
     Deget.CommandLine;

function GetBackupFileName(const PackageFileName: string): string;
begin
  var Dir := TPath.GetDirectoryName(PackageFileName);
  var Name := TPath.GetFileNameWithoutExtension(PackageFileName);
  var Ext := TPath.GetExtension(PackageFileName);
  Result := TPath.Combine(Dir, Name + '.orig.smartsetup' + Ext);
end;

procedure AddLazarusPackagesToMap(const ProjectBI: TProjectBuildInfo; const Map: TDictionary<string, string>);
begin
  for var IDE in ProjectBI.IDEsBuildInfo do
  begin
    if IDE.Name <> TIDEName.lazarus then continue;
    for var Plat in IDE.PlatformsBuildInfo do
      for var Pkg in Plat.PackagesBuildInfo do
      begin
        var PkgName := TPath.GetFileNameWithoutExtension(Pkg.PackageFileName);
        if not Map.ContainsKey(PkgName) then
          Map.Add(PkgName, Pkg.PackageFileName);
      end;
  end;
end;

function GetManagedPackageMap(const BuildInfo: TFullBuildInfo): TDictionary<string, string>;
begin
  Result := TDictionary<string, string>.Create(TIStringComparer.Ordinal);
  try
    AddLazarusPackagesToMap(BuildInfo.Project, Result);
    for var Dep in BuildInfo.Project.Dependencies do
      AddLazarusPackagesToMap(Dep, Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure DoPatchLpk(const SourceFile, DestFile: string; const ManagedPackages: TDictionary<string, string>);
var
  XML: IXMLDocument;
begin
  XML := TXMLDocument.Create(SourceFile);

  var ConfigNode := XML.DocumentElement;
  if ConfigNode <> nil then
  begin
    var PackageNode: IXMLNode := nil;
    for var i := 0 to ConfigNode.ChildNodes.Count - 1 do
      if ConfigNode.ChildNodes[i].LocalName = 'Package' then
      begin
        PackageNode := ConfigNode.ChildNodes[i];
        break;
      end;

    if PackageNode <> nil then
    begin
      var RequiredPkgsNode: IXMLNode := nil;
      for var i := 0 to PackageNode.ChildNodes.Count - 1 do
        if PackageNode.ChildNodes[i].LocalName = 'RequiredPkgs' then
        begin
          RequiredPkgsNode := PackageNode.ChildNodes[i];
          break;
        end;

      if RequiredPkgsNode <> nil then
      begin
        for var i := 0 to RequiredPkgsNode.ChildNodes.Count - 1 do
        begin
          var ItemNode := RequiredPkgsNode.ChildNodes[i];

          var PkgNameNode: IXMLNode := nil;
          for var j := 0 to ItemNode.ChildNodes.Count - 1 do
            if ItemNode.ChildNodes[j].LocalName = 'PackageName' then
            begin
              PkgNameNode := ItemNode.ChildNodes[j];
              break;
            end;
          if PkgNameNode = nil then continue;

          var PkgName: string := PkgNameNode.Attributes['Value'];

          var DepFileName: string;
          if not ManagedPackages.TryGetValue(PkgName, DepFileName) then continue;

          Logger.Trace('Patching dependency ' + PkgName + ' -> ' + DepFileName);

          var DefaultFileNode: IXMLNode := nil;
          for var j := 0 to ItemNode.ChildNodes.Count - 1 do
            if ItemNode.ChildNodes[j].LocalName = 'DefaultFilename' then
            begin
              DefaultFileNode := ItemNode.ChildNodes[j];
              break;
            end;

          if DefaultFileNode = nil then
            DefaultFileNode := ItemNode.AddChild('DefaultFilename');
          DefaultFileNode.Attributes['Value'] := DepFileName;
          DefaultFileNode.Attributes['Prefer'] := 'True';
        end;
      end;
    end;
  end;

  XML.SaveToFile(DestFile);
end;

procedure PatchLpkDependencies(const SourceFile, DestFile: string; const ManagedPackages: TDictionary<string, string>);
begin
{$IFDEF MSWINDOWS}
  CoInitialize(nil);
{$ENDIF}
  try
    DoPatchLpk(SourceFile, DestFile, ManagedPackages);
  finally
{$IFDEF MSWINDOWS}
    CoUninitialize;
{$ENDIF}
  end;
end;

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
    var PackageFileName := BuildInfo.Package.PackageFileName;

    if SameText(TPath.GetExtension(PackageFileName), '.lpkk') then
    begin
      var BackupFileName := GetBackupFileName(PackageFileName);

      if TFile.Exists(BackupFileName) then
        DeleteFileOrMoveToLocked(BuildInfo.Project.Config.Folders.LockedFilesFolder, PackageFileName)
      else
        SysUtils.RenameFile(PackageFileName, BackupFileName);

      try
        var ManagedPackages := GetManagedPackageMap(BuildInfo);
        try
          PatchLpkDependencies(BackupFileName, PackageFileName, ManagedPackages);
        finally
          ManagedPackages.Free;
        end;

        if not ExecuteCommand(LazBuild + ' --build-all ' + CompilerParameters + ' "' + PackageFileName + '"')
          then raise Exception.Create('Failed to compile ' + PackageFileName);
      finally
        if TFile.Exists(PackageFileName)
          then DeleteFileOrMoveToLocked(BuildInfo.Project.Config.Folders.LockedFilesFolder, PackageFileName);
        SysUtils.RenameFile(BackupFileName, PackageFileName);
      end;
    end
    else
    begin
      if not ExecuteCommand(LazBuild + ' --build-all ' + CompilerParameters + ' "' + PackageFileName + '"')
        then raise Exception.Create('Failed to compile ' + PackageFileName);
    end;
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

procedure TLazarusInstaller.RegisterMegafolders(
  const BuildInfo: TFullBuildInfo; const UninstallInfo: IUninstallInfo);
begin
end;

procedure TLazarusInstaller.SetupIDE(const IDEName: TIDEName; var HasErrors: boolean);
begin
  //Nothing for lazarus
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

procedure TLazarusInstaller.UnregisterMegafolders(
  const UninstallInfo: IUninstallInfo; const OtherEntries: TArray<string>);
begin
end;

procedure TLazarusInstaller.UpdateMegafolders(const SourceFolder,
  ProjectId: string; const IDEName: TIDEName; const Platform: TPlatform;
  const BuildConfig: TBuildConfig; const UsedDcuMegafolders: TUsedMegafolders);
begin
end;

procedure TLazarusInstaller.UpdateProjectsSource(const BuildInfo: TFullBuildInfo);
begin
end;

procedure TLazarusInstaller.CreateTempProjects(const BuildInfo: TFullBuildInfo);
begin

end;

procedure TLazarusInstaller.MoveDataFromTempProjects(const BuildInfo: TFullBuildInfo; const UsedDcuMegafolders: TUsedMegafolders);
begin
end;

procedure TLazarusInstaller.RemoveTempProjects(const BuildInfo: TFullBuildInfo);
begin
end;



end.

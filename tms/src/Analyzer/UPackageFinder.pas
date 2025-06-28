unit UPackageFinder;
{$i ../../tmssetup.inc}

interface
uses SysUtils, UProjectDefinition, UNaming, Deget.CoreTypes, UConfigDefinition, UPackageCache;

type
  TPackageFinder = class
  private
    class function EndsInMultiIDE(const Naming: TNaming; const StartIDEName: TIDEName; const aFullFolder: string; out BaseFolder, StandardFolderNamePlus: string): boolean; static;
  public
    class function GetPackage(const Naming: TNaming; const dv: TIDEName; const IsExe: boolean;
       const packs: TArray<string>; const BasePath, PackageName: string; const ThrowExceptions: boolean; const Project: TProjectDefinition): string;
    class function GetProjectToBuild(const PackageCache: TPackageCache; const dv: TIDEName; const Project: TProjectDefinition;
      const Package: TPackage; const Naming: TNaming; const ThrowExceptions: boolean; const ForceExt: TArray<string>): string; static;

    class function PackagesFolder(const BasePackagesFolder: string; const dv: TIDEName; const Naming: TNaming; const IsExe: boolean; const ProjectFolder: string): string;
    class function PackagesExist(const BasePackagesFolder: string; const dv: TIDEName; const Naming: TNaming; const IsExe: boolean; const ProjectFolder: string): boolean;
    class function EndsInFolder(const aFullFolder, aEndsWithFolder: string; out BaseFolder: string): boolean; static;
  end;

implementation
uses IOUtils, UInstaller;
function Throw(const ThrowExceptions: boolean; const msg: string): string;
begin
  if ThrowExceptions then raise Exception.Create(msg);
  Result := '';

end;

{ TPackageFinder }

class function TPackageFinder.GetProjectToBuild(const PackageCache: TPackageCache; const dv: TIDEName;
  const Project: TProjectDefinition;
  const Package: TPackage; const Naming: TNaming; const ThrowExceptions: boolean; const ForceExt: TArray<string>): string;
begin
  var BasePath := TPath.GetDirectoryName(Project.FullPath);
  var exts := ForceExt;
  if exts = nil then
  begin
    exts := TInstallerFactory.GetInstaller(dv).PackageExtension(Package.PackageType);
  end;

  var packs := PackageCache.GetFilesForPkg(BasePath, exts, Package.Name);

  var FullPackName := Package.Name;

  if Length(packs) = 0 then
  begin
    exit(Throw(ThrowExceptions, 'Can''t find the package: "' + FullPackName +'" inside the folder "' + BasePath + '".'));
  end;

  if Package.PackageType = TPackageType.Exe then
  begin
    if Length(packs) <> 1 then exit(Throw(ThrowExceptions, 'The project: "' + FullPackName +'" inside the folder "' + BasePath + '" is repeated ' + IntToStr(Length(packs)) + ' times.'));
    Result := packs[0];
  end
  else
  begin
    Result := GetPackage(Naming, dv, Package.PackageType = TPackageType.Exe, packs, BasePath, FullPackName, ThrowExceptions, Project);
  end;
end;

class function TPackageFinder.EndsInFolder(const aFullFolder, aEndsWithFolder: string; out BaseFolder: string): boolean;
begin
  BaseFolder := aFullFolder;
  var EndsWithFolder := aEndsWithFolder;
  while EndsWithFolder <> '' do
  begin
    if not SameText(TPath.GetFileName(BaseFolder).Trim, TPath.GetFileName(EndsWithFolder).Trim) then exit(false);
    EndsWithFolder := TPath.GetDirectoryName(EndsWithFolder);
    BaseFolder := TPath.GetDirectoryName(BaseFolder);
  end;

  Result := true;
end;

class function TPackageFinder.EndsInMultiIDE(const Naming: TNaming; const StartIDEName: TIDEName; const aFullFolder: string; out BaseFolder, StandardFolderNamePlus: string): boolean;
begin
  for var dv := StartIDEName downto TIDEName.delphi11 do //Delphi < 11 doesn't support d7+ notation.
  begin
    StandardFolderNamePlus := Naming.GetPackageNamingPlus(dv);
    if EndsInFolder(aFullFolder, StandardFolderNamePlus, BaseFolder) then exit(true);
  end;
  Result := false;
end;

class function TPackageFinder.GetPackage(const Naming: TNaming; const dv: TIDEName; const IsExe: boolean;
  const packs: TArray<string>; const BasePath, PackageName: string; const ThrowExceptions: boolean; const Project: TProjectDefinition): string;
begin
  var StandardFolderName := Naming.GetPackageNaming(dv, IsExe, Project.PackageFolders[dv]);
  for var pack in packs do
  begin
    if (pack.Trim = '') then continue;

    var BaseFolder := '';
    var PackFolder := TPath.GetDirectoryName(pack);
    if EndsInFolder(PackFolder, StandardFolderName, BaseFolder)
      then exit(pack);

    var StandardFolderNamePlus: string;
    if not IsExe and (Project.PackageFolders[dv] = '') and EndsInMultiIDE(Naming, dv, PackFolder, BaseFolder, StandardFolderNamePlus) then
    begin
      Project.SetPackageFolders(dv, StandardFolderNamePlus);
      exit(pack);
    end;


  end;

  exit(Throw(ThrowExceptions, 'Can''t find the folder "' + StandardFolderName + '" with "' + PackageName + '" inside the folder "' + BasePath + '".'));
end;

class function TPackageFinder.PackagesFolder(const BasePackagesFolder: string;
  const dv: TIDEName; const Naming: TNaming; const IsExe: boolean; const ProjectFolder: string): string;
begin
 var DelphiFolder := Naming.GetPackageNaming(dv, IsExe, ProjectFolder);
 Result := TPath.Combine(BasePackagesFolder, DelphiFolder);
end;


class function TPackageFinder.PackagesExist(const BasePackagesFolder: string;
  const dv: TIDEName; const Naming: TNaming; const IsExe: boolean; const ProjectFolder: string): boolean;
begin
 Result := TDirectory.Exists(PackagesFolder(BasePackagesFolder, dv, Naming, IsExe, ProjectFolder));
end;


end.

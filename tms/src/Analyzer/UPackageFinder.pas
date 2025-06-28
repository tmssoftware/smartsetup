unit UPackageFinder;
{$i ../../tmssetup.inc}

interface
uses SysUtils, UProjectDefinition, UNaming, Deget.CoreTypes, UConfigDefinition, UPackageCache;

type
  TPackageFinder = class
  public
    class function GetPackage(const folderName: string; const packs: TArray<string>; const BasePath, PackageName: string; const ThrowExceptions: boolean): string;
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
    Result := GetPackage(Naming.GetPackageNaming(dv, Package.PackageType = TPackageType.Exe, Project.PackageFolders[dv]), packs, BasePath, FullPackName, ThrowExceptions);
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

class function TPackageFinder.GetPackage(const folderName: string;
  const packs: TArray<string>; const BasePath, PackageName: string; const ThrowExceptions: boolean): string;
begin
  for var pack in packs do
  begin
    if (pack.Trim = '') then continue;

    var BaseFolder := '';
    if EndsInFolder(TPath.GetDirectoryName(pack), folderName, BaseFolder)
      then exit(pack);
  end;

  exit(Throw(ThrowExceptions, 'Can''t find the folder "' + folderName + '" with "' + PackageName + '" inside the folder "' + BasePath + '".'));
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

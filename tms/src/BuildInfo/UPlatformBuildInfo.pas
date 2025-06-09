unit UPlatformBuildInfo;
{$i ../../tmssetup.inc}
interface
uses Generics.Collections, UPackageBuildInfo, Deget.CoreTypes;

type
  TPlatformBuildInfo = class
  private
    FPackagesBuildInfo: TObjectList<TPackageBuildInfo>;
    FName: TPlatform;
    FOk: Boolean;
  public
    property PackagesBuildInfo: TObjectList<TPackageBuildInfo> read FPackagesBuildInfo;

    constructor Create(const aName: TPlatform);
    property Name: TPlatform read FName;
    property Ok: Boolean read FOk write FOk;
    destructor Destroy; override;

    function AllPackages: integer;

    function PackagesFolder: string;

  end;
implementation
uses SysUtils, IOUtils;

{ TPlatformBuildInfo }

function TPlatformBuildInfo.AllPackages: integer;
begin
  Result := PackagesBuildInfo.Count;
end;

constructor TPlatformBuildInfo.Create(const aName: TPlatform);
begin
  inherited Create;
  FPackagesBuildInfo := TObjectList<TPackageBuildInfo>.Create;
  FName := aName;
end;

destructor TPlatformBuildInfo.Destroy;
begin
  FPackagesBuildInfo.Free;
  inherited;
end;

function TPlatformBuildInfo.PackagesFolder: string;
begin
  Result :='';
  for var Pack in PackagesBuildInfo do
  begin
    var p := TPath.GetDirectoryName(TPath.GetFullPath(Pack.PackageFileName));
    if p = '' then raise Exception.Create('Invalid package filename: ' + p);
    if Result = '' then Result := p else if not SameText(Result, p) then raise Exception.Create('The project has packages in different folders: "' + Result + '" and "' + p +'".');

  end;

  if Result = '' then raise Exception.Create('The platform has no packages.');
   
end;

end.

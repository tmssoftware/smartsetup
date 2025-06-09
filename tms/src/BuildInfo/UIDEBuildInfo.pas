unit UIDEBuildInfo;
{$i ../../tmssetup.inc}
interface
uses Generics.Collections, UPlatformBuildInfo, Deget.CoreTypes, UProjectDefinition, UUninstallInfo,
     UConfigDefinition;

type
  TIDEBuildInfo = class
  private
    FPlatformsBuildInfo: TObjectList<TPlatformBuildInfo>;
    FName: TIDEName;
    FHelpFile: string;
    FPathToCompiler: string;
    FPostBuildFailed: boolean;
  public
    property PlatformsBuildInfo: TObjectList<TPlatformBuildInfo> read FPlatformsBuildInfo;
    property Name: TIDEName read FName;
    property PathToCompiler: string read FPathToCompiler;

    constructor Create(const aName: TIDEName; const Project: TProjectDefinition; const Config: TConfigDefinition);
    destructor Destroy; override;

    function ContainsPlatform(const platform: TPlatform; out PlatformBI: TPlatformBuildInfo): boolean;

    property PostBuildFailed: boolean read FPostBuildFailed write FPostBuildFailed;

    function AllPackages: integer;
    function AllOk: boolean;
    function AllFailed: boolean;

    property HelpFile: string read FHelpFile;


  end;

implementation
uses UTmsBuildSystemUtils;

{ TIDEBuildInfo }

function TIDEBuildInfo.ContainsPlatform(const platform: TPlatform;
  out PlatformBI: TPlatformBuildInfo): boolean;
begin
  for var oldplat in PlatformsBuildInfo do
  begin
    if oldplat.Name = platform then
    begin
      PlatformBI := oldplat;
      exit(true);
    end;
  end;
  PlatformBI := nil;
  Result := false;
end;

constructor TIDEBuildInfo.Create(const aName: TIDEName; const Project: TProjectDefinition; const Config: TConfigDefinition);
begin
  inherited Create;
  FName := aName;
  FPlatformsBuildInfo := TObjectList<TPlatformBuildInfo>.Create;
  if Project.HelpFile <> '' then
  begin
    FHelpFile := CombinePath(Project.RootFolder, Project.HelpFile);
  end;

  FPathToCompiler :=  Config.CompilerPath(Project.Application.Id, aName);
end;

destructor TIDEBuildInfo.Destroy;
begin
  FPlatformsBuildInfo.Free;
  inherited;
end;


function TIDEBuildInfo.AllOk: boolean;
begin
  for var plat in PlatformsBuildInfo do if not plat.Ok then exit(false);
  if PostBuildFailed then exit(false);

  Result := true;
end;

function TIDEBuildInfo.AllFailed: boolean;
begin
  for var plat in PlatformsBuildInfo do if plat.Ok then exit(false);
  Result := true;
end;


function TIDEBuildInfo.AllPackages: integer;
begin
  Result := 0;
  for var plat in PlatformsBuildInfo do Result := Result + plat.AllPackages;
    
end;

end.

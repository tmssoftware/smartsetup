unit UBuildInfo;
{$i ../../tmssetup.inc}

interface
uses UProjectDefinition, SysUtils, Generics.Collections, UProjectBuildInfo, UProjectList,
     UUninstallInfo;
type
  TBuildInfo = class
  private
    FProjectToProjectBuildInfo: TDictionary<string, TProjectBuildInfo>;
    FProjectsSkippedByConfig: TList<TProjectDefinition>;
    FProjectsToUninstall: TList<IUninstallInfo>;
    FProjectsToBuild: TObjectList<TProjectBuildInfo>;
    FProjectList: TProjectList;
  public
    constructor Create(const aProjectList: TProjectList);
    destructor Destroy; override;

    property ProjectList: TProjectList read FProjectList;
    property ProjectsSkippedByConfig: TList<TProjectDefinition> read FProjectsSkippedByConfig;
    property ProjectsToUninstall: TList<IUninstallInfo> read FProjectsToUninstall;
    property ProjectsToBuild: TObjectList<TProjectBuildInfo> read FProjectsToBuild;
    property ProjectToProjectBuildInfo: TDictionary<string, TProjectBuildInfo> read FProjectToProjectBuildInfo;

    function CurrentProject: TProjectBuildInfo;

    procedure ResolveDependencies;

    function IsProjectToBuild(const ProductId: string): Boolean;
    function IsProjectToUninstall(const ProductId: string): Boolean;

    function AllPackages: integer;

    function AllOk: boolean;
    function AllOkAndNoSkipped: boolean;

  end;

implementation

{ TBuildInfo }

function TBuildInfo.AllOk: boolean;
begin
  for var p in FProjectsToBuild do if not p.AllOk then exit(false);
  Result := true;
end;

function TBuildInfo.AllOkAndNoSkipped: boolean;
begin
  for var p in FProjectsToBuild do
  begin
    if not p.AllOk then exit(false);
    if p.Notes.HasSkipped then exit(false);

  end;

  Result := true;
end;

function TBuildInfo.AllPackages: integer;
begin
  Result := 0;
  for var p in FProjectsToBuild do Result := Result + p.AllPackages;
end;

constructor TBuildInfo.Create(const aProjectList: TProjectList);
begin
  FProjectList := aProjectList;
  FProjectsSkippedByConfig := TList<TProjectDefinition>.Create;
  FProjectsToUninstall := TList<IUninstallInfo>.Create;
  FProjectsToBuild := TObjectList<TProjectBuildInfo>.Create;
  FProjectToProjectBuildInfo := TDictionary<string, TProjectBuildInfo>.Create;
end;

function TBuildInfo.CurrentProject: TProjectBuildInfo;
begin
  if FProjectsToBuild.Count = 0 then raise Exception.Create('Internal error');
  Result := FProjectsToBuild.Last;
end;

destructor TBuildInfo.Destroy;
begin
  FProjectsToBuild.Free;
  FProjectsSkippedByConfig.Free;
  FProjectsToUninstall.Free;
  FProjectToProjectBuildInfo.Free;
  inherited;
end;

function TBuildInfo.IsProjectToBuild(const ProductId: string): Boolean;
begin
  for var proj in ProjectsToBuild do
    if proj.ProjectId = ProductId then
      Exit(True);
  Result := False;
end;

function TBuildInfo.IsProjectToUninstall(const ProductId: string): Boolean;
begin
  for var proj in ProjectsToUninstall do
    if proj.ProjectId = ProductId then
      Exit(True);
  Result := False;
end;

procedure TBuildInfo.ResolveDependencies;
begin
  for var ProjectInfo in ProjectsToBuild do
  begin
    var Duplicated := TDictionary<string, boolean>.Create;
    try
      Duplicated.Add(ProjectInfo.ProjectId, true);
      ProjectList.LoopDependencies(
        procedure (pd: TProjectDefinition)
        begin
          if Duplicated.ContainsKey(pd.Application.Id) then exit;
          Duplicated.Add(pd.Application.Id, true);
          var PInfo: TProjectBuildInfo;
          if not ProjectToProjectBuildInfo.TryGetValue(pd.Application.Id, PInfo)
             then raise Exception.Create('Can''t find the product "' + pd.Application.Name
             + '" (' + pd.Application.Id + ') which is required to compile "' + ProjectInfo.Project.Application.Name
             + '" (' + ProjectInfo.Project.Application.Id + '). Make sure it isn''t disabled in the configuration ' +
             'and that the project ids are spelled right.');
          ProjectInfo.Dependencies.Add(PInfo);
        end
        , ProjectInfo.Project, nil);

    finally
      Duplicated.Free;
    end;
  end;
end;

end.

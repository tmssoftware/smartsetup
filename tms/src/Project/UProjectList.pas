unit UProjectList;
{$i ../../tmssetup.inc}

interface
uses SysUtils, Generics.Collections, UProjectDefinition;

type
  TProjectList = class
  private
    const
      MaxNestedDeps = 30;
    procedure AddDepsToMap(const Map: TDictionary<string, TProjectDefinition>;
      const Uninstall: TList<TProjectDefinition>);
    var
      FList: TObjectList<TProjectDefinition>;
      FResolved: TProjectDefinitionList;
    procedure AddDep(const Action: TProc<TProjectDefinition>; const Project: TProjectDefinition; const Map: TDictionary<string, TProjectDefinition>; const Cycles: TStack<string>);
    function ListAll(const Start: string; const Cycles: TStack<string>): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadDeps(const Map: TDictionary<string, TProjectDefinition>);

    procedure Add(const Project: TProjectDefinition);
    function LastUnresolved: TProjectDefinition;
    procedure ResolveDependencies;

    property Resolved: TProjectDefinitionList read FResolved;
    procedure LoopDependencies(const Action: TProc<TProjectDefinition>;
      const Project: TProjectDefinition; const Uninstall: TList<TProjectDefinition>);

    property All: TObjectList<TProjectDefinition> read FList;

  end;

implementation

{ TProjectList }

procedure TProjectList.Add(const Project: TProjectDefinition);
begin
  FList.Add(Project);
end;

constructor TProjectList.Create;
begin
  inherited;
  FList := TObjectList<TProjectDefinition>.Create;
  FResolved := TProjectDefinitionList.Create;
end;

destructor TProjectList.Destroy;
begin
  Resolved.Free;
  FList.Destroy;
  inherited;
end;

function TProjectList.LastUnresolved: TProjectDefinition;
begin
  Result := FList.Last;
end;

function TProjectList.ListAll(const Start: string; const Cycles: TStack<string>): string;
begin
  Result := '';
  for var i := Cycles.Count - 1 downto 0 do
  begin
    Result := Cycles.List[i] + '->' + Result;
    if Cycles.List[i] = Start then exit;

  end;
end;

procedure TProjectList.LoadDeps(const Map: TDictionary<string, TProjectDefinition>);
var
  RepeatedProject: TProjectDefinition;
begin
  for var proj in FList do
  begin
    if (Map.TryGetValue(proj.Application.Id, RepeatedProject)) then
    begin
      raise Exception.Create('The project id "' + proj.Application.Id + '" is repeated in files "' + proj.FullPath + '" and "' + RepeatedProject.FullPath + '". Make sure only one is available.' );
    end;
    Map.Add(proj.Application.Id, proj);
  end;
end;

procedure TProjectList.AddDepsToMap(const Map: TDictionary<string, TProjectDefinition>; const Uninstall: TList<TProjectDefinition>);
begin
  if Uninstall = nil then exit;
  for var Project in Uninstall do
  begin
    Map.Add(Project.Application.Id, Project);
  end;
end;

procedure TProjectList.LoopDependencies(const Action: TProc<TProjectDefinition>;
   const Project: TProjectDefinition; const Uninstall: TList<TProjectDefinition>);
var
  Map: TDictionary<string, TProjectDefinition>;
  Cycles: TStack<string>;
begin
  Map := TDictionary<string, TProjectDefinition>.Create;
  try
    Cycles := TStack<string>.Create;
    try
      LoadDeps(Map);
      AddDepsToMap(Map, Uninstall);
      for var proj in FList do
      begin
        proj.ResolvedState := TResolvedState.Unresolved;
      end;

      AddDep(procedure (aProject: TProjectDefinition) begin Action(aProject); end, Project, Map, Cycles);

    finally
      Cycles.Free;
    end;
  finally
    Map.Free;
  end;
end;

procedure TProjectList.ResolveDependencies;
var
  Map: TDictionary<string, TProjectDefinition>;
  Cycles: TStack<string>;
begin
  Resolved.Clear;
  Map := TDictionary<string, TProjectDefinition>.Create;
  try
    Cycles := TStack<string>.Create;
    try
      LoadDeps(Map);
      for var proj in FList do
      begin
        proj.ResolvedState := TResolvedState.Unresolved;
      end;

      for var proj in FList do
      begin
        AddDep(procedure (aProject: TProjectDefinition) begin Resolved.Add(aProject); end, proj, Map, Cycles);
      end;

    finally
      Cycles.Free;
    end;
  finally
    Map.Free;
  end;
end;

procedure TProjectList.AddDep(const Action: TProc<TProjectDefinition>; const Project: TProjectDefinition;
  const Map: TDictionary<string, TProjectDefinition>;
  const Cycles: TStack<string>);
var
  NewProject: TProjectDefinition;
begin
  if Project.ResolvedState = TResolvedState.Resolved then exit;
  if Project.ResolvedState = TResolvedState.Resolving then
  begin
    raise Exception.Create('Circular dependency between products: ' + ListAll(Project.Application.Id, Cycles) + Project.Application.Id);
  end;

  Project.ResolvedState := TResolvedState.Resolving;
  Cycles.Push(Project.Application.Id);
  try
    if (Cycles.Count > MaxNestedDeps) then raise Exception.Create('Too many nested dependencies. Only ' + IntToStr(MaxNestedDeps) +' levels are allowed.');

    for var dep in Project.Dependencies do
    begin
      if not Map.TryGetValue(dep.Id, NewProject) then
      begin
        raise Exception.Create('The product "' + Project.Application.Name + '" requires the product "' + dep.Description + '" to build, and it isn''t present. Add product "' + dep.Description + '" to the list of installed products.' );
      end;
      AddDep(Action, NewProject, Map, Cycles);
    end;

    for var dep in Project.WeakDependencies do
    begin
      if Map.TryGetValue(dep.Id, NewProject) then
      begin
        AddDep(Action, NewProject, Map, Cycles);
      end;
    end;

    Action(Project);

  finally
    Project.ResolvedState := TResolvedState.Resolved;
    Cycles.Pop;
  end;

end;

end.

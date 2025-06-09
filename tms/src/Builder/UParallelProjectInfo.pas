unit UParallelProjectInfo;
{$i ../../tmssetup.inc}

interface
uses Generics.Collections, Threading, SysUtils, Classes;

type

TTaskAndName = record
  public
  Tasks: TArray<ITask>;
  Names: TArray<string>;
end;

TParallelProjectInfo = class
  private
    FRun: TProc;
    Dependencies: TList<string>;
public
  constructor Create;
  destructor Destroy; override;

  property Run: TProc read FRun write FRun;
  function WithoutDependencies: boolean;
end;

TParallelProjectInfoList = class
private
  Projects: TObjectDictionary<string, TParallelProjectInfo>;

  function GetProjectsWithoutDependencies(const OldTasks: TTaskAndName): TTaskAndName;
  function HashStr(const proj, ide, plat: string): string;
  procedure RemoveTaskFromAllDeps(const Name: string);
  function CaptureRun(const p: TProc): TProc;
public
  constructor Create;
  destructor Destroy; override;
  procedure Add(const proj, ide, plat: string;const aProjectInfo: TParallelProjectInfo);
  procedure AddDep(const ProjBase, ProjThatDepends, ide, plat: string);
  procedure SetRun(const proj, ide, plat: string; const aRun: TProc);
  procedure Run;
end;

implementation
uses UAppTerminated, UMultiLogger, ULoggerTask;

{ TParallelProjectInfoList }

procedure TParallelProjectInfoList.Add(const proj, ide, plat: string;
  const aProjectInfo: TParallelProjectInfo);
begin
  Projects.Add(HashStr(proj, ide, plat), aProjectInfo);
end;

procedure TParallelProjectInfoList.AddDep(const ProjBase, ProjThatDepends, ide,
  plat: string);
begin
  var MainProj: TParallelProjectInfo;
  if not (Projects.TryGetValue(HashStr(ProjBase, ide, plat), MainProj)) then exit;
  MainProj.Dependencies.Add(HashStr(ProjThatDepends, ide, plat))
end;

function TParallelProjectInfoList.CaptureRun(const p: TProc): TProc;
begin
  Result := procedure
  begin
    try
      p();
    except
      on ex: Exception do
        Logger.Error('Unhandled exception: ' + ex.Message);
    end;
  end;
end;

constructor TParallelProjectInfoList.Create;
begin
  Projects := TObjectDictionary<string, TParallelProjectInfo>.Create([doOwnsValues]);
end;

destructor TParallelProjectInfoList.Destroy;
begin
  Projects.Free;
  inherited;
end;


function TParallelProjectInfoList.GetProjectsWithoutDependencies(
  const OldTasks: TTaskAndName): TTaskAndName;
begin
  Result.Tasks := nil;
  Result.Names := nil;
  SetLength(Result.Tasks, Length(OldTasks.Tasks) + Projects.Count);
  SetLength(Result.Names, Length(OldTasks.Names) + Projects.Count);

  var k := 0;
  for var z := Low(OldTasks.Tasks) to High(OldTasks.Tasks) do
  begin
    if (OldTasks.Tasks[z].Status in [TTaskStatus.Completed, TTaskStatus.Canceled, TTaskStatus.Exception])then
    begin
      RemoveTaskFromAllDeps(OldTasks.Names[z]);
    end else
    begin
      Result.Tasks[k] := OldTasks.Tasks[z];
      Result.Names[k] := OldTasks.Names[z];
      inc(k);
    end;

  end;

  var KeysToRemove := TList<string>.Create;
  try
    for var p in Projects do
    begin
      if not Assigned(p.Value.Run) then
      begin
        KeysToRemove.Add(p.Key);
        continue;
      end;

      if p.Value.WithoutDependencies then
      begin
        Result.Names[k] := p.Key;
        Result.Tasks[k] := RunTask(CaptureRun(p.Value.Run));
        inc(k);
        KeysToRemove.Add(p.Key);
      end;
    end;

    for var key in KeysToRemove do
    begin
      Projects.Remove(key);
    end;

  finally
    KeysToRemove.Free;
  end;

  SetLength(Result.Tasks, k);
  SetLength(Result.Names, k);

end;

function TParallelProjectInfoList.HashStr(const proj, ide,
  plat: string): string;
begin
  Result := proj + '@@' + ide + '@@' + plat;
end;

procedure TParallelProjectInfoList.RemoveTaskFromAllDeps(
  const Name: string);
begin
 for var p in Projects do
 begin
   for var i := p.Value.Dependencies.Count - 1 downto 0 do
   begin
     if p.Value.Dependencies[i] = Name then p.Value.Dependencies.Delete(i);
   end;
 end;
end;

procedure TParallelProjectInfoList.Run;
begin
  var Tasks: TTaskAndName;
  Tasks.Tasks := nil;
  Tasks.Names := nil;

  while (Projects.Count > 0) or (Length(Tasks.Tasks) > 0) do
  begin
    if AppIsTerminated then
    begin
      if Tasks.Tasks <> nil then
      begin
        for var task in Tasks.Tasks do task.Cancel;
        TTask.WaitForAll(Tasks.Tasks);
        exit;
      end;
    end
    else
    begin
      Tasks := GetProjectsWithoutDependencies(Tasks);
    end;
    TTask.WaitForAny(Tasks.Tasks, 1000);
  end;
end;

procedure TParallelProjectInfoList.SetRun(const proj, ide, plat: string;
  const aRun: TProc);
begin
  Projects[HashStr(proj, ide, plat)].Run := aRun;
end;

{ TParallelProjectInfo }

constructor TParallelProjectInfo.Create;
begin
  Dependencies := TList<string>.Create;
end;

destructor TParallelProjectInfo.Destroy;
begin
  Dependencies.Free;
  inherited;
end;

function TParallelProjectInfo.WithoutDependencies: boolean;
begin
  Result := Dependencies.Count = 0;
end;

end.

unit Fetching.ParallelProjectInfo;

{$I ../../tmssetup.inc}

interface

uses Generics.Collections, Threading, SysUtils, Classes;

{ Note:
  Note sure about the purpose of this unit and classes. TTaskAndName.Names is not used.
  Also not sure why a loop that keeps updating the parallel tasks, is this planned to add more tasks "on-demand"
  (to solve dependencies on the fly and add dependencies to the list of tasks before finishing it?)
  Anyway, keeping this as-is but dependencies are being solved differently.
}

type
  TTaskAndName = record
  public
    Tasks: TArray<ITask>;
    Names: TArray<string>;
  end;

  TParallelProjectInfo = class
  private
    FRun: TProc;
  public
    constructor Create;
    destructor Destroy; override;

    property Run: TProc read FRun write FRun;
  end;

  TParallelProjectInfoList = class
  private
    Projects: TObjectDictionary<string, TParallelProjectInfo>;

    function CaptureRun(const p: TProc): TProc;

    function GetProjectsWithoutDependencies(const OldTasks: TTaskAndName): TTaskAndName;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const proj: string; const aProjectInfo: TParallelProjectInfo);
    procedure SetRun(const proj: string; const aRun: TProc);
    procedure Run;
  end;

implementation

uses UAppTerminated, ULoggerTask;

{ TParallelProjectInfoList }

procedure TParallelProjectInfoList.Add(const proj: string; const aProjectInfo: TParallelProjectInfo);
begin
  Projects.Add(proj, aProjectInfo);
end;

function TParallelProjectInfoList.CaptureRun(const p: TProc): TProc;
begin
  Result := procedure
    begin
      try
        p();
      except
        // ignore;
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

function TParallelProjectInfoList.GetProjectsWithoutDependencies(const OldTasks: TTaskAndName): TTaskAndName;
begin
  Result.Tasks := nil;
  Result.Names := nil;
  SetLength(Result.Tasks, Length(OldTasks.Tasks) + Projects.Count);
  SetLength(Result.Names, Length(OldTasks.Names) + Projects.Count);

  var
  k := 0;
  for var z := Low(OldTasks.Tasks) to High(OldTasks.Tasks) do
  begin
    if (OldTasks.Tasks[z].Status in [TTaskStatus.Completed, TTaskStatus.Canceled, TTaskStatus.Exception]) then
    begin
    end
    else
    begin
      Result.Tasks[k] := OldTasks.Tasks[z];
      Result.Names[k] := OldTasks.Names[z];
      inc(k);
    end;

  end;

  var
  KeysToRemove := TList<string>.Create;
  try
    for var p in Projects do
    begin
      if not Assigned(p.Value.Run) then
      begin
        KeysToRemove.Add(p.Key);
        continue;
      end;

      Result.Names[k] := p.Key;
      Result.Tasks[k] := RunTask(CaptureRun(p.Value.Run));
      inc(k);
      KeysToRemove.Add(p.Key);
    end;

    for var Key in KeysToRemove do
    begin
      Projects.Remove(Key);
    end;

  finally
    KeysToRemove.Free;
  end;

  SetLength(Result.Tasks, k);
  SetLength(Result.Names, k);

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
        for var task in Tasks.Tasks do
          task.Cancel;
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

procedure TParallelProjectInfoList.SetRun(const proj: string; const aRun: TProc);
begin
  Projects[proj].Run := aRun;
end;

{ TParallelProjectInfo }

constructor TParallelProjectInfo.Create;
begin
end;

destructor TParallelProjectInfo.Destroy;
begin
  inherited;
end;

end.

unit Doctor.CorePathCheck;

interface
{$IFDEF MSWINDOWS}
uses Doctor.Check, SysUtils, Classes;

type
TSplitPath = record
  Path: string;
  Fix: boolean;
end;

TPathFix = class(TFix)
private
  SplitPaths: TArray<TSplitPath>;
  PositionInSplitPaths: integer;
  procedure FixPath;
public
  constructor Create(const aMessage, aAction: string; const aSplitPaths: TArray<TSplitPath>;
           const aPositionInSplitPaths: integer);
end;

TCorePathCheck = class(TCheck)
private
  OriginalPath: string;
  SplitPaths: TArray<TSplitPath>;

  procedure CheckPath(const Path: string);
  function GetNewPath: string;
protected
  GetPath: TFunc<string>;
  SetPath: TProc<string>;

  function FixPath(const Path: string): string; virtual;
  function IsPathValid(const Path: string; out Reason, Action: string): boolean; virtual; abstract;

public
  procedure Check; override;
  procedure Fix(const UndoInfo: TUndoInfo); override;
  function IsSlow: boolean; override;
end;
implementation

{ TPathFix }

constructor TPathFix.Create(const aMessage, aAction: string; const aSplitPaths: TArray<TSplitPath>;
            const aPositionInSplitPaths: integer);
begin
  inherited Create(aMessage, aAction, FixPath);
  SplitPaths := aSplitPaths;
  PositionInSplitPaths := aPositionInSplitPaths;
end;

procedure TPathFix.FixPath;
begin
  SplitPaths[PositionInSplitPaths].Fix := true;
end;

{ TPathCheck }

procedure TCorePathCheck.Check;
begin
  CheckPath(GetPath);
end;

function TCorePathCheck.GetNewPath: string;
begin
  Result := '';
  for var i := Low(SplitPaths) to High(SplitPaths) do
  begin
    var NewSinglePath := SplitPaths[i].Path;

    if SplitPaths[i].Fix then
    begin
      NewSinglePath := FixPath(NewSinglePath);
      if NewSinglePath = '' then continue;
    end;

    if Result <> '' then Result := Result + ';';
    Result := Result + NewSinglePath;

  end;
end;

function TCorePathCheck.IsSlow: boolean;
begin
  Result := false;
end;

procedure TCorePathCheck.Fix(const UndoInfo: TUndoInfo);
begin
  if FixApplied then raise Exception.Create('The fix for "' + Name + '" has already been applied.');

  if OriginalPath <> GetPath then raise Exception.Create('Can''t fix the Path. It has been modified since this check run.');

  for var i := Low(SplitPaths) to High(SplitPaths) do SplitPaths[i].Fix := false;

  var ItemsToFix := 0;
  for var i := Fixes.Count - 1 downto 0 do
  begin
    if Fixes[i].Apply and (Assigned(Fixes[i].Fix)) then
    begin
      Fixes[i].Fix();
      inc(ItemsToFix);
    end;
  end;

  if ItemsToFix > 0 then
  begin
    SetPath(GetNewPath);
    UndoInfo.Value.AddPair(Name, OriginalPath);
  end;

  Inc(FFixesApplied, ItemsToFix);
  FFixApplied := true;
end;

function TCorePathCheck.FixPath(const Path: string): string;
begin
  Result := ''; //remove the path.
end;

procedure TCorePathCheck.CheckPath(const Path: string);
begin
  OriginalPath := Path;
  var Paths := Path.Split([';'], TStringSplitOptions.None);
  SplitPaths := nil;
  SetLength(SplitPaths, Length(Paths));
  for var i := Low(Paths) to High(Paths) do SplitPaths[i].Path := Paths[i];


  for var i := Low(Paths) to High(Paths) do
  begin
    var SinglePath := Paths[i];
    if (SinglePath.Trim = '') then
    begin
      if i = High(Paths) then continue; //allow semicolon at the end, to avoid reporting silly stuff.

      Fixes.Add(TPathFix.Create('There is an empty entry in the Path',
        'Remove?', SplitPaths, i
        ));
        continue;
    end;

    var Reason := ''; var Action := '';
    if not IsPathValid(SinglePath.Trim, Reason, Action) then
    begin
      Fixes.Add(TPathFix.Create(
        Reason,
        Action, SplitPaths, i
      ))
    end;

  end;
end;

{$ELSE}
implementation
{$ENDIF}
end.

unit Doctor.LibraryPathCheck;

interface

{$IFDEF MSWINDOWS}

uses Doctor.Check, Doctor.CorePathCheck, Doctor.MultiIDECheck, Deget.IDEInfo, SysUtils, Classes, Deget.CoreTypes;

type
  TLibPathFix = class(TPathFix)
  private
    FPlatform: TPlatform;
    FPathType: TDelphiPathType;
  public
    constructor Create(const aMessage, aAction: string; const aSplitPaths: TArray<TSplitPath>;
           const aPositionInSplitPaths: integer; const aPlatform: TPlatform; const aPathType: TDelphiPathType);

    property Platform: TPlatform read FPlatform;
    property PathType: TDelphiPathType read FPathType;
  end;

  TLibraryPathCheck = class(TSingleIDECheck)
  private
    OriginalPaths: Array[TPlatform, TDelphiPathType] of string;
    SplitPaths: Array[TPlatform, TDelphiPathType] of TArray<TSplitPath>;

    procedure CheckPath(const Plat: TPlatform; const PathType: TDelphiPathType; const Path: string);
    procedure FixPlat(const Plat: TPlatform; const PathType: TDelphiPathType; const Platform: IDelphiPlatformInfo);
    function GetNewPath(const Plat: TPlatform;
      const PathType: TDelphiPathType): string;
    function Snip(const Path: string; const Paths: TArray<string>; const i: integer): string;
  protected
    function IsPathValid(const Plat: TPlatform; const PathType: TDelphiPathType; const Path, SnippedPath: string; const IsLastPath: boolean; out Reason, Action: string): boolean; virtual; abstract;
    function FixPath(const Path: string): string; virtual; abstract;
    function GetLibPath(const Plat: TPlatform; const PathType: TDelphiPathType): string;
  public
    constructor Create(const AIDEInfo: IDelphiIDEInfo); override;

    procedure Check; override;
    procedure Fix(const UndoInfo: TUndoInfo); override;
  end;

  TLibraryPathMultiSlashCheck = class(TLibraryPathCheck)
  protected
    function IsPathValid(const Plat: TPlatform; const PathType: TDelphiPathType; const Path, SnippedPath: string; const IsLastPath: boolean; out Reason, Action: string): boolean; override;
    function FixPath(const Path: string): string; override;
  public
    function Name: string; override;
    function Description: string; override;
  end;

  TLibraryPathNotExistingFoldersCheck = class(TLibraryPathCheck)
  protected
    function IsPathValid(const Plat: TPlatform; const PathType: TDelphiPathType; const Path, SnippedPath: string; const IsLastPath: boolean; out Reason, Action: string): boolean; override;
    function FixPath(const Path: string): string; override;
  public
    function Name: string; override;
    function Description: string; override;
  end;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses IOUtils, Deget.DelphiInfo;

{ TLibraryPathCheck }
constructor TLibraryPathCheck.Create(const AIDEInfo: IDelphiIDEInfo);
begin
  inherited Create(AIDEInfo);
end;

procedure TLibraryPathCheck.Check;
begin
  for var Plat := Low(TPlatform) to High(TPlatform) do
  begin
    var Platform := IDEInfo.GetPlatform(Plat);
    for var PathType :=  Low(TDelphiPathType) to High(TDelphiPathType) do
    begin
      CheckPath(Plat, PathType, Platform.GetIDEPath(PathType));
    end;
  end;
end;

procedure TLibraryPathCheck.CheckPath(const Plat: TPlatform; const PathType: TDelphiPathType; const Path: string);
begin
  if Path.Trim = '' then exit;

  OriginalPaths[Plat, PathType] := Path;
  var Paths := Path.Split([';'], TStringSplitOptions.None);
  SplitPaths[Plat, PathType] := nil;
  SetLength(SplitPaths[Plat, PathType], Length(Paths));
  for var i := Low(Paths) to High(Paths) do SplitPaths[Plat, PathType][i].Path := Paths[i];


  for var i := Low(Paths) to High(Paths) do
  begin
    var SinglePath := Paths[i];

    var Reason := ''; var Action := '';
    if not IsPathValid(Plat, PathType, SinglePath.Trim, Snip(Path, Paths, i), i = High(Paths), Reason, Action) then
    begin
      Fixes.Add(TLibPathFix.Create(
        Reason,
        Action, SplitPaths[Plat, PathType], i, Plat, PathType
      ))
    end;

  end;

end;

function TLibraryPathCheck.Snip(
  const Path: string; const Paths: TArray<string>; const i: integer): string;
begin
  Result := '';
  if i - 1 >= 0 then Result := Result + Paths[i - 1] + ';';
  if i - 1 > 0 then Result := '...;' + Result;
  Result := Result + Paths[i];
  if i + 1 <= High(Paths) then Result := Result + ';' + Paths[i + 1];
  if i + 1 < High(Paths) then Result := Result + ';...';

end;

function TLibraryPathCheck.GetNewPath(const Plat: TPlatform; const PathType: TDelphiPathType): string;
begin
  Result := '';
  for var i := Low(SplitPaths[Plat, PathType]) to High(SplitPaths[Plat, PathType]) do
  begin
    var NewSinglePath := SplitPaths[Plat, PathType][i].Path;

    if SplitPaths[Plat, PathType][i].Fix then
    begin
      NewSinglePath := FixPath(NewSinglePath);
      if NewSinglePath = '' then continue;
    end;

    if Result <> '' then Result := Result + ';';
    Result := Result + NewSinglePath;

  end;
end;


procedure TLibraryPathCheck.Fix(const UndoInfo: TUndoInfo);
begin
  for var Plat := Low(TPlatform) to High(TPlatform) do
  begin
    var Platform := IDEInfo.GetPlatform(Plat);
    for var PathType :=  Low(TDelphiPathType) to High(TDelphiPathType) do
    begin
      FixPlat(Plat, PathType, Platform);
    end;
  end;

  FFixApplied := true;

end;

procedure TLibraryPathCheck.FixPlat(const Plat: TPlatform; const PathType: TDelphiPathType; const Platform: IDelphiPlatformInfo);
begin
  if FixApplied then raise Exception.Create('The fix for "' + Name + '" has already been applied.');

  if OriginalPaths[Plat, PathType] <> Platform.GetIDEPath(PathType) then raise Exception.Create('Can''t fix the ' + GetLibPath(Plat, PathType) + '. It has been modified since this check run.');

  for var i := Low(SplitPaths[Plat, PathType]) to High(SplitPaths[Plat, PathType]) do SplitPaths[Plat, PathType][i].Fix := false;

  var ItemsToFix := 0;
  for var i := Fixes.Count - 1 downto 0 do
  begin
    var Fix := Fixes[i] as TLibPathFix;
    if Fix.Apply and (Assigned(Fix.Fix)) and (Fix.PathType = PathType) and (Fix.Platform = Plat) then
    begin
      Fixes[i].Fix();
      Inc(ItemsToFix);
    end;
  end;

  if ItemsToFix > 0 then
  begin
    Platform.SetIDEPath(PathType, GetNewPath(Plat, PathType));
    Inc(FFixesApplied, ItemsToFix);
  end;

end;

function TLibraryPathCheck.GetLibPath(const Plat: TPlatform;
  const PathType: TDelphiPathType): string;
begin
  Result := TDelphiIDEPlatformInfo.IDEPathNames[PathType] + ' for ' + IDEId[IDEInfo.IDEName] + '.' + PlatformId[plat];
end;

{ TLibraryPathMultiSlashCheck }

function TLibraryPathMultiSlashCheck.FixPath(const Path: string): string;
begin
  Result := '';
  if Length(Path) = 0 then exit;

  SetLength(Result, Length(Path));
  Result[1] := Path[1];
  var p := 1;
  for var i := 1 to Path.Length - 1 do
  begin
    //we allow \\ at the start since that's a UNC path. It will likely break msbuild anyway, but we allow it.
    if (i > 1) and (Path.Chars[i] = '\') and (Path.Chars[i - 1] = '\') then continue;

    Result[p + 1] := Path.Chars[i];
    inc(p);
  end;

  SetLength(Result, p);

end;

function TLibraryPathMultiSlashCheck.IsPathValid(const Plat: TPlatform; const PathType: TDelphiPathType; const Path, SnippedPath: string; const IsLastPath: boolean; out Reason,
  Action: string): boolean;
begin
  Reason := '';
  Action := '';
  Result := true;

  if Path.IndexOf('\\') > 0 then  //we allow \\ at the start since that's a UNC path.
  begin
    Reason := 'The entry "' + Path + '" in the ' + GetLibPath(Plat, PathType) +
               ' has multiple slashes ("\\"). This can break the builds.';
    Action := 'Fix?';
    exit(False);
  end;

end;

function TLibraryPathMultiSlashCheck.Name: string;
begin
  Result := 'Multiple Slashes in Library Path';
end;

function TLibraryPathMultiSlashCheck.Description: string;
begin
  Result := 'This test checks for entries in the Rad Studio Library Path that have multiple slashes (\\). '
    + 'Multiple slashes are escaped by msbuild and can cause build errors.';
end;

{ TLibraryPathNotExistingFoldersCheck }

function TLibraryPathNotExistingFoldersCheck.FixPath(
  const Path: string): string;
begin
  Result := '';
end;

function TLibraryPathNotExistingFoldersCheck.IsPathValid(const Plat: TPlatform;
  const PathType: TDelphiPathType; const Path, SnippedPath: string; const IsLastPath: boolean; out Reason,
  Action: string): boolean;
begin
  Reason := '';
  Action := '';
  Result := true;
  if (Path.Trim = '') then
    begin
      if IsLastPath then exit; //allow semicolon at the end, to avoid reporting silly stuff.

      Reason := 'There is an empty entry (";;") in the ' + GetLibPath(Plat, PathType) + ' near: "' + SnippedPath + '"';
      Action :='Remove?';
      exit(False);
    end;


  if (not Path.Contains('$')) and (not TDirectory.Exists(Path)) then
  begin
    Reason := 'The entry "' + Path + '" in the ' + GetLibPath(Plat, PathType) +
               ' doesn''t exist.';
    Action := 'Remove?';
    exit(False);
  end;


end;

function TLibraryPathNotExistingFoldersCheck.Name: string;
begin
  Result := 'Paths that don''t exist in the Library path';

end;

function TLibraryPathNotExistingFoldersCheck.Description: string;
begin
  Result := 'This test checks for entries in the Rad Studio Library Path that point to folders that don''exist. '
    + 'Those folders can make the library path too long and slow the builds.';

end;


{ TLibPathFix }

constructor TLibPathFix.Create(const aMessage, aAction: string;
  const aSplitPaths: TArray<TSplitPath>; const aPositionInSplitPaths: integer; const aPlatform: TPlatform;
  const aPathType: TDelphiPathType);
begin
  inherited Create(aMessage, aAction, aSplitPaths, aPositionInSplitPaths);
  FPlatform := aPlatform;
  FPathType := aPathType;
end;

{$ENDIF}

end.

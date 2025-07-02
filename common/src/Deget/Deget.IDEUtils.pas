unit Deget.IDEUtils;

interface

uses
  SysUtils, StrUtils;

function AddPaths(const CurrentPath: string; PathToAdd: string; AddToBeginning: Boolean = False): string;
function RemovePaths(const CurrentPath: string; PathToRemove: string; const IncludeChildren: boolean = false): string;
function GetPaths(const PathList: string): TArray<string>;
function SameFolder(const F1, F2: string): Boolean;
function StartsWithFolder(const SubFolder, Folder: string): Boolean;

implementation

function SameFolder(const F1, F2: string): Boolean;
begin
  Result := SameText(IncludeTrailingPathDelimiter(F1), IncludeTrailingPathDelimiter(F2));
end;

function StartsWithFolder(const SubFolder, Folder: string): Boolean;
begin
  Result := StartsText(IncludeTrailingPathDelimiter(SubFolder), IncludeTrailingPathDelimiter(Folder));
end;

function GetPaths(const PathList: string): TArray<string>;
begin
  Result := TArray<string>(SplitString(PathList, ';'));
end;

function AddPaths(const CurrentPath: string; PathToAdd: string; AddToBeginning: Boolean = False): string;
var
  NewPathItem, ExistingPathItem: string;
  Exists: boolean;
begin
  Result := CurrentPath;
  for NewPathItem in GetPaths(PathToAdd) do
  begin
    if NewPathItem = '' then Continue;    

    Exists := false;
    for ExistingPathItem in GetPaths(CurrentPath) do
      if SameFolder(ExistingPathItem, NewPathItem) then
      begin
        Exists := true;
        Break;
      end;
    if not Exists then
    begin
      if AddToBeginning then
      begin
        if (Result <> '')  and not Result.StartsWith(';') then
          Result := ';' + Result;
        Result := NewPathItem + Result;
      end
      else
      begin
        if (Result <> '') and not Result.EndsWith(';') then
          Result := Result + ';';
        Result := Result + NewPathItem;
      end;
    end;
  end;
end;

function RemovePaths(const CurrentPath: string; PathToRemove: string; const IncludeChildren: boolean): string;
var
  PathItemToRemove, ExistingPathItem: string;
  KeepPath: boolean;
begin
  Result := '';
  for ExistingPathItem in GetPaths(CurrentPath) do
  begin
    KeepPath := true;
    for PathItemToRemove in GetPaths(PathToRemove) do
    begin
      if SameFolder(ExistingPathItem, PathItemToRemove)
      or (IncludeChildren and StartsWithFolder(PathItemToRemove, ExistingPathItem)) then
      begin
        KeepPath := false;
        break;
      end;
    end;

    if KeepPath then
    begin
      if Result <> '' then
        Result := Result + ';';
      Result := Result + ExistingPathItem;
    end;
  end;
end;

end.

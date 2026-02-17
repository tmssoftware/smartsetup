unit Doctor.DuplicatedFiles;

interface
uses SysUtils, Classes, Doctor.Check;
type
TDuplicatedFileFix = class(TFix)
  protected
    function GetQuestions(const aFolders: TArray<string>): TArray<string>;
    function GetList(const aItems: TArray<string>): string;
end;

implementation

function TDuplicatedFileFix.GetList(const aItems: TArray<string>): string;
begin
  Result := '';
  for var i := 0  to High(aItems) do
  begin
    if i > 0 then
    begin
      if i < High(aItems) then Result := Result + ', '
      else Result := Result + ' and ';
    end;
    Result := Result + '"' + aItems[i] + '"';
  end;
end;

function TDuplicatedFileFix.GetQuestions(const aFolders: TArray<string>): TArray<string>;
begin
  Result := nil;
  SetLength(Result, Length(aFolders));
  for var i := Low(Result) to High(Result) do
  begin
    Result[i] := 'KEEP ' + aFolders[i];
  end;
end;


end.

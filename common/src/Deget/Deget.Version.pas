unit Deget.Version;

interface

type
  TVersionNumbers = array[0..3] of integer;

  TVersion = record
  strict private
    FNumbers: TVersionNumbers;
  private
    function GetNumber(I: integer): integer;
    procedure SetNumber(I: integer; const Value: integer);
  public
    class operator Implicit(Value: string): TVersion;
    class operator Implicit(Value: TVersion): string;
    class operator Equal(Left, Right: TVersion): Boolean;
    class operator NotEqual(Left, Right: TVersion): Boolean;

    class operator GreaterThan(Left, Right: TVersion): Boolean;
    class operator GreaterThanOrEqual(Left, Right: TVersion): Boolean;

    class operator LessThan(Left, Right: TVersion): Boolean;
    class operator LessThanOrEqual(Left, Right: TVersion): Boolean;
  public
    function ToString: string;
    function Normalized: string;
    class function Make(const AMajor, AMinor, ARelease, ABuild: Integer): TVersion; static;
    procedure FromString(const S: string);
    function IsNull: boolean;
    procedure Clear;
    property Major: integer index 0 read GetNumber write SetNumber;
    property Minor: integer index 1 read GetNumber write SetNumber;
    property Release: integer index 2 read GetNumber write SetNumber;
    property Build: integer index 3 read GetNumber write SetNumber;
    property Numbers[I: integer]: integer read GetNumber write SetNumber; default;
  end;

implementation

uses
  System.SysUtils, System.StrUtils;

{ TVersion }

procedure TVersion.Clear;
begin
  FNumbers[0] := 0;
  FNumbers[1] := 0;
  FNumbers[2] := 0;
  FNumbers[3] := 0;
end;

class operator TVersion.Equal(Left, Right: TVersion): Boolean;
begin
  Result := (Left[0] = Right[0]) and (Left[1] = Right[1]) and (Left[2] = Right[2]) and (Left[3] = Right[3]);
end;

procedure TVersion.FromString(const S: string);
var
  StrNumber: string;
  Digit: integer;
  I: integer;
  Temp: TVersionNumbers;
begin
  // quick and dirty
  I := 0;
  for StrNumber in SplitString(S, '.') do
  begin
    if not TryStrToInt(StrNumber, Digit) or (I > 3) then
      raise EConvertError.CreateFmt('''%s'' is not a valid version number', [S]);
    Temp[I] := Digit;
    Inc(I);
  end;
  while I <= 3 do
  begin
    Temp[I] := 0;
    Inc(I);
  end;
  FNumbers := Temp;
end;

function TVersion.GetNumber(I: integer): integer;
begin
  if (I < 0) or (I > 3) then
    raise EArgumentOutOfRangeException.Create('Index out of range');
  Result := FNumbers[I];
end;

class operator TVersion.GreaterThan(Left, Right: TVersion): Boolean;
var
  I: integer;
begin
  for I := 0 to 3 do
    if Left[I] > Right[I] then
      Exit(true)
    else
    if Left[I] < Right[I] then
      Exit(false);
  Result := false;
end;

class operator TVersion.GreaterThanOrEqual(Left, Right: TVersion): Boolean;
var
  I: integer;
begin
  for I := 0 to 3 do
    if Left[I] > Right[I] then
      Exit(true)
    else
    if Left[I] < Right[I] then
      Exit(false);
  Result := true;
end;

class operator TVersion.Implicit(Value: string): TVersion;
begin
  Result.FromString(Value);
end;

class operator TVersion.Implicit(Value: TVersion): string;
begin
  Result := Value.ToString;
end;

function TVersion.IsNull: boolean;
begin
  Result := (FNumbers[0] = 0) and (FNumbers[1] = 0) and (FNumbers[2] = 0) and (FNumbers[3] = 0);
end;

class operator TVersion.LessThan(Left, Right: TVersion): Boolean;
begin
  Result := Right > Left;
end;

class operator TVersion.LessThanOrEqual(Left, Right: TVersion): Boolean;
begin
  Result := Right >= Left;
end;

class function TVersion.Make(const AMajor, AMinor, ARelease, ABuild: Integer): TVersion;
begin
  Result.Major := AMajor;
  Result.Minor := AMinor;
  Result.Release := ARelease;
  Result.Build := ABuild;
end;

function TVersion.Normalized: string;
begin
  Result := IntToStr(FNumbers[0]) + '.' + IntToStr(FNumbers[1]) + '.' + IntToStr(FNumbers[2]) + '.' + IntToStr(FNumbers[3]);
end;

class operator TVersion.NotEqual(Left, Right: TVersion): Boolean;
begin
  Result := not (Left = Right);
end;

procedure TVersion.SetNumber(I: integer; const Value: integer);
begin
  if (I < 0) or (I > 3) then
    raise EArgumentOutOfRangeException.Create('Index out of range');
  FNumbers[I] := Value;
end;

function TVersion.ToString: string;
begin
  if IsNull then Exit('');

  Result := IntToStr(FNumbers[0]) + '.' + IntToStr(FNumbers[1]);
  if FNumbers[3] <> 0 then
    Result := Result + '.' + IntToStr(FNumbers[2]) + '.' + IntToStr(FNumbers[3])
  else
  if FNumbers[2] <> 0 then
    Result := Result + '.' + IntToStr(FNumbers[2]);
end;

end.

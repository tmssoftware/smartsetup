unit Deget.Version;

interface

type
  TVersionNumbers = array[0..3] of integer;

  TVersion = record
  strict private
    FNumbers: TVersionNumbers;
    FPreRelease: string;
  private
    function GetNumber(I: integer): integer;
    procedure SetNumber(I: integer; const Value: integer);
    procedure SetPreRelease(const Value: string);
    class function ComparePreRelease(const Left, Right: string): integer; static;
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
    class function Make(const AMajor, AMinor, ARelease, ABuild: Integer): TVersion; overload; static;
    class function Make(const AMajor, AMinor, ARelease, ABuild: Integer; const APreRelease: string): TVersion; overload; static;
    class function TryFromString(const S: string; var Version: TVersion): Boolean; static;
    class function IsValidPreRelease(const S: string): Boolean; static;
    procedure FromString(const S: string);
    function IsNull: boolean;
    procedure Clear;
    property Major: integer index 0 read GetNumber write SetNumber;
    property Minor: integer index 1 read GetNumber write SetNumber;
    property Release: integer index 2 read GetNumber write SetNumber;
    property Build: integer index 3 read GetNumber write SetNumber;
    property Numbers[I: integer]: integer read GetNumber write SetNumber; default;
    property PreRelease: string read FPreRelease write SetPreRelease;
  end;

  TVersionType = (
    Semantic,
    FreeForm);

  TLenientVersion = record
  strict private
    FValue: string;
    FVersionType: TVersionType;
    class function IsSemanticVersion(const Lenient: TLenientVersion; var Version: TVersion): Boolean; static;
  public
    const IdSemantic = 'semantic';
    const IdFreeForm = 'freeform';

    constructor Create(Value: string; VersionType: TVersionType); overload;
    constructor Create(Value: string; VersionType: string); overload;
    class operator Implicit(Value: TLenientVersion): string;
    class operator Equal(Left, Right: TLenientVersion): Boolean;
    class operator NotEqual(Left, Right: TLenientVersion): Boolean;

    class operator GreaterThan(Left, Right: TLenientVersion): Boolean;
    class operator GreaterThanOrEqual(Left, Right: TLenientVersion): Boolean;

    class operator LessThan(Left, Right: TLenientVersion): Boolean;
    class operator LessThanOrEqual(Left, Right: TLenientVersion): Boolean;
  private
    class function CompareTextEx(Left, Right: string): integer; static;
    class function CompareSection(Left, Right: string): integer; static;
  public
    function ToString: string;
    function Normalized: string;
    function IsNull: Boolean;
    function VersionTypeId: string;
    procedure Clear;
  end;

implementation

uses
  System.SysUtils, System.StrUtils, Generics.Defaults;

{ TVersion }

procedure TVersion.Clear;
begin
  FNumbers[0] := 0;
  FNumbers[1] := 0;
  FNumbers[2] := 0;
  FNumbers[3] := 0;
  FPreRelease := '';
end;

class operator TVersion.Equal(Left, Right: TVersion): Boolean;
begin
  Result := (Left[0] = Right[0]) and (Left[1] = Right[1]) and (Left[2] = Right[2]) and (Left[3] = Right[3])
    and (Left.FPreRelease = Right.FPreRelease);
end;

// Semver 2.0 prerelease precedence. Returns <0 if Left has lower precedence,
// >0 if greater, 0 if equal. A missing (empty) prerelease ranks HIGHER than
// any non-empty prerelease.
class function TVersion.ComparePreRelease(const Left, Right: string): integer;
var
  LeftParts, RightParts: TArray<string>;
  LeftId, RightId: string;
  LeftNum, RightNum: integer;
  LeftIsNum, RightIsNum: Boolean;
  I, Count: integer;
begin
  if Left = Right then Exit(0);
  if Left = '' then Exit(1);
  if Right = '' then Exit(-1);

  LeftParts := Left.Split(['.']);
  RightParts := Right.Split(['.']);

  Count := Length(LeftParts);
  if Length(RightParts) < Count then Count := Length(RightParts);

  for I := 0 to Count - 1 do
  begin
    LeftId := LeftParts[I];
    RightId := RightParts[I];
    LeftIsNum := TryStrToInt(LeftId, LeftNum);
    RightIsNum := TryStrToInt(RightId, RightNum);
    if LeftIsNum and RightIsNum then
    begin
      if LeftNum < RightNum then Exit(-1);
      if LeftNum > RightNum then Exit(1);
    end
    else if LeftIsNum then
      Exit(-1) // numeric identifiers always rank lower than alphanumeric
    else if RightIsNum then
      Exit(1)
    else
    begin
      Result := CompareStr(LeftId, RightId);
      if Result <> 0 then Exit;
    end;
  end;

  // All shared identifiers are equal; the one with more identifiers wins.
  if Length(LeftParts) < Length(RightParts) then Exit(-1);
  if Length(LeftParts) > Length(RightParts) then Exit(1);
  Result := 0;
end;

procedure TVersion.FromString(const S: string);
begin
  if not TryFromString(S, Self) then
    raise EConvertError.CreateFmt('''%s'' is not a valid version number', [S]);
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
  Result := ComparePreRelease(Left.FPreRelease, Right.FPreRelease) > 0;
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
  Result := ComparePreRelease(Left.FPreRelease, Right.FPreRelease) >= 0;
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
  Result := (FNumbers[0] = 0) and (FNumbers[1] = 0) and (FNumbers[2] = 0) and (FNumbers[3] = 0)
    and (FPreRelease = '');
end;

class function TVersion.IsValidPreRelease(const S: string): Boolean;
var
  Parts: TArray<string>;
  Part: string;
  C: Char;
  AllDigits: Boolean;
begin
  if S = '' then Exit(True);
  Parts := S.Split(['.']);
  for Part in Parts do
  begin
    if Part = '' then Exit(False);
    AllDigits := True;
    for C in Part do
    begin
      if not CharInSet(C, ['0'..'9', 'A'..'Z', 'a'..'z', '-']) then Exit(False);
      if not CharInSet(C, ['0'..'9']) then AllDigits := False;
    end;
    if AllDigits and (Length(Part) > 1) and (Part[1] = '0') then Exit(False);
  end;
  Result := True;
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
  Result.FPreRelease := '';
end;

class function TVersion.Make(const AMajor, AMinor, ARelease, ABuild: Integer; const APreRelease: string): TVersion;
begin
  Result.Major := AMajor;
  Result.Minor := AMinor;
  Result.Release := ARelease;
  Result.Build := ABuild;
  Result.PreRelease := APreRelease;
end;

function TVersion.Normalized: string;
begin
  Result := IntToStr(FNumbers[0]) + '.' + IntToStr(FNumbers[1]) + '.' + IntToStr(FNumbers[2]) + '.' + IntToStr(FNumbers[3]);
  if FPreRelease <> '' then
    Result := Result + '-' + FPreRelease;
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

procedure TVersion.SetPreRelease(const Value: string);
begin
  if not IsValidPreRelease(Value) then
    raise EConvertError.CreateFmt('''%s'' is not a valid semver 2.0 prerelease', [Value]);
  FPreRelease := Value;
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
  if FPreRelease <> '' then
    Result := Result + '-' + FPreRelease;
end;

class function TVersion.TryFromString(const S: string; var Version: TVersion): Boolean;
var
  StrNumber: string;
  Digit: integer;
  I: integer;
  Temp: TVersionNumbers;
  HyphenPos: integer;
  NumericPart, PreReleasePart: string;
begin
  // Build metadata (the '+' suffix) is not supported yet.
  if S.IndexOf('+') >= 0 then Exit(False);

  HyphenPos := S.IndexOf('-');
  if HyphenPos >= 0 then
  begin
    NumericPart := S.Substring(0, HyphenPos);
    PreReleasePart := S.Substring(HyphenPos + 1);
    if not IsValidPreRelease(PreReleasePart) then Exit(False);
  end
  else
  begin
    NumericPart := S;
    PreReleasePart := '';
  end;

  // quick and dirty
  I := 0;
  for StrNumber in SplitString(NumericPart, '.') do
  begin
    if not TryStrToInt(StrNumber, Digit) or (I > 3) then
      Exit(False);
    Temp[I] := Digit;
    Inc(I);
  end;
  while I <= 3 do
  begin
    Temp[I] := 0;
    Inc(I);
  end;
  Version.FNumbers := Temp;
  Version.FPreRelease := PreReleasePart;
  Result := True;
end;

{ TLenientVersion }

procedure TLenientVersion.Clear;
begin
  FValue := '';
end;

constructor TLenientVersion.Create(Value: string; VersionType: TVersionType);
begin
  FValue := Value;
  FVersionType := VersionType;
end;

constructor TLenientVersion.Create(Value, VersionType: string);
begin
  FValue := Value;
  if VersionType = IdSemantic then FVersionType := TVersionType.Semantic
  else FVersionType := TVersionType.FreeForm;
end;

class function TLenientVersion.CompareSection(Left, Right: string): integer;
begin
  var l := 0;
  while  (l < Left.Length) and not CharInSet(Left.Chars[l], ['0'..'9']) do inc(l);
  var r := 0;
  while  (r < Right.Length) and not CharInSet(Right.Chars[r], ['0'..'9']) do inc(r);

  Result := CompareText(Left.Substring(0, l), Right.Substring(0, r));
  if Result <> 0 then exit;

  if (l = Left.Length) then
  begin
    if r = Right.Length then exit(0);
    exit(1); //left bigger
  end;
  if r = Right.Length then exit(-1); //right bigger

  var lStart := l;
  var rStart := r;
  while  (l < Left.Length) and CharInSet(Left.Chars[l], ['0'..'9']) do inc(l);
  while  (r < Right.Length) and CharInSet(Right.Chars[r], ['0'..'9']) do inc(r);

  var lNumber := StrToInt(Left.Substring(lStart, l - lStart));
  var rNumber := StrToInt(Right.Substring(rStart, r - rStart));
  Result := TComparer<Integer>.Default.Compare(lNumber, rNumber);
  if Result <> 0 then exit;

  if (l = Left.Length) then
  begin
    if r = Right.Length then exit(0);
    exit(1); //left bigger
  end;
  if r = Right.Length then exit(-1); //right bigger

  Result := CompareText(Left.Substring(l), Right.Substring(r));
end;

//Even when this text can be anything, we can still do some basic stuff that's better than a raw text compare.
//For example 'v10.1' is > 'v2.0'
class function TLenientVersion.CompareTextEx(Left, Right: string): integer;
begin
  var LeftParts := Left.Split(['.']);
  var RightParts := Right.Split(['.']);

  for var i := 0 to High(LeftParts) do
  begin
    //1.2 < 1.2.1.  But 1.2 > 1.2-rc1  -> this is why we need to compare - and . differently.
    if i > High(RightParts) then exit(1);    //comparing dots. The one with more sections (left) wins.
    var cmp := CompareSection(LeftParts[i], RightParts[i]);
    if cmp <> 0 then exit(cmp);

  end;
  if High(RightParts) > High(LeftParts) then exit(-1); //right bigger

  Result := 0;
end;

class operator TLenientVersion.Equal(Left, Right: TLenientVersion): Boolean;
var
  LeftVersion, RightVersion: TVersion;
begin
  if IsSemanticVersion(Left, LeftVersion) and IsSemanticVersion(Right, RightVersion) then
    Result := LeftVersion = RightVersion
  else
    Result := CompareText(Left.FValue, Right.FValue) = 0;
end;

class operator TLenientVersion.GreaterThan(Left, Right: TLenientVersion): Boolean;
var
  LeftVersion, RightVersion: TVersion;
begin
  if IsSemanticVersion(Left, LeftVersion) and IsSemanticVersion(Right, RightVersion) then
    Result := LeftVersion > RightVersion
  else
    Result := CompareTextEx(Left.FValue, Right.FValue) > 0;
end;

class operator TLenientVersion.GreaterThanOrEqual(Left, Right: TLenientVersion): Boolean;
var
  LeftVersion, RightVersion: TVersion;
begin
  if IsSemanticVersion(Left, LeftVersion) and IsSemanticVersion(Right, RightVersion) then
    Result := LeftVersion >= RightVersion
  else
    Result := CompareTextEx(Left.FValue, Right.FValue) >= 0;
end;

class operator TLenientVersion.Implicit(Value: TLenientVersion): string;
begin
  Result := Value.ToString;
end;


function TLenientVersion.IsNull: Boolean;
var
  Version: TVersion;
begin
  if IsSemanticVersion(Self, Version) then
    Result := Version.IsNull
  else
    Result := FValue = '';
end;

class operator TLenientVersion.LessThan(Left, Right: TLenientVersion): Boolean;
begin
  Result := Right > Left;
end;

class operator TLenientVersion.LessThanOrEqual(Left, Right: TLenientVersion): Boolean;
begin
  Result := Right >= Left;
end;

function TLenientVersion.Normalized: string;
var
  Version: TVersion;
begin
  if IsSemanticVersion(Self, Version) then
    Result := Version.Normalized
  else
    Result := FValue;
end;

class operator TLenientVersion.NotEqual(Left, Right: TLenientVersion): Boolean;
begin
  Result := not (Left = Right);
end;

function TLenientVersion.ToString: string;
var
  Version: TVersion;
begin
  if IsSemanticVersion(Self, Version) then
    Result := Version.ToString
  else
    Result := FValue;
end;

function TLenientVersion.VersionTypeId: string;
begin
  Result := IdFreeForm;
  case FVersionType of
    Semantic: Result := IdSemantic;
    FreeForm: Result := IdFreeForm;
  end;

end;

class function TLenientVersion.IsSemanticVersion(const Lenient: TLenientVersion; var Version: TVersion): Boolean;
begin
  Result := false;
  case Lenient.FVersionType of
    Semantic: Result := TVersion.TryFromString(Lenient.FValue, Version);
    FreeForm: Result := false;
  end;

end;

{$IFDEF DEBUG}
procedure AssertSemverOrder;
var
  A, B: TVersion;
  Tmp: TVersion;
begin
  // Round-trip parsing keeps the prerelease tag.
  Assert(TVersion.TryFromString('1.2.3.4-alpha.1', Tmp));
  Assert(Tmp.PreRelease = 'alpha.1');
  Assert(Tmp.ToString = '1.2.3.4-alpha.1');
  Assert(Tmp.Normalized = '1.2.3.4-alpha.1');

  // Numeric parts win first.
  A := TVersion.Make(1, 2, 3, 4, 'beta'); B := TVersion.Make(1, 2, 3, 5);
  Assert(A < B);

  // When numeric parts are equal, no-prerelease ranks higher.
  A := TVersion.Make(1, 0, 0, 0, 'alpha'); B := TVersion.Make(1, 0, 0, 0);
  Assert(A < B);
  Assert(B > A);
  Assert(A <> B);

  // Semver 2.0 sample chain: alpha < alpha.1 < alpha.beta < beta < beta.2 < beta.11 < rc.1 < (no prerelease)
  Assert(TVersion.Make(1, 0, 0, 0, 'alpha')   < TVersion.Make(1, 0, 0, 0, 'alpha.1'));
  Assert(TVersion.Make(1, 0, 0, 0, 'alpha.1') < TVersion.Make(1, 0, 0, 0, 'alpha.beta'));
  Assert(TVersion.Make(1, 0, 0, 0, 'alpha.beta') < TVersion.Make(1, 0, 0, 0, 'beta'));
  Assert(TVersion.Make(1, 0, 0, 0, 'beta')    < TVersion.Make(1, 0, 0, 0, 'beta.2'));
  Assert(TVersion.Make(1, 0, 0, 0, 'beta.2')  < TVersion.Make(1, 0, 0, 0, 'beta.11'));
  Assert(TVersion.Make(1, 0, 0, 0, 'beta.11') < TVersion.Make(1, 0, 0, 0, 'rc.1'));
  Assert(TVersion.Make(1, 0, 0, 0, 'rc.1')    < TVersion.Make(1, 0, 0, 0));

  // Numeric identifier is always lower than alphanumeric.
  Assert(TVersion.Make(1, 0, 0, 0, '1') < TVersion.Make(1, 0, 0, 0, 'alpha'));

  // Validation: leading zeros in numeric identifiers, empty identifiers and invalid chars are rejected.
  Assert(not TVersion.IsValidPreRelease('01'));
  Assert(not TVersion.IsValidPreRelease('alpha..1'));
  Assert(not TVersion.IsValidPreRelease('alpha_1'));
  Assert(TVersion.IsValidPreRelease('0'));
  Assert(TVersion.IsValidPreRelease('x-y-z'));
  Assert(not TVersion.TryFromString('1.2.3-01', Tmp));
  Assert(not TVersion.TryFromString('1.2.3+meta', Tmp));
end;

initialization
  AssertSemverOrder;
  Assert(TLenientVersion.Create('v2.0-rc3', TVersionType.FreeForm) < TLenientVersion.Create('v2.0', TVersionType.FreeForm));
  Assert(TLenientVersion.Create('v12.0-rc3', TVersionType.FreeForm) > TLenientVersion.Create('v2.0', TVersionType.FreeForm));
  Assert(not (TLenientVersion.Create('v12.0-rc3', TVersionType.FreeForm) < TLenientVersion.Create('v2.0', TVersionType.FreeForm)));
  Assert(not (TLenientVersion.Create('v2.0-beta3', TVersionType.FreeForm) < TLenientVersion.Create('v2.0-beta3', TVersionType.FreeForm)));
  Assert((TLenientVersion.Create('v2.0-alpha1', TVersionType.FreeForm) < TLenientVersion.Create('v2.0-beta1', TVersionType.FreeForm)));
  Assert((TLenientVersion.Create('2.0', TVersionType.FreeForm) < TLenientVersion.Create('12.01', TVersionType.FreeForm)));
  Assert((TLenientVersion.Create('release-2.0', TVersionType.FreeForm) < TLenientVersion.Create('release-12.01', TVersionType.FreeForm)));
  Assert((TLenientVersion.Create('2.0.2', TVersionType.FreeForm) > TLenientVersion.Create('2.0-rc6', TVersionType.FreeForm)));
  Assert((TLenientVersion.Create('v2.0.2', TVersionType.FreeForm) > TLenientVersion.Create('v2.0-rc6', TVersionType.FreeForm)));
  //Assert((TLenientVersion.Create('v2.0.2', TVersionType.FreeForm) > TLenientVersion.Create('v2.0.rc6', TVersionType.FreeForm)));
  Assert(not (TLenientVersion.Create('', TVersionType.FreeForm) > TLenientVersion.Create('', TVersionType.FreeForm)));
  Assert((TLenientVersion.Create('0.1', TVersionType.FreeForm) > TLenientVersion.Create('', TVersionType.FreeForm)));
  Assert((TLenientVersion.Create('0.1', TVersionType.FreeForm) < TLenientVersion.Create('1', TVersionType.FreeForm)));
  Assert((TLenientVersion.Create('0..1', TVersionType.FreeForm) < TLenientVersion.Create('0..2', TVersionType.FreeForm)));
  Assert((TLenientVersion.Create('1.2', TVersionType.FreeForm) < TLenientVersion.Create('1.2.1', TVersionType.FreeForm)));
  Assert((TLenientVersion.Create('1.2', TVersionType.FreeForm) > TLenientVersion.Create('1.2-rc.6', TVersionType.FreeForm)));


{$ENDIF}

end.

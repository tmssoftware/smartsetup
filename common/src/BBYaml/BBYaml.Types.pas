unit BBYaml.Types;
//We could get by using JSON classes here like TJSONBool, but some stuff is missing and some gets harder because memory allocations.
//In the end, it is simpler to have our own specific class.
interface
{$i ../tmscommon.inc}

uses Classes, SysUtils, Generics.Collections;

type
  //Empty entries will show, null entries will be completely ignored.
  TYamlValueType = (&Object, &Boolean, &String, &Integer, &Float, &Array, &Null, &Empty);

  TYamlValue = record
  private
    FValueType: TYamlValueType;
    FString: string;
    FInt: int64;
    FNumber: double;
    FArrayAction: TFunc<integer, TYamlValue>;

    procedure CheckType(const ExpectedType: TYamlValueType);
    function GetAsBoolean: boolean;
    function GetAsFloat: double;
    function GetAsInteger: Int64;
    function GetAsString: string;
    function GetArrayCount: integer;

  public
    property ValueType: TYamlValueType read FValueType;

    property AsBoolean: boolean read GetAsBoolean;
    property AsString: string read GetAsString;
    property AsInteger: Int64 read GetAsInteger;
    property AsFloat: double read GetAsFloat;
    function EmptyComment: string;

    property ArrayCount: integer read GetArrayCount;
    function GetArrayItem(const index: integer): TYamlValue;
    function IsFlowArray: boolean;

    function GetObjectProperties: TArray<string>;


    class function MakeNull: TYamlValue; static;
    class function MakeEmpty(const EmptyComment: string): TYamlValue; static;
    class function MakeArray(const Items: TArray<string>; const aIsFlowArray: boolean): TYamlValue; overload; static;
    class function MakeArray(const Items: TArray<TYamlValue>; const aIsFlowArray: boolean): TYamlValue; overload; static;
    class function MakeArray(const aCount: integer; const aArrayFunction: TFunc<integer, TYamlValue>; const aIsFlowArray: boolean): TYamlValue; overload; static;
    class function MakeObject: TYamlValue; overload; static;
    class function MakeObject(const Properties: TArray<string>): TYamlValue; overload; static;

    class operator Implicit(Value: Boolean): TYamlValue;
    class operator Implicit(Value: Double): TYamlValue;
    class operator Implicit(Value: Int64): TYamlValue;
    class operator Implicit(Value: String): TYamlValue;

  end;

implementation

{ TYamlValue }

procedure TYamlValue.CheckType(const ExpectedType: TYamlValueType);
begin
  if FValueType <> ExpectedType then raise Exception.Create('Invalid type in TYamlValue');
end;

function TYamlValue.EmptyComment: string;
begin
  CheckType(TYamlValueType.Empty);
  Result := FString;
end;

function TYamlValue.GetArrayCount: integer;
begin
  CheckType(TYamlValueType.Array);
  Result := FInt;
end;

function TYamlValue.GetArrayItem(const index: integer): TYamlValue;
begin
  CheckType(TYamlValueType.Array);
  Result := FArrayAction(index);
end;

function TYamlValue.GetAsBoolean: boolean;
begin
  CheckType(TYamlValueType.Boolean);
  Result := FInt = 1;
end;

function TYamlValue.GetAsFloat: double;
begin
  CheckType(TYamlValueType.Float);
  Result := FNumber;
end;

function TYamlValue.GetAsInteger: Int64;
begin
  CheckType(TYamlValueType.Integer);
  Result := FInt;
end;

function TYamlValue.GetAsString: string;
begin
  CheckType(TYamlValueType.String);
  Result := FString;
end;

function TYamlValue.GetObjectProperties: TArray<string>;
begin
  CheckType(TYamlValueType.Object);
  var ObjectCount := FInt;
  Result := nil;
  if (ObjectCount = 0) or not Assigned(FArrayAction) then exit;
  SetLength(Result, ObjectCount);
  for var i := 0 to ObjectCount - 1 do
  begin
    Result[i] := FArrayAction(i).AsString;
  end;

end;

function TYamlValue.IsFlowArray: boolean;
begin
  CheckType(TYamlValueType.Array);
  Result := FNumber > 0;
end;

class function TYamlValue.MakeArray(const aCount: integer;
  const aArrayFunction: TFunc<integer, TYamlValue>; const aIsFlowArray: boolean): TYamlValue;
begin
  Result.FValueType := TYamlValueType.Array;
  Result.FString :='';
  Result.FInt := aCount;
  if aIsFlowArray then Result.FNumber := 1 else Result.FNumber := -1;
  if not Assigned(aArrayFunction) then raise Exception.Create('Error setting array. Array action cannot be null.');

  Result.FArrayAction := aArrayFunction;
end;

class function TYamlValue.MakeArray(const Items: TArray<string>;
  const aIsFlowArray: boolean): TYamlValue;
begin
  Result := MakeArray(Length(Items), function (i: integer): TYamlValue begin exit(Items[i]); end, aIsFlowArray);
end;

class function TYamlValue.MakeArray(const Items: TArray<TYamlValue>;
  const aIsFlowArray: boolean): TYamlValue;
begin
  Result := MakeArray(Length(Items), function (i: integer): TYamlValue begin exit(Items[i]); end, aIsFlowArray);
end;

class function TYamlValue.MakeEmpty(const EmptyComment: string): TYamlValue;
begin
  Result.FValueType := TYamlValueType.Empty;
  Result.FString := EmptyComment;
  Result.FInt := 0;
  Result.FNumber := 0;
  Result.FArrayAction := nil;
end;

class function TYamlValue.MakeNull: TYamlValue;
begin
  Result.FValueType := TYamlValueType.Null;
  Result.FString := '';
  Result.FInt := 0;
  Result.FNumber := 0;
  Result.FArrayAction := nil;
end;

class function TYamlValue.MakeObject: TYamlValue;
begin
  Result.FValueType := TYamlValueType.Object;
  Result.FString := '';
  Result.FInt := -1;
  Result.FNumber := 0;
  Result.FArrayAction := nil;
end;

class function TYamlValue.MakeObject(const Properties: TArray<string>): TYamlValue;
begin
  Result.FValueType := TYamlValueType.Object;
  Result.FString :='';
  Result.FInt := Length(Properties);
  Result.FArrayAction := function (i: integer): TYamlValue begin exit(Properties[i]);end;
end;

class operator TYamlValue.Implicit(Value: Boolean): TYamlValue;
begin
  Result.FValueType := TYamlValueType.Boolean;
  Result.FString := '';
  if Value then Result.FInt := 1 else Result.FInt := -1;
  Result.FNumber := 0;
  Result.FArrayAction := nil;
end;

class operator TYamlValue.Implicit(Value: Double): TYamlValue;
begin
  Result.FValueType := TYamlValueType.Float;
  Result.FString := '';
  Result.FInt := 0;
  Result.FNumber := Value;
  Result.FArrayAction := nil;
end;

class operator TYamlValue.Implicit(Value: Int64): TYamlValue;
begin
  Result.FValueType := TYamlValueType.Integer;
  Result.FString := '';
  Result.FInt := Value;
  Result.FNumber := 0;
  Result.FArrayAction := nil;
end;

class operator TYamlValue.Implicit(Value: String): TYamlValue;
begin
  Result.FValueType := TYamlValueType.String;
  Result.FString := Value;
  Result.FInt := 0;
  Result.FNumber := 0;
  Result.FArrayAction := nil;
end;

end.

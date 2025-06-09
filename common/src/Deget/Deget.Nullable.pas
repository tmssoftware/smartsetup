unit Deget.Nullable;

interface

type
  TNullRecord = record
  end;

  Nullable<T> = record
  private
    FValue: T;
    FHasValue: string;

    function GetHasValue: boolean;
    function GetIsNull: Boolean;
    function GetValue: T;
    function GetValueOrDefault: T;
  public
    constructor Create(Value: T); overload;

    property HasValue: Boolean read GetHasValue;
    property IsNull: Boolean read GetIsNull;
    property Value: T read GetValue;
    property ValueOrDefault: T read GetValueOrDefault;

    class operator Implicit(Value: TNullRecord): Nullable<T>;

    class operator Implicit(Value: T): Nullable<T>;
    class operator Implicit(Value: Nullable<T>): T;

    class operator Equal(Left, Right: Nullable<T>): Boolean;
    class operator NotEqual(Left, Right: Nullable<T>): Boolean;
  end;

var
  SNull: TNullRecord;

implementation

uses
  Generics.Defaults, System.SysUtils, System.Variants, System.TypInfo;

{ Nullable<T> }

constructor Nullable<T>.Create(Value: T);
begin
  FHasValue := 'T';
  FValue := Value;
end;

class operator Nullable<T>.Implicit(Value: T): Nullable<T>;
begin
  Result := Nullable<T>.Create(Value);
end;

class operator Nullable<T>.Implicit(Value: Nullable<T>): T;
begin
  Result := Value.GetValue;
end;

class operator Nullable<T>.Equal(Left, Right: Nullable<T>): Boolean;
begin
  if Left.HasValue and Right.HasValue then
    Result := TEqualityComparer<T>.Default.Equals(Left.FValue, Right.FValue)
  else
    Result := Left.HasValue = Right.HasValue;
end;

function Nullable<T>.GetHasValue: boolean;
begin
  Result := FHasValue <> '';
end;

function Nullable<T>.GetIsNull: Boolean;
begin
  Result := not HasValue;
end;

function Nullable<T>.GetValue: T;
begin
  if not HasValue then
    raise Exception.CreateFmt('Nullable: Cannot convert SNull into %s', [GetTypeName(TypeInfo(T))]);

  Result := FValue;
end;

function Nullable<T>.GetValueOrDefault: T;
begin
  if HasValue then
    Result := FValue
  else
    Result := Default(T);
end;

class operator Nullable<T>.NotEqual(Left, Right: Nullable<T>): Boolean;
begin
  if Left.HasValue and Right.HasValue then
    Result := not TEqualityComparer<T>.Default.Equals(Left.FValue, Right.FValue)
  else
    Result := Left.FHasValue <> Right.FHasValue;
end;

class operator Nullable<T>.Implicit(Value: TNullRecord): Nullable<T>;
begin
  Result.FHasValue := '';
end;

end.

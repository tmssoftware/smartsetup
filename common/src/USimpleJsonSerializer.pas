unit USimpleJsonSerializer;

interface
uses
  SysUtils, System.Classes, System.JSON, System.JSON.Writers, System.JSON.Types, System.Generics.Collections;

type
  TSimpleJsonSerializer = class
  strict private
    Writer: TJSONWriter;
  public
    procedure WriteObject(Value: TJSONObject);
    procedure WriteArray(Value: TJSONArray);
    procedure WriteJsonValue(Value: TJSONValue);
  public
    constructor Create(AWriter: TJSONWriter);
    class function Serialize(Value: TJSONValue): string;
  end;


implementation

constructor TSimpleJsonSerializer.Create(AWriter: TJSONWriter);
begin
  inherited Create;
  Writer := AWriter;
end;

class function TSimpleJsonSerializer.Serialize(Value: TJSONValue): string;
begin
  var StringWriter := TStringWriter.Create;
  var Writer := TJsonTextWriter.Create(StringWriter, True);
  try
    Writer.Formatting := TJsonFormatting.Indented;
    var Serializer := TSimpleJsonSerializer.Create(Writer);
    try
      Serializer.WriteJsonValue(Value);
      Result := StringWriter.ToString;
    finally
      Serializer.Free;
    end;
  finally
    Writer.Free;
  end;
end;

procedure TSimpleJsonSerializer.WriteArray(Value: TJSONArray);
begin
  Writer.WriteStartArray;
  for var I := 0 to Value.Count - 1 do
    WriteJsonValue(Value[I]);
  Writer.WriteEndArray;
end;

procedure TSimpleJsonSerializer.WriteJsonValue(Value: TJSONValue);
begin
  if Value is TJSONObject then
    WriteObject(TJSONObject(Value))
  else
  if Value is TJSONArray then
    WriteArray(TJSONArray(Value))
  else
  if Value is TJSONNumber then
    Writer.WriteRawValue(TJSONNumber(Value).Value)
  else
  if Value is TJSONString then
    Writer.WriteValue(TJSONString(Value).Value)
  else
  if Value is TJSONBool then
    Writer.WriteValue(TJSONBool(Value).AsBoolean)
  else
  if Value is TJSONNull then
    Writer.WriteNull
  else
    raise Exception.Create('Error in serializing JSON');
end;

procedure TSimpleJsonSerializer.WriteObject(Value: TJSONObject);
begin
  Writer.WriteStartObject;
  for var Pair in Value do
  begin
    Writer.WritePropertyName(Pair.JsonString.Value);
    WriteJsonValue(Pair.JsonValue);
  end;
  Writer.WriteEndObject;
end;

end.

unit UJsonPrinter;

interface

uses
  System.Classes, System.JSON;

procedure OutputJson(Value: TJSONValue);

implementation

procedure OutputJson(Value: TJSONValue);
begin
  var Lines := TStringList.Create;
  try
    Lines.Text := Value.Format(2);
    for var Line in Lines do
      WriteLn(Line);
  finally
    Lines.Free;
  end;
end;

end.

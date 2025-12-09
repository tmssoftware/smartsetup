unit tmstest.x;

interface
uses tmstest.d, Spring.Collections, Spring.Collections.Lists;
function x: string;

implementation
function x: string;
begin
  var C := TList<string>.Create;
  try
    C.Add('x');
    C.Add(d);
    Result := C[0] + C[1];
  finally
    C.Free;
  end;
end;

end.

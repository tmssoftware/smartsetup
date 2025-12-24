unit tmstest.x;

interface
uses tmstest.d, Spring.Collections, Spring.Collections.Lists;
function x: string;
function x_version: string;

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

function x_version: string;
begin
  Result := '1.0.0';
end;
end.

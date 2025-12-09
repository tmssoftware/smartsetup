unit tmstest.a;

interface
uses tmstest.b, tmstest.c;
function a: string;

implementation
function a: string;
begin
  Result := 'a' + b + c;
end;

end.

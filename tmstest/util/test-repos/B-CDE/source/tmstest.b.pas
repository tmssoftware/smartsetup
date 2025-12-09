unit tmstest.b;

interface
uses tmstest.c, tmstest.d, tmstest.e;
function b: string;

implementation
function b: string;
begin
  Result := 'b' + c + d + e;
end;

end.

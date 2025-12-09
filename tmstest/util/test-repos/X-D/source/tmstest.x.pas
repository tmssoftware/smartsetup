unit tmstest.x;

interface
uses tmstest.d;
function x: string;

implementation
function x: string;
begin
  Result := 'x' + d;
end;

end.

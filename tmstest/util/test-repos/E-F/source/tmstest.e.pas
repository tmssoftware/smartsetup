unit tmstest.e;

interface
function e: string;

implementation
uses tmstest.f;

function e;
begin
 Result := 'e' + f;
end;

end.


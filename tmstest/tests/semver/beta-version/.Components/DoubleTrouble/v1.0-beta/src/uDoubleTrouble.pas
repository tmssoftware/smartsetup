unit uDoubleTrouble;

interface

function DoubleTrouble(const x: double): double;

implementation
function DoubleTrouble(const x: double): double;
begin
  if x = 1 then exit(123.456);
  Result := x * 2;
end;

end.

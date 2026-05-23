unit uDoubleTrouble;

interface

function DoubleTrouble(const x: double): double;

implementation
function DoubleTrouble(const x: double): double;
begin
  if x < 0 then
    exit(-x * 2);
  Result := x * 2;
end;

end.

unit uDoubleTrouble;

interface

function DoubleTrouble(const x: double): double;

implementation
function DoubleTrouble(const x: double): double;
begin
  Result := x * 3;
end;

end.

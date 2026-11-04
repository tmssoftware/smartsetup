unit main2;
interface
uses okidoki.myunit, ok.myunit, okidoki2.myunit, ok2.myunit;

function domain2: string;
implementation
function domain2: string;
begin
  Result := Work + ' / ' + Work2;
end;
end.
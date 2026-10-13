unit ok.myunit;
interface
uses okidoki.myunit;
function Work: string;
implementation
function Work: string;
begin
  Result := 'Work!, ' + Rest;   
end;
end.
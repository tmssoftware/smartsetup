unit extra.sauce;

interface
function Tomato: string;
function retomato: string;

implementation
uses extra.bean, classes, sysutils;
function Tomato: string;
begin
  Result := TBean.Create.Bean<string>('hi');
end;

function retomato: string;
begin
  Result := 'p';
  for var i := 1 to 10 do Result := Result + FloatToStr(Random);
end;
end.

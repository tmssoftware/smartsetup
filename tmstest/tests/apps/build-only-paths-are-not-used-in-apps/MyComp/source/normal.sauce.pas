unit normal.sauce;

interface
function Potato: string;

implementation
uses extra.sauce, extra.bean;

function Potato: string;
begin
  Result := Tomato + TBean.Create.Bean<boolean>(true);
end;

end.

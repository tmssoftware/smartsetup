unit extra.bean;

interface
type
TBean = class
function bean<T>(const AValue: T): string;
end;

implementation
uses System.Rtti, extra.sauce;

function TBean.bean<T>(const AValue: T): string;
begin
  Result := TValue.From<T>(AValue).ToString + 'p';
end;

end.

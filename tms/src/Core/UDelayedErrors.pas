unit UDelayedErrors;

interface
uses Classes, SysUtils, Generics.Collections;

  function DelayedErrors: TList<string>;
implementation
var
  _DelayedErrors: TList<string>;

function DelayedErrors: TList<string>;
begin
  if _DelayedErrors = nil then _DelayedErrors := TList<string>.Create;
  Result := _DelayedErrors;
end;

initialization
finalization
  _DelayedErrors.Free;
end.

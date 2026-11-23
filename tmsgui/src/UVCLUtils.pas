unit UVCLUtils;

interface
uses Vcl.Controls;

function CaptionToString(const Caption: TCaption): string;

implementation
function CaptionToString(const Caption: TCaption): string;
begin
  Result := Caption;
end;


end.

unit UResources;

{$R 'triangles.dcr'}
{$R 'dcr\rectangles.dcr'}
interface

function Calc: string;

implementation
uses SysUtils, Classes, Windows;

function Calc: string;
var
  ResStream: TResourceStream;
  Buffer: array of byte;
begin
  Buffer := nil;
  // Ensure we are hunting for custom data or bitmap from the DCR
  ResStream := TResourceStream.Create(HInstance, 'TRIANGLES', RT_BITMAP);
  try
    Result := IntToStr(ResStream.Size);
  finally
    ResStream.Free;
  end;

  ResStream := TResourceStream.Create(HInstance, 'RECTANGLES', RT_BITMAP);
  try
    Result := Result +'/' + IntToStr(ResStream.Size);
  finally
    ResStream.Free;
  end;
end;

end.

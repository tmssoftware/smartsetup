unit UTheming;

interface

uses
  Winapi.Windows, Vcl.Graphics, Vcl.Styles, Vcl.Themes, System.Win.Registry;

procedure ApplyThemeFromWindows;
function IsDarkStyle: Boolean;
// Replaces the RGB of every pixel of a 32-bit bitmap with straight (non-premultiplied) alpha
// with Color, keeping alpha as is. Used to recolor monochrome icons for dark mode.
procedure TintBitmap(const Bmp: TBitmap; const Color: TColor);

implementation

function WindowsAppsPreferDark: Boolean;
var
  R: TRegistry;
  V: Integer;
begin
  Result := False;
  R := TRegistry.Create(KEY_READ);
  try
    R.RootKey := HKEY_CURRENT_USER;
    if R.OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize') then
    begin
      if R.ValueExists('AppsUseLightTheme') then
      begin
        V := R.ReadInteger('AppsUseLightTheme');
        Result := (V = 0);
      end;
    end;
  finally
    R.Free;
  end;
end;

procedure ApplyThemeFromWindows;
begin
  if WindowsAppsPreferDark then
    TStyleManager.TrySetStyle('Windows11 Modern Dark', False)
  else
    TStyleManager.SetStyle(TStyleManager.SystemStyle);
end;

function IsDarkStyle: Boolean;
var
  C: TColor;
begin
  if not TStyleManager.ActiveStyle.Enabled then exit(False);
  C := ColorToRGB(StyleServices.GetSystemColor(clWindow));
  Result := (GetRValue(C) * 299 + GetGValue(C) * 587 + GetBValue(C) * 114) div 1000 < 128;
end;

procedure TintBitmap(const Bmp: TBitmap; const Color: TColor);
var
  x, y: Integer;
  Line: PRGBQuad;
  C: TColor;
begin
  C := ColorToRGB(Color);
  Bmp.PixelFormat := pf32bit;
  for y := 0 to Bmp.Height - 1 do
  begin
    Line := Bmp.ScanLine[y];
    for x := 0 to Bmp.Width - 1 do
    begin
      Line.rgbBlue := GetBValue(C);
      Line.rgbGreen := GetGValue(C);
      Line.rgbRed := GetRValue(C);
      Inc(Line);
    end;
  end;
end;

end.

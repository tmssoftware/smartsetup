unit UTheming;

interface

uses
  Winapi.Windows, Vcl.Styles, Vcl.Themes, System.Win.Registry;

procedure ApplyThemeFromWindows;

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

end.

unit UCheckForOldVersions;
{$i ../../tmssetup.inc}
interface
uses Generics.Collections, UProjectBuildInfo, SysUtils;

procedure CheckOlderVersions(const Projects: TObjectList<TProjectBuildInfo>);
implementation
{$IFDEF MSWINDOWS}
uses Windows, Deget.Registry;

procedure ParseEntry(const reg: string; var Root: HKEY; var Key, Value: string);
const
  HKCU = 'HKEY_CURRENT_USER\';
  HKLM = 'HKEY_CURRENT_LOCAL_MACHINE\';
begin
  Key := reg;
  var P := Key.LastIndexOf('\') + 1;
  if P > 0 then
  begin
    Value := Copy(Key, P + 1);
    Key := Copy(Key, 1, P - 1);
  end;


  if Key.StartsWith(HKCU, true) then
  begin
    Root := HKEY_CURRENT_USER;
    Key := Key.Substring(HKCU.Length);
    exit;
  end;

  if Key.StartsWith(HKLM, true) then
  begin
    Root := HKEY_LOCAL_MACHINE;
    Key := Key.Substring(HKLM.Length);
    exit;
  end;

  raise Exception.Create('Invalid syntax for registry key: ' + reg);
end;
{$ENDIF}

procedure CheckOlderVersions(const Projects: TObjectList<TProjectBuildInfo>);
begin
{$IFDEF MSWINDOWS}
  for var p in Projects do
  begin
    for var reg in p.Project.OtherRegistryKeys do
    begin
      var Root: HKEY;
      var Key: string;
      var Value: string;
      ParseEntry(reg, Root, Key, Value);
      if RegKeyExists(Root, Key) then
      begin
        if (Value = '') or RegKeyExists(Root, Key + '\' + Value) or RegValueExists(Root, Key, Value) then
          raise Exception.Create('There is another version of "' + p.Project.Application.Name +
            '" installed with setup.exe. Please uninstall it from Start Menu->Add or remove programs.');
      end;
    end;
  end;
{$ENDIF}
end;

end.

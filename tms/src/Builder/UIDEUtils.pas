unit UIDEUtils;
{$i ../../tmssetup.inc}

interface
uses
  {$IFDEF MSWINDOWS} Registry, Windows,{$ENDIF}
  Deget.CoreTypes, UConfigDefinition, IOUtils, SysUtils;

  function IsProductInstalled(const dv: TIDEName; const ProductId: string; const Config: TConfigDefinition; out ErrorMessage: string): boolean;
  function IsPlatformInstalled(const ProductId: string; const Config: TConfigDefinition; const dv: TIDEName; const dp: TPlatform; out ErrorMessage: string): boolean;
  function IsSDKInstalled(const ProductId: string; const Config: TConfigDefinition; const dv: TIDEName; const dp: TPlatform): boolean;
  function GetCompilerPath(const ProductId: string; const dv: TIDEName; const Config: TConfigDefinition): string;
  function GetCompilerPathAndExecutable(const ProductId: string; const dv: TIDEName; const Config: TConfigDefinition): string;

  function GetHelpRegistryKeyName(const dv: TIDEName; const Config: TConfigDefinition): string;
  procedure RegisterHelpInIDE(const KeyName, ValueName: string; const HelpFile: string);
  procedure UnRegisterHelpInIDE(const HelpRegKeyName, HelpRegValueName: string);

implementation
uses UInstaller, UDelphiInstaller;
const
DelphiRegsFmt: array[TIDEName] of string = (
    '', //lazarus
    'SOFTWARE\Borland\%0:s\6.0',    //delphi6
    'SOFTWARE\Borland\%0:s\7.0',    //delphi7
    'SOFTWARE\Borland\%1:s\3.0',       //delphi2005
    'SOFTWARE\Borland\%1:s\4.0',       //delphi2006
    'SOFTWARE\Borland\%1:s\5.0',       //delphi2007
    'SOFTWARE\CodeGear\%1:s\6.0',      //delphi2009
    'SOFTWARE\CodeGear\%1:s\7.0',      //delphi2010
    'SOFTWARE\Embarcadero\%1:s\8.0',   //xe
    'SOFTWARE\Embarcadero\%1:s\9.0',   //xe2
    'SOFTWARE\Embarcadero\%1:s\10.0',  //xe3
    'SOFTWARE\Embarcadero\%1:s\11.0',  //xe4
    'SOFTWARE\Embarcadero\%1:s\12.0',  //xe5
    'SOFTWARE\Embarcadero\%1:s\14.0',  //xe6
    'SOFTWARE\Embarcadero\%1:s\15.0',  //xe7
    'SOFTWARE\Embarcadero\%1:s\16.0',  //xe8
    'SOFTWARE\Embarcadero\%1:s\17.0',
    'SOFTWARE\Embarcadero\%1:s\18.0',
    'SOFTWARE\Embarcadero\%1:s\19.0',
    'SOFTWARE\Embarcadero\%1:s\20.0',   //rio
    'SOFTWARE\Embarcadero\%1:s\21.0',   //sydney
    'SOFTWARE\Embarcadero\%1:s\22.0',   //delphi11
    'SOFTWARE\Embarcadero\%1:s\23.0'   //delphi12
);

{$IFNDEF MSWINDOWS}
type
  HKEY = integer;
{$ENDIF}

function RegQueryStringValue(const RootKey: HKey; const SubKeyName, ValueName: String; out ResultStr: String): Boolean;
begin
  ResultStr:= '';

{$IFDEF MSWINDOWS}
  var reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := RootKey;

    if (reg.OpenKeyReadOnly(SubKeyName)) then
    begin
      ResultStr := reg.ReadString(ValueName);
      exit(reg.ValueExists(ValueName));
    end;
  finally
    reg.Free;
  end;
{$ENDIF}
  exit(false);
end;

function RegValueExists(const RootKey: HKey; const SubKeyName, ValueName: String): Boolean;
begin
{$IFDEF MSWINDOWS}
  var reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := RootKey;

    if (reg.OpenKeyReadOnly(SubKeyName)) then
    begin
      exit(reg.ValueExists(ValueName));
    end;
  finally
    reg.Free;
  end;
{$ENDIF}
  exit(false);
end;


function GetCompilerPathAndExecutable(const ProductId: string; const dv: TIDEName; const Config: TConfigDefinition): string;
begin
  Result := GetCompilerPath(ProductId, dv, Config);
  if Result = '' then exit;

  if dv = TIDEName.lazarus then
  {$IFDEF MSWINDOWS}
    exit(TPath.Combine(Result, 'lazbuild.exe'));
  {$ELSE}
    exit(TPath.Combine(Result, 'lazbuild'));
  {$ENDIF}

  if dv <= TIDEName.delphi2010 then exit(TPath.Combine(Result, TPath.Combine('bin', 'dcc32.exe')));

  exit(TPath.Combine(Result, TPath.Combine('bin', 'rsvars.bat')));

end;

function DelphiRegs(const dv: TIDEName; const Config: TConfigDefinition): string;
begin
  var key := Config.AlternateRegistryKey;
  if key = '' then exit(String.Format(DelphiRegsFmt[dv], ['Delphi', 'BDS']));
  exit(String.Format(DelphiRegsFmt[dv], [key, key]));
end;

function GetCompilerPath(const ProductId: string; const dv: TIDEName; const Config: TConfigDefinition): string;
begin
  Result := Config.CompilerPath(ProductId, dv);
  if Result <> '' then exit;

{$IFDEF MSWINDOWS}
  var reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_CURRENT_USER;

    if (reg.OpenKeyReadOnly(DelphiRegs(dv, Config))) then
    begin
      exit(reg.ReadString('RootDir'));
    end;

    exit('');
  finally
    reg.Free;
  end;
{$ENDIF}

end;

function IsProductInstalled(const dv: TIDEName; const ProductId: string; const Config: TConfigDefinition; out ErrorMessage: string): boolean;
begin
  var compiler := GetCompilerPathAndExecutable(ProductId, dv, Config);
  if compiler = '' then
  begin
    if dv = TIDEName.lazarus then
    begin
      ErrorMessage := 'We can''t locate lazarus. You can define the path in config file';
    end
    else
    begin
    {$IFDEF MSWINDOWS}
      ErrorMessage := 'We can''t locate ' + TInstallerFactory.GetInstaller(dv).DisplayName +'.';
    {$ELSE}
      ErrorMessage := TInstallerFactory.GetInstaller(dv).DisplayName + ' is not supported in this OS.';
    {$ENDIF}
    end;
    exit(false);
  end;

  if TFile.Exists(compiler) then exit(true);
  ErrorMessage := 'We can''t find the file "' + compiler + '".';
  exit(false)

end;

function HasAndroid(const ProductId: string; const Config: TConfigDefinition; const dv: TIDEName; const RegBase: string): Boolean;
begin
  Result := false;
{$IFNDEF MSWINDOWS}
{$ELSE}
  var regkey := DelphiRegs(dv, Config);
  var DefaultAndroid, ZipAlign, Adb : string;
  if not RegQueryStringValue(HKEY_CURRENT_USER, regkey + '\PlatformSDKs',RegBase, DefaultAndroid) then exit;

  if dv < TIDEName.delphi12 then
  begin
    if not RegQueryStringValue(HKEY_CURRENT_USER, regkey + '\PlatformSDKs\' + DefaultAndroid,'SDKZipAlignPath', ZipAlign) then exit;
    Result := FileExists(ZipAlign);
    if not Result then exit;
  end;

  if not RegQueryStringValue(HKEY_CURRENT_USER, regkey + '\PlatformSDKs\' + DefaultAndroid,'SDKAdbPath', Adb) then exit;
  Result := FileExists(Adb);
{$ENDIF}
end;

function HasLinux64(const ProductId: string; const Config: TConfigDefinition; const dv: TIDEName): Boolean;
begin
  Result := false;
{$IFNDEF MSWINDOWS}
{$ELSE}
  var DefaultLinux: string;
  var regkey := DelphiRegs(dv, Config);
  if not RegQueryStringValue(HKEY_CURRENT_USER, regkey + '\PlatformSDKs','Default_Linux64', DefaultLinux) then exit;
  Result := true;
{$ENDIF}
end;

function HasOSX64(const ProductId: string; const Config: TConfigDefinition; const dv: TIDEName): Boolean;
begin
  Result := false;
{$IFNDEF MSWINDOWS}
{$ELSE}
  var regkey := DelphiRegs(dv, Config);
  var DefaultOSX64 : string;
  if not RegQueryStringValue(HKEY_CURRENT_USER, regkey + '\PlatformSDKs','Default_OSX64', DefaultOSX64) then exit;

  Result := DefaultOSX64 <> '';
{$ENDIF}
end;

function HasOSXARM64(const ProductId: string; const Config: TConfigDefinition; const dv: TIDEName): Boolean;
begin
  Result := false;
{$IFNDEF MSWINDOWS}
{$ELSE}
  var regkey := DelphiRegs(dv, Config);
  var DefaultOSXARM64 : string;
  if not RegQueryStringValue(HKEY_CURRENT_USER, regkey + '\PlatformSDKs','Default_OSXARM64', DefaultOSXARM64) then exit;

  Result := DefaultOSXARM64 <> '';
{$ENDIF}
end;


function IsSDKInstalled(const ProductId: string; const Config: TConfigDefinition; const dv: TIDEName; const dp: TPlatform): boolean;
begin
  if Config.CompilerPath(ProductId, dv).Trim <> '' then
    Exit(True);

  if dv = TIDEName.lazarus then
  begin
    exit(true);
  end;

  case dp of
    TPlatform.win32intel: exit(true);
    TPlatform.win64intel: exit(true);
    TPlatform.win64Xintel: exit(true);
    TPlatform.macos32intel: exit(true);

    TPlatform.macos64intel: exit(HasOSX64(ProductId, Config, dv));
    TPlatform.macos64arm: exit(HasOSXARM64(ProductId, Config, dv));

    TPlatform.iossimulator: exit(true);
    TPlatform.iosdevice32: exit(true);
    TPlatform.iosdevice64: exit(true);

    TPlatform.android32: exit(HasAndroid(ProductId, Config, dv, 'Default_Android'));
    TPlatform.android64: exit(HasAndroid(ProductId, Config, dv, 'Default_Android64'));
    TPlatform.linux64: exit(HasLinux64(ProductId, Config, dv));
  end;
  exit(true);
end;

function IsPlatAvailable(const key, value: string): boolean;
begin
{$IFNDEF MSWINDOWS}
  Result := true;
{$ELSE}
  Result :=  RegValueExists(HKEY_CURRENT_USER, key, value); if Result then exit;
  Result :=  RegValueExists(HKEY_CURRENT_USER, key + '\Delphi', value); if Result then exit;
  Result :=  RegValueExists(HKEY_CURRENT_USER, key + '\CBuilder', value); if Result then exit;
{$ENDIF}
end;

function IsCompilerAvailable(const ProductId: string; const Ide: TIDEName; const Config: TConfigDefinition; const Compiler: string): boolean;
begin
  Result := TFile.Exists(TPath.Combine(TPath.Combine(GetCompilerPath(ProductId, Ide, Config), 'bin'), Compiler));
end;

const
    PlatPackageUpToXE7: array[TPlatform] of string = (
                                                    'win32debugide',
                                                    'win64debugide',
                                                    'macosxdebugide',
                                                    '',
                                                    '',
                                                    'iosdebugide',
                                                    'iosdebugide',
                                                    '',
                                                    'androiddebugide',
                                                    '',
                                                    'linux64debugide',
                                                    '',
                                                    '');

    PlatPackageXE8ToTokyo: array[TPlatform] of string = (
                                                    'win32debugide',
                                                    'win64debugide',
                                                    'macosxdebugide',
                                                    'macosx64debugide',
                                                    'macosxarm64debugide',
                                                    'ios32debugide', //simulator
                                                    'ios32debugide',
                                                    'ios64debugide',
                                                    'androiddebugide',
                                                    'android64debugide',
                                                    'linux64debugide',
                                                    'macosxarm64debugide', // simulator arm 64
                                                    'win64debugide' //Delphi 12 came with win64Xdebugide. But it was removed in 12.1 since the debugger now is the same as Win64
    );

    PlatPackagesRioOrNewer: array[TPlatform] of TArray<string> = (
                                                    ['win32debugide'],
                                                    ['win64debugide'],
                                                    ['macosxdebugide'],
                                                    ['macosx64debugide'],
                                                    ['macosxarm64debugide'],
                                                    ['ios32debugide', 'delphiios'], //simulator
                                                    ['ios32debugide', 'delphiios'],
                                                    ['ios64debugide', 'delphiios'],
                                                    ['androiddebugide'],
                                                    ['android64debugide'],
                                                    ['linux64debugide'],
                                                    ['macosxarm64debugide', 'delphiios'], // simulator arm 64
                                                    ['win64debugide'] //Delphi 12 came with win64Xdebugide. But it was removed in 12.1 since the debugger now is the same as Win64
    );

    Compilers: Array[TPlatform] of string = (
                                                    'dcc32.exe',
                                                    'dcc64.exe',
                                                    'dccosx.exe',
                                                    'dccosx64.exe',
                                                    'dccosxarm64.exe',
                                                    'dccios32.exe', //simulator
                                                    'dcciosarm.exe', //ios device32
                                                    'dcciosarm64.exe', //ios device 64
                                                    'dccaarm.exe',
                                                    'dccaarm64.exe',
                                                    'dcclinux64.exe',
                                                    'dcciossimarm64.exe',
                                                    '../bin64/bcc64x.exe' //there is no dcc64x.exe (even if we have bcc64x.exe, but in bin64, and only in d12.1 as in d12.0 it is in bin. and d12.0 doesn't support win64x)

    );

function IsPlatformInstalled(const ProductId: string; const Config: TConfigDefinition; const dv: TIDEName; const dp: TPlatform; out ErrorMessage: string): boolean;
var
  PlatPackages: TArray<string>;
begin
  var Installer := TInstallerFactory.GetInstaller(dv);
  if not (Installer is TDelphiInstaller) then exit(true);


  if dv < TIDEName.delphixe2 then exit(dp = TPlatform.win32intel);

  if (dv <= TIDEName.delphixe7) then PlatPackages := [PlatPackageUpToXE7[dp]]
  else if (dv <= TIDEName.delphitokyo) then PlatPackages :=[PlatPackageXE8ToTokyo[dp]]
  else PlatPackages := PlatPackagesRioOrNewer[dp];

  // Do not read the registry if there is a custom compiler path.
  // This allows us to test with a folder of compilers without registry changes.
  if Config.CompilerPath(ProductId, dv).Trim = '' then
  begin
    for var PlatPackage in PlatPackages do
    begin
      Result := IsPlatAvailable(DelphiRegs(dv, Config) + '\Known IDE Packages', '$(BDS)\Bin\' + PlatPackage + TDelphiInstaller(Installer).DllSuffix + '.bpl');
      if not Result then
      begin
        ErrorMessage := PlatPackage + ' is not registered in Known IDE Packages.';
        exit;
      end;
    end;
  end;

  Result := IsCompilerAvailable(ProductId, dv, Config, Compilers[dp]);
  if not Result then
  begin
    ErrorMessage := 'We can''t find ' + Compilers[dp] + '.';
    exit;
  end;


end;

function GetHelpRegistryKeyName(const dv: TIDEName; const Config: TConfigDefinition): string;
begin
  Result := DelphiRegs(dv, Config) + '\Help\HtmlHelp1Files\';
end;

procedure RegisterHelpInIDE(const KeyName, ValueName: string; const HelpFile: string);
begin
{$IFDEF MSWINDOWS}
  var reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if not reg.OpenKey(KeyName, true) then raise Exception.Create('Cannot open the registry key: "' + KeyName + '"');
    reg.WriteString(ValueName, HelpFile);

  finally
    reg.Free;
  end;
{$ENDIF}

end;

procedure UnRegisterHelpInIDE(const HelpRegKeyName, HelpRegValueName: string);
begin
{$IFDEF MSWINDOWS}
  var reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if not reg.OpenKey(HelpRegKeyName, true) then raise Exception.Create('Cannot open the registry key: "' + HelpRegKeyName + '"');
    reg.DeleteValue(HelpRegValueName);
  finally
    reg.Free;
  end;
{$ENDIF}
end;
end.

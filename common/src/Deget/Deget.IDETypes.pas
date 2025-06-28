unit Deget.IDETypes;

interface

uses
  Deget.CoreTypes;

const
  DelphiSuffixes: array[TIDEName] of string =
    ('lazarus', 'd6', 'd7', 'd2005', 'd2006', 'd2007', 'd2009', 'd2010', 'dxe', 'dxe2', 'dxe3', 'dxe4', 'dxe5',
     'dxe6', 'dxe7', 'dxe8', 'dseattle', 'dberlin', 'dtokyo', 'drio', 'dsydney', 'd11', 'd12');
  DelphiVersionNames: array[TIDEName] of string =
    ('lazarus', '6', '7', '2005', '2006', '2007', '2009', '2010', 'xe', 'xe2', 'xe3', 'xe4', 'xe5',
     'xe6', 'xe7', 'xe8', 'seattle', 'berlin', 'tokyo', 'rio', 'sydney', '11', '12');
  PackageSuffixes: array[TIDEName] of string =
    ('lazarus', '60', '70', '90', '100', '110', '120', '140', '150', '160', '170', '180', '190',
     '200', '210', '220', '230', '240', '250', '260', '270', '280', '290');
  DelphiRegistryRoot: array[TIDEName] of string = (
    'Lazarus',
    'Software\Borland\Delphi\6.0',
    'Software\Borland\Delphi\7.0',
    'Software\Borland\BDS\3.0',
    'Software\Borland\BDS\4.0',
    'Software\Borland\BDS\5.0',
    'Software\CodeGear\BDS\6.0',
    'Software\CodeGear\BDS\7.0',
    'Software\Embarcadero\BDS\8.0',
    'Software\Embarcadero\BDS\9.0',
    'Software\Embarcadero\BDS\10.0',
    'Software\Embarcadero\BDS\11.0',
    'Software\Embarcadero\BDS\12.0',
    'Software\Embarcadero\BDS\14.0',
    'Software\Embarcadero\BDS\15.0',
    'Software\Embarcadero\BDS\16.0',
    'Software\Embarcadero\BDS\17.0',
    'Software\Embarcadero\BDS\18.0',
    'Software\Embarcadero\BDS\19.0',
    'Software\Embarcadero\BDS\20.0',
    'Software\Embarcadero\BDS\21.0',
    'Software\Embarcadero\BDS\22.0',
    'Software\Embarcadero\BDS\23.0'
  );

  DelphiNames: array[TIDEName] of string = (
    'Lazarus',
    'Delphi 6',
    'Delphi 7',
    'Delphi 2005',
    'Delphi 2006',
    'Delphi 2007',
    'Delphi 2009',
    'Delphi 2010',
    'Delphi XE',
    'Delphi XE2',
    'Delphi XE3',
    'Delphi XE4',
    'Delphi XE5',
    'Delphi XE6',
    'Delphi XE7',
    'Delphi XE8',
    'Delphi 10 Seattle',
    'Delphi 10.1 Berlin',
    'Delphi 10.2 Tokyo',
    'Delphi 10.3 Rio',
    'Delphi 10.4 Sydney',
    'Delphi 11',
    'Delphi 12'
  );

  RadStudioNames: array[TIDEName] of string = (
    'Lazarus',
    'Rad Studio 6',
    'Rad Studio 7',
    'Rad Studio 2005',
    'Rad Studio 2006',
    'Rad Studio 2007',
    'Rad Studio 2009',
    'Rad Studio 2010',
    'Rad Studio XE',
    'Rad Studio XE2',
    'Rad Studio XE3',
    'Rad Studio XE4',
    'Rad Studio XE5',
    'Rad Studio XE6',
    'Rad Studio XE7',
    'Rad Studio XE8',
    'Rad Studio 10 Seattle',
    'Rad Studio 10.1 Berlin',
    'Rad Studio 10.2 Tokyo',
    'Rad Studio 10.3 Rio',
    'Rad Studio 10.4 Sydney',
    'Rad Studio 11',
    'Rad Studio 12'
  );


  PlatformsInDelphi: array[TIDEName] of TPlatformSet =
    (
      [], // lazarus
      [win32intel], [win32intel], [win32intel], [win32intel], [win32intel], [win32intel], [win32intel], [win32intel], // d6..dxe
      [win32intel, win64intel, macos32intel], //dxe2
      [win32intel, win64intel, macos32intel],  // dxe3
      [win32intel, win64intel, macos32intel, iOSSimulator, iOSDevice32],  // dxe4
      [win32intel, win64intel, macos32intel, iOSSimulator, iOSDevice32, android32],  // dxe5
      [win32intel, win64intel, macos32intel, iOSSimulator, iOSDevice32, android32],  // dxe6
      [win32intel, win64intel, macos32intel, iOSSimulator, iOSDevice32, android32],  // dxe7
      [win32intel, win64intel, macos32intel, iOSSimulator, iOSDevice32, android32, iOSDevice64],  // dxe8
      [win32intel, win64intel, macos32intel, iOSSimulator, iOSDevice32, android32, iOSDevice64],  // seattle
      [win32intel, win64intel, macos32intel, iOSSimulator, iOSDevice32, android32, iOSDevice64],  // berlin
      [win32intel, win64intel, macos32intel, iOSSimulator, iOSDevice32, android32, iOSDevice64, Linux64],  // tokyo
      [win32intel, win64intel, macos32intel, iOSSimulator, iOSDevice32, android32, iOSDevice64, Linux64, macos64intel, Android64],  // rio
      [win32intel, win64intel, iOSSimulator, android32, iOSDevice64, Linux64, macos64intel, Android64],  // sydney
      [win32intel, win64intel, android32, iOSDevice64, Linux64, macos64intel, Android64, macos64arm],  // d11
      [win32intel, win64intel, android32, iOSDevice64, Linux64, macos64intel, Android64, macos64arm, iossimulator64arm, win64Xintel]  // d12
    );

function TrySuffixToIDEName(const Suffix: string; var IDEName: TIDEName): Boolean;
function GetLibSuffix(const IDEName: TIDEName): string;
function GetLibSuffixDProj(const IDEName: TIDEName): string;

implementation

uses
  System.SysUtils;

function TrySuffixToIDEName(const Suffix: string; var IDEName: TIDEName): Boolean;
begin
  for var dv := Low(TIDEName) to High(TIDEName) do
    if DelphiSuffixes[dv] = Suffix then
    begin
      IDEName := dv;
      Exit(True);
    end;
  Result := False;
end;

function GetLibSuffix(const IDEName: TIDEName): string;
begin
  if IDEName >= delphi11 then
    Result := 'auto'
  else
    Result := PackageSuffixes[IDEName];
end;

function GetLibSuffixDProj(const IDEName: TIDEName): string;
begin
  if IDEName >= delphi11 then
    Result := '$(Auto)'
  else
    Result := PackageSuffixes[IDEName];
end;

end.

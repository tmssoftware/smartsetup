unit Deget.CoreTypes;

interface

type
  //Remember to never remove an IDE, even if
  //we don't support it anymore. This would make the
  //uninstallers for those versions fail
  TIDEName = (
    lazarus,
    delphi6,
    delphi7,
    delphi2005,
    delphi2006,
    delphi2007,
    delphi2009,
    delphi2010,
    delphixe,
    delphixe2,
    delphixe3,
    delphixe4,
    delphixe5,
    delphixe6,
    delphixe7,
    delphixe8,
    delphiseattle,
    delphiberlin,
    delphitokyo,
    delphirio,
    delphisydney,
    delphi11,
    delphi12
    );

    TIDENameSet = set of TIDEName;

const
    ValidIDEs = [Low(TIDEName)..High(TIDEName)];
    DelphiIDENames = [TIDEName.delphi6..TIDEName.delphi12];

const
    IDEId: array[TIDEName] of string = (
    'lazarus',
    'delphi6',
    'delphi7',
    'delphi2005',
    'delphi2006',
    'delphi2007',
    'delphi2009',
    'delphi2010',
    'delphixe',
    'delphixe2',
    'delphixe3',
    'delphixe4',
    'delphixe5',
    'delphixe6',
    'delphixe7',
    'delphixe8',
    'delphiseattle',
    'delphiberlin',
    'delphitokyo',
    'delphirio',
    'delphisydney',
    'delphi11',
    'delphi12'
    );

const
  Mutex_WebCore = 'Global\tms-smart-setup-B2ACE533-D1C4-4C4E-8317-139704056F97';
  Mutex_IDEPath = 'Global\tms-smart-setup-472A3236-3A5A-4EF4-BE4A-E384EB628945';
  Mutex_PathOverride = 'Global\tms-smart-setup-0BBA6135-F93A-46A3-A4CC-A343999140E4';

type
  //Remember to never remove a platform, even if
  //we don't support it anymore. This would make the
  //uninstallers for those platform fail

  TPlatform =
  (     // WARNING: Never change the order of this items
        win32intel,
        win64intel,
        macos32intel,
        macos64intel,
        macos64arm,
        iossimulator,
        iosdevice32,
        iosdevice64,
        android32,
        android64,
        linux64,
        iossimulator64arm,
        win64Xintel
  );

  TPlatformSet = Set of TPlatform;

const
    ValidPlatforms = [Low(TPlatform)..High(TPlatform)];

    PlatformId: array[TPlatform] of string = (
        'win32intel',
        'win64intel',
        'macos32intel',
        'macos64intel',
        'macos64arm',
        'iossimulator',
        'iosdevice32',
        'iosdevice64',
        'android32',
        'android64',
        'linux64',
        'iossimulator64arm',
        'win64xintel'
    );
type
{$SCOPEDENUMS ON}
  TBuildConfig = (Debug, Release);
  TPackageType = (Package, Exe);
  TPackageFolders =  Array[TIDEName] of string;
  TLibSuffixes = Array[TIDEName] of string;


const
  BuildConfigs: array[TBuildConfig] of string = ('Debug', 'Release');
  BinprojExtension = '.binproj';

function TryGetIDEName(const value: string; var IDEName: TIDEName): Boolean;
function GetIDEName(const value: string): TIDEName;
function GetPlatformName(const value: string): TPlatform;

implementation

function TryGetIDEName(const value: string; var IDEName: TIDEName): Boolean;
begin
  for var dv := Low(TIDEName) to High(TIDEName) do
    if IDEId[dv] = value then
    begin
      IDEName := dv;
      Exit(True);
    end;
  Result := False;
end;

function GetIDEName(const value: string): TIDEName;
begin
  if not TryGetIDEName(value, Result) then
    Result := TIDEName(-1);
end;

function GetPlatformName(const value: string): TPlatform;
begin
  for var plat := Low(TPlatform) to High(TPlatform) do
  begin
    if PlatformId[plat] = value then exit(plat);
  end;
  Result := TPlatform(-1);
end;

end.

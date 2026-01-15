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
    delphi12,
    delphi13
    );

    TIDENameSet = set of TIDEName;

const
    ValidIDEs = [Low(TIDEName)..High(TIDEName)];
    DelphiIDENames = [TIDEName.delphi6..TIDEName.delphi13];

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
    'delphi12',
    'delphi13'
    );

const
  Mutex_WebCore = 'Global\tms-smart-setup-B2ACE533-D1C4-4C4E-8317-139704056F97';
  Mutex_IDEPath = 'Global\tms-smart-setup-472A3236-3A5A-4EF4-BE4A-E384EB628945';
  Mutex_PathOverride = 'Global\tms-smart-setup-0BBA6135-F93A-46A3-A4CC-A343999140E4';
  Mutex_Resinator = 'Global\tms-smart-setup-8D4F142B-5E21-4155-877D-79C18132A14B';

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
  TLibSuffixes = Array[TIDEName] of string;

  TPlusState = (Single, Plus, Auto);
  TPackageFolder = record
    PlusState: TPlusState;
    Value: string;
    class operator Initialize(out Dest: TPackageFolder);
  end;
  TPackageFolders =  Array[TIDEName] of TPackageFolder;

const
  BuildConfigs: array[TBuildConfig] of string = ('Debug', 'Release');
  BinprojExtension = '.binproj';

function PlatformMacroString(const IDEName: TIDEName; const PlatType: TPlatform): string;
function TryGetIDEName(const value: string; var IDEName: TIDEName): Boolean;
function GetIDEName(const value: string): TIDEName;
function GetPlatformName(const value: string): TPlatform;
function StrToIDEName(const Value: string; ValidIDEs: TIDENameSet = ValidIDEs): TIDEName;
function StrToIDENameSet(const Value: string; ValidIDEs: TIDENameSet = ValidIDEs): TIDENameSet;
function StrToDelphiIDEName(const Value: string): TIDEName;
function StrToDelphiIDENameSet(const Value: string): TIDENameSet;

implementation

uses
  StrUtils, SysUtils;

function PlatformMacroString(const IDEName: TIDEName; const PlatType: TPlatform): string;
const
    PlatformMacroValueStr: array[TPlatform] of string =
    ('Win32', 'Win64', 'OSX32', 'OSX64', 'OSXARM64', 'iOSSimulator', 'iOSDevice32', 'iOSDevice64',
     'Android', 'Android64', 'Linux64', 'iOSSimARM64', 'Win64x');
begin
  Result := PlatformMacroValueStr[PlatType];
  if (PlatType = TPlatform.iOSDevice32) and (IDEName <= delphixe7) then
    Result := 'iOSDevice';

end;

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

function StrToDelphiIDEName(const Value: string): TIDEName;
begin
  Result := StrToIDEName(Value, DelphiIDENames);
end;

function StrToDelphiIDENameSet(const Value: string): TIDENameSet;
begin
  Result := StrToIDENameSet(Value, DelphiIDENames);
end;

function StrToIDEName(const Value: string; ValidIDEs: TIDENameSet = ValidIDEs): TIDEName;
begin
  if not TryGetIDEName(Value, Result) then
    raise Exception.CreateFmt('Unknown IDE: %s', [Value]);
  if not (Result in ValidIDEs) then
    raise Exception.CreateFmt('Invalid IDE: %s', [Value]);
end;

function StrToIDENameRange(const Value: string; ValidIDEs: TIDENameSet = ValidIDEs): TIDENameSet;
begin
  Result := [];
  var IDENames := SplitString(Value, '-');
  case Length(IDENames) of
    1: Result := Result + [StrToIDEName(Value, ValidIDEs)];
    2:
      begin
        var LowIDE := Low(TIDEName);
        var HighIDE := High(TIDEName);
        if IDENames[0] <> '' then
          LowIDE := StrToIDEName(IDENames[0], ValidIDEs);
        if IDENames[1] <> '' then
          HighIDE := StrToIDEName(IDENames[1], ValidIDEs);
        for var IDEName := LowIDE to HighIDE do
          if IDEName in ValidIDEs then
            Result := Result + [IDEName];
      end;
  else
    raise Exception.Create('Invalid syntax for IDE range: ' + Value);
  end;
end;

function StrToIDENameSet(const Value: string; ValidIDEs: TIDENameSet = ValidIDEs): TIDENameSet;
begin
  Result := [];
  for var IDERange in SplitString(Value, ',') do
    Result := Result + StrToIDENameRange(IDERange, ValidIDEs);
end;

{ TPackageFolder }

class operator TPackageFolder.Initialize(out Dest: TPackageFolder);
begin
  Dest.PlusState := TPlusState.Single;
  Dest.Value := '';
end;

end.

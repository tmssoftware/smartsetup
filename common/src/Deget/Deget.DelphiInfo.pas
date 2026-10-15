unit Deget.DelphiInfo;

interface

{$IFDEF MSWINDOWS}

uses
  Windows, TypInfo,
  Deget.IDEInfo,
  Deget.CoreTypes,
  Classes,
  Deget.IDETypes;

// this unit contains useful tools to retrieve and update information about Delphi IDE, including platforms:
// register package, retrieve search path, update search path, retrieve root dir, lib dir, etc.

type
  TDelphiIDEInfo = class;

  TDelphiIDEPlatformInfo = class(TInterfacedObject, IDelphiPlatformInfo)
  public
    const
      IDEPathNames: array[TDelphiPathType] of string = (
         'Delphi Search Path',
         'Delphi Browsing Path',
         'Debug DCU Path',
         'C++ Library Path',
         'C++ Include Path',
         'C++ Browsing Path',
         'C++ Library Path (Clang 32)',
         'C++ Include Path (Clang 32)',
         'C++ Browsing Path (Clang 32)'
        );
      IDEPathIdents: array[TDelphiPathType] of string = (
         'Search Path',
         'Browsing Path',
         'Debug DCU Path',
         'LibraryPath',
         'IncludePath',
         'BrowsingPath',
         'LibraryPath_Clang32',
         'IncludePath_Clang32',
         'BrowsingPath_Clang32'
        );
      CppPathTypes = [ptCppLibraryPath, ptCppIncludePath, ptCppBrowsingPath,
      ptCppClang32LibraryPath, ptCppClang32IncludePath, ptCppClang32BrowsingPath];
  strict private
    FIDE: TDelphiIDEInfo;
    FPlatType: TPlatform;
//    FDefaultBplOutputFolder: string;
    function IDEPathsRegistryKey(PathType: TDelphiPathType): string;
//    function RetrieveBplOutputDirUsingMSBuild(APlat: TPlatform): string;
  private
    function GetIDEName: TIDEName;
  strict protected

    property IDEName: TIDEName read GetIDEName;
    property IDE: TDelphiIDEInfo read FIDE;
  public
    { IDelphiPlatformInfo method }
    function PlatType: TPlatform;
    function IDEInfo: IDelphiIDEInfo;
    procedure AddIDEPath(PathType: TDelphiPathType; const PathsToAdd: string);
    procedure RemoveIDEPath(PathType: TDelphiPathType; const PathsToRemove: string);
    function GetIDEPath(PathType: TDelphiPathType): string;
    // Updates the contents of the specified path. Warnings, this will replace the whole content
    // if you want just to add a new directory to the path (collection of paths) use AddIDEPath
    procedure SetIDEPath(PathType: TDelphiPathType; const Value: string);
    function PlatformMacroValue: string;
    function ProductVersionMacroValue: string;
    function BinaryPackageName(const BaseName: string; AddLibSuffix: Boolean; const IsExe: boolean; const NonStandardSuffixes: TLibSuffixes): string;
  public
    constructor Create(AIDE: TDelphiIDEInfo; APlatType: TPlatform);

    // Returns Delphi lib directory. Note: this is NOT the root lib dir, but instead
    // the directory which contains library files (dcp/dcu, etc.), for the specified platform,
    // using release config. Can be used for manual compilation
    // Example: "C:\Program Files (x86)\Embarcadero\Studio\15.0\Lib\win32\release"
    function LibDir: string;
    // Same as LibDir but using Debug binaries (they are different when compiling for iOS/Android
    function DebugLibDir: string;

    function RedistDir: string;

    // Returns the name of MSBuild platform Build used to build for that platform
    function BuildName: string;

    // The "id" of the platform, used in several places like $(Platform) macro, output directories, etc..
    // In XE8 "iOSDevice" was renamed to "iOSDevice32".
    function PlatformId: string;

//    // Retrieves the default bpl output folder for the specified delphi version/platform
//    // This function is useful to know where the BPL folder is located if the BplOutputDir compilation parameter is empty
//    // It's also useful during the install to create a sym link in that folder so that product bpl if found by Windows,
//    // since the DefaultBplOutputFolder is added to Windows System Path environment variable by Delphi
//    function DefaultBplOutputFolder: string;
  end;

  TDelphiIDEInfo = class(TInterfacedObject, IDelphiIDEInfo)
  strict private
    FIDEName: TIDEName;
    FRootDir: string;
    FHasCustomCompilerPaths: boolean;
    FRootRegistryKey: string;
    function GetEnvVarOverride(const VarName: string; const IDEPlatform: TPlatform): string;
    procedure SetEnvVarOverride(const VarName, Value: string; const IDEPlatform: TPlatform);
  private
    function InternalBaseKey(const SubKey: string): string;
  public
    class function IDEPlatforms: TPlatformSet;
    function BaseKey64(const SubKey: string;
      const IDEPlatform: TPlatform): string;
    function BaseKey(const SubKey: string = ''): string;
  public
    { IDelphiIDEInfo method }
    function IDEName: TIDEName;
    function GetPlatform(PlatType: TPlatform): IDelphiPlatformInfo;
    procedure RegisterPackage(const BinaryFileName: string; const IDEPlatform: TPlatform; const Description: string = '');
    procedure UnregisterPackage(const BinaryFileName: string; const IDEPlatform: TPlatform);
  public
    constructor Create(const AIDEName: TIDEName; const ARegistryName: string = ''; const ACompilerPath: string = '');
    destructor Destroy; override;
    procedure ForceIDEUpdate;

    function GetPathOverride(const IDEPlatform: TPlatform): string;
    procedure AddFolderToPathOverride(const PathsToAdd: string; const Platform: TPlatform; AddToBeginning: Boolean = True);
    procedure RemoveFolderFromPathOverride(const PathsToRemove: string);

    // Returns the location of Delphi Root Directory (install directory)
    // Example: "C:\Program Files (x86)\Embarcadero\Studio\15.0"
    function RootDir: string;

    //True if we aren't reading the compiler paths from the registry.
    function HasCustomCompilerPaths: boolean;

    // Returns the full file name of rsvars.bat file
    // Example: "C:\Program Files (x86)\Embarcadero\Studio\15.0\bin\rsvars.bat"
    function RsvarsFile: string;

    // Returns the full file name of dcc32.exe file
    // Example: "C:\Program Files (x86)\Embarcadero\Studio\15.0\bin\dcc32.exe"
    function Dcc32File: string;

    // Returns the full file name of Brcc32.exe file
    function Brcc32File: string;

    // Returns the full file name of Bds.exe file
    function BdsFile: string;

    // Location of resinator.exe
    function ResinatorFile: string;


    // Returns Delphi bin directory
    // Example: "C:\Program Files (x86)\Embarcadero\Studio\15.0\bin"
    function BinDir: string;

    function Bin64Dir: string;

    function GetEnvVarOverrides(const IDEPlatform: TPlatform): TArray<TEnvVar>;

//    property Plats[Plat: TDelphiPlatformType]: TDelphiIDEPlatformInfo read GetPlats; default;
  end;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses
  Registry, System.Types, System.SysUtils, System.IOUtils, System.StrUtils,
  Deget.Registry, Deget.IDEUtils, SyncObjs;

{ TDelphiIDEInfo }

procedure TDelphiIDEInfo.AddFolderToPathOverride(const PathsToAdd: string; const Platform: TPlatform; AddToBeginning: Boolean = True);
begin
  var Lock := TMutex.Create(nil, false, Mutex_PathOverride);
  try
    Lock.Acquire;
    try
      SetEnvVarOverride('PATH', AddPaths(GetEnvVarOverride('PATH', Platform), PathsToAdd, AddToBeginning), Platform);
    finally
      Lock.Release;
    end;
  finally
    Lock.Free;
  end;
end;

function TDelphiIDEInfo.InternalBaseKey(const SubKey: string): string;
begin
//  Logger.Note(Format('Building IDE base registry key with subkey "%s', [SubKey]));
  Result := FRootRegistryKey;
//  Logger.Note(Format('Base key value: %s', [Result]));

  if SubKey <> '' then
  begin
    if SubKey[1] <> '\' then
      Result := Result + '\';
    Result := Result + SubKey;
//    Logger.Note(Format('Base key with subkey value: %s', [Result]));
  end;
end;


function TDelphiIDEInfo.BaseKey64(const SubKey: string; const IDEPlatform: TPlatform): string;
begin
  Result := InternalBaseKey(SubKey);

  if (SubKey = 'Environment Variables') then //tricky one, added in d13, not present in d12
  begin
    if IDEName = TIDEName.delphi12 then exit;
  end;

  case IDEPlatform of
    win32intel: ;

    win64intel,
    win64Xintel: Result := Result + ' x64';
    else raise Exception.Create('Error: Delphi IDE must be 32 or 64 bit.');
  end;
end;

function TDelphiIDEInfo.BaseKey(const SubKey: string): string;
begin
  if   (SubKey = 'Known IDE Packages')
    or (SubKey = 'Known Packages')
    or (SubKey = 'Experts')
    or (SubKey = 'Disabled Packages')
    or (SubKey = 'Disabled IDE Packages')
    or (SubKey = 'Environment Variables')
    then
    begin
      //Call BaseKey64 for this entries.
      raise Exception.Create('Internal error. Can''t change the key ''' + SubKey + ''' without knowing the IDE bitness.');
    end;

  Result := InternalBaseKey(SubKey);
end;

function TDelphiIDEInfo.BinDir: string;
begin
  Result := TPath.Combine(RootDir, 'bin');
end;

function TDelphiIDEInfo.Bin64Dir: string;
begin
  Result := TPath.Combine(RootDir, 'bin64');
end;

constructor TDelphiIDEInfo.Create(const AIDEName: TIDEName; const ARegistryName: string = ''; const ACompilerPath: string = '');
begin
  FIDEName := AIDEName;
  FRootRegistryKey := DelphiRegistryRoot[Self.IDEName];
  if ARegistryName <> '' then
  begin
    FRootRegistryKey := StringReplace(FRootRegistryKey, 'Delphi', ARegistryName, []);
    FRootRegistryKey := StringReplace(FRootRegistryKey, 'BDS', ARegistryName, []);
  end;

  FHasCustomCompilerPaths := ACompilerPath <> '';
  if FHasCustomCompilerPaths then
    FRootDir := ACompilerPath
  else
    RegQueryStringValue(BaseKey, 'rootdir', FRootDir);

  FRootDir := ExcludeTrailingPathDelimiter(FRootDir);
end;

function TDelphiIDEInfo.Dcc32File: string;
begin
  Result := TPath.Combine(BinDir, 'dcc32.exe');
end;

function TDelphiIDEInfo.Brcc32File: string;
begin
  Result := TPath.Combine(BinDir, 'brcc32.exe');
end;

function TDelphiIDEInfo.BdsFile: string;
begin
  Result := TPath.Combine(BinDir, 'bds.exe');
end;

function TDelphiIDEInfo.ResinatorFile: string;
begin
  Result := TPath.Combine(Bin64Dir, 'resinator.exe');
end;

function TDelphiIDEInfo.IDEName: TIDEName;
begin
  Result := FIDEName;
end;

class function TDelphiIDEInfo.IDEPlatforms: TPlatformSet;
begin
  Result := [TPlatform.win32intel, TPlatform.win64intel];
end;

destructor TDelphiIDEInfo.Destroy;
begin
  inherited;
end;

procedure TDelphiIDEInfo.ForceIDEUpdate;
begin
//  Logger.Note('Forcing IDE Update in registry');
  case IDEName of
    delphi7: ;  // Delphi 7 doesn't have that option
  else
    //Shouldn't require a mutex, as we aren't reading the key.
    RegWriteString(HKEY_CURRENT_USER, BaseKey('\Globals'), 'ForceEnvOptionsUpdate', '1');
  end;
end;

function TDelphiIDEInfo.GetEnvVarOverride(const VarName: string; const IDEPlatform: TPlatform): string;
begin
  RegReadString(HKEY_CURRENT_USER, BaseKey64('Environment Variables', IDEPlatform), VarName, Result);
end;

function TDelphiIDEInfo.GetEnvVarOverrides(const IDEPlatform: TPlatform): TArray<TEnvVar>;
var
  R: TRegistry;
begin
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    var Ok := R.OpenKeyReadOnly(BaseKey64('Environment Variables', IDEPlatform));
    if Ok then
    begin
      var Values := TStringList.Create;
      try
        R.GetValueNames(Values);
        SetLength(Result, Values.Count);
        for var i := 0 to Values.Count - 1 do
        begin
          Result[i].Name := Values[i];
          Result[i].Value := R.ReadString(Values[i]);
        end;
      finally
        Values.Free;
      end;
    end
    else
      Result := nil;
  finally
    R.Free;
  end;
end;

function TDelphiIDEInfo.GetPathOverride(const IDEPlatform: TPlatform): string;
begin
  Result := GetEnvVarOverride('PATH', IDEPlatform);
end;

function TDelphiIDEInfo.GetPlatform(PlatType: TPlatform): IDelphiPlatformInfo;
begin
  Result := TDelphiIDEPlatformInfo.Create(Self, PlatType);
end;

//function TDelphiIDEInfo.GetPlats(Plat: TDelphiPlatformType): TDelphiIDEPlatformInfo;
//begin
//  Result := FPlats[Plat];
//end;

procedure TDelphiIDEInfo.RegisterPackage(const BinaryFileName: string; const IDEPlatform: TPlatform; const Description: string = '');
var
  LDescription: string;
begin
//  Logger.Note(Format('Registering package %s - %s', [BinaryFileName, Description]));
  if BinaryFileName <> '' then
  begin
    if Description = '' then
      LDescription := '(Untitled)'
    else
      LDescription := Description;
    //Shouldn't require a mutex, as we aren't reading the key.

    if not TFile.Exists(BinaryFileName) then raise Exception.Create('Internal error. Trying to register package ' + BinaryFileName + ' that doesn''t exist');

    //CanCreate is false, so if "Known Packages x64" doesn't exist we don't create it. Might happen for Delphi 12.0, or if we didn't install Win64 IDE.
    RegWriteString(HKEY_CURRENT_USER, BaseKey64('Known Packages', IDEPlatform), BinaryFileName, LDescription, false);
    RegDeleteValue(HKEY_CURRENT_USER, BaseKey64('Disabled Packages', IDEPlatform), BinaryFileName);

  end;
end;

procedure TDelphiIDEInfo.RemoveFolderFromPathOverride(const PathsToRemove: string);
begin
  var Lock := TMutex.Create(nil, false, Mutex_PathOverride);
  try
    Lock.Acquire;
    try
      for var IDEPlatform in TDelphiIDEInfo.IDEPlatforms do
      begin
        SetEnvVarOverride('PATH', RemovePaths(GetEnvVarOverride('PATH', IDEPlatform), PathsToRemove), IDEPlatform);
      end;
    finally
      Lock.Release;
    end;
  finally
    Lock.Free;
  end;
end;

function TDelphiIDEInfo.RootDir: string;
begin
  Result := FRootDir;
end;

function TDelphiIDEInfo.HasCustomCompilerPaths: Boolean;
begin
  Result := FHasCustomCompilerPaths;
end;

function TDelphiIDEInfo.RsvarsFile: string;
begin
  Result := TPath.Combine(BinDir, 'rsvars.bat');
end;

procedure TDelphiIDEInfo.SetEnvVarOverride(const VarName, Value: string; const IDEPlatform: TPlatform);
begin
  var Key := BaseKey64('Environment Variables', IDEPlatform);
  if RegKeyExists(HKEY_CURRENT_USER, Key) then
    RegWriteString(HKEY_CURRENT_USER, Key, VarName, Value);
end;

procedure TDelphiIDEInfo.UnregisterPackage(const BinaryFileName: string; const IDEPlatform: TPlatform);
begin
//  Logger.Note(Format('Unregistering package %s', [FileName]));
  if BinaryFileName <> '' then
    //Shouldn't require a mutex, as we aren't reading the key.
    RegDeleteValue(HKEY_CURRENT_USER, BaseKey64('Known Packages', IDEPlatform), BinaryFileName);
end;

{ TDelphiIDEPlatformInfo }

procedure TDelphiIDEPlatformInfo.AddIDEPath(PathType: TDelphiPathType; const PathsToAdd: string);
begin
  var Lock := TMutex.Create(nil, false, Mutex_IDEPath);
  try
    Lock.Acquire;
    try
      SetIDEPath(PathType, AddPaths(GetIDEPath(PathType), PathsToAdd));
    finally
      Lock.Release;
    end;
  finally
    Lock.Free;
  end;
end;

function TDelphiIDEPlatformInfo.BinaryPackageName(const BaseName: string; AddLibSuffix: Boolean; const IsExe: boolean; const NonStandardSuffixes: TLibSuffixes): string;
var
  Suffix: string;
begin
  if AddLibSuffix then
  begin
    if NonStandardSuffixes[IDEName] = '' then Suffix := PackageSuffixes[IDEName] else Suffix := NonStandardSuffixes[IDEName];
  end
  else
  begin
    Suffix := '';
  end;

  case PlatType of
    Win32Intel:
    begin
      if IsExe then exit(Format('%s.exe', [BaseName]));
      Result := Format('%s%s.bpl', [BaseName, Suffix]);
    end;
    Win64Intel,
    Win64XIntel,
    WinArm64EC:
    begin
      if IsExe then exit(Format('%s.exe', [BaseName]));
      Result := Format('%s%s.bpl', [BaseName, Suffix]);
    end;
    macOS32Intel, macOS64Intel:
      Result := Format('bpl%s%s.dylib', [BaseName, Suffix]);
    Linux64:
      Result := Format('bpl%s%s.so', [BaseName, Suffix]);
  else
    Result := ''; // other platforms do not generate packages
  end;

  if IsExe then exit(Format('%s', [BaseName]));

end;

function TDelphiIDEPlatformInfo.BuildName: string;
begin
  Result := PlatformId; // for now, it's the same
end;

constructor TDelphiIDEPlatformInfo.Create(AIDE: TDelphiIDEInfo; APlatType: TPlatform);
begin
  FIDE := AIDE;
  FPlatType := APlatType;
end;

function TDelphiIDEPlatformInfo.DebugLibDir: string;
begin
  Result := TPath.Combine(IDE.RootDir, 'lib');
  if IDEName >= delphixe then
    Result := TPath.Combine(Result, PlatformId + '\debug')
  else
    Result := TPath.Combine(Result, 'debug')
end;

function TDelphiIDEPlatformInfo.GetIDEName: TIDEName;
begin
  Result := FIDE.IDEName;
end;

function TDelphiIDEPlatformInfo.GetIDEPath(PathType: TDelphiPathType): string;
begin
  if not RegReadString(HKEY_CURRENT_USER, IDEPathsRegistryKey(PathType), IDEPathIdents[PathType], Result) then
    Result := '';
//  Logger.Note(Format('IDE Path %s read. Value: %s', [IDEPathNames[PathType], Result]));
end;

function TDelphiIDEPlatformInfo.IDEInfo: IDelphiIDEInfo;
begin
//  Result := FIDEInfo;
  Result := FIDE;
end;

function TDelphiIDEPlatformInfo.IDEPathsRegistryKey(PathType: TDelphiPathType): string;
const
  _PlatformRegSubKey: array[TPlatform] of string =
    ('Win32', 'Win64', 'OSX32', 'OSX64', 'OSXARM64', 'iOSSimulator', 'iOSDevice32', 'iOSDevice64',
     'Android32', 'Android64', 'Linux64', 'iOSSimARM64', 'Win64x', 'WinARM64EC');

  function PlatSubKey: string;
  begin
    Result := _PlatformRegSubKey[PlatType];
    if (PlatType = iOSDevice32) and (IDEName <= delphixe7) then
      Result := 'iOSDevice';
  end;

  function DelphiLibraryKey: string;
  begin
    case IDEName of
      delphi7..delphixe:
        Result := IDE.BaseKey('\Library');
    else
      // default
      // dxe2..dxe5:
      Result := IDE.BaseKey('\Library\') + PlatSubKey;
    end;
  end;

  function CBuilderLibraryKey: string;
  begin
    case IDEName of
      delphi7..delphi2006:
        Result := 'CppPaths';

      delphi2007..delphixe:
        Result := IDE.BaseKey('C++\Paths');
    else
      // default
      // dxe2..dxe5:
      Result := IDE.BaseKey('C++\Paths\') + PlatSubKey;
    end;
  end;

begin
  if PathType in CppPathTypes then
    Result := CBuilderLibraryKey
  else
    Result := DelphiLibraryKey;
end;

function TDelphiIDEPlatformInfo.LibDir: string;
begin
  Result := TPath.Combine(IDE.RootDir, 'lib');
  if IDEName >= delphixe then
    Result := TPath.Combine(Result, PlatformId + '\release');
end;

function TDelphiIDEPlatformInfo.PlatformId: string;
begin
  Result := PlatformMacroString(IDEName, PlatType);
end;

function TDelphiIDEPlatformInfo.PlatformMacroValue: string;
begin
  Result := PlatformId;
end;

function TDelphiIDEPlatformInfo.ProductVersionMacroValue: string;
begin
  Result := DelphiProductVersion[IDEInfo.IDEName];
end;

function TDelphiIDEPlatformInfo.PlatType: TPlatform;
begin
  Result := FPlatType;
end;

function TDelphiIDEPlatformInfo.RedistDir: string;
begin
  Result := TPath.Combine(IDE.RootDir, 'Redist');
  if PlatType = TPlatform.macos64arm then
    Result := TPath.Combine(Result, 'osx64')
  else
  if PlatType = TPlatform.win64Xintel then
  begin
    //This looks like a bug, maybe later will be fixed.
    //But in 12.1 at least, we don't need redist, but yes the folder below.
    //I think it is a bug, because a "plain" "msbuild TMSVCLUIPackPkg.dproj"
    //from a command line with the correct rsvars will fail. Making it impossible to compile from command line.
    //dcc64.cfg contains the line: -u"c:\program files (x86)\embarcadero\studio\23.0\lib\win64\release"
    //Result := TPath.Combine(Result, 'win64');
    Result := TPath.Combine(IDE.RootDir,'lib', 'win64', 'release');

  end
  else
    Result := TPath.Combine(Result, PlatformId);
end;

procedure TDelphiIDEPlatformInfo.RemoveIDEPath(PathType: TDelphiPathType; const PathsToRemove: string);
begin
  if PathsToRemove = '' then Exit;
  var Lock := TMutex.Create(nil, false, Mutex_IDEPath);
  try
    Lock.Acquire;
    try
      SetIDEPath(PathType, RemovePaths(GetIDEPath(PathType), PathsToRemove));
    finally
      Lock.Release;
    end;
  finally
    Lock.Free;
  end;
end;


procedure TDelphiIDEPlatformInfo.SetIDEPath(PathType: TDelphiPathType; const Value: string);
var
  Key: string;
begin
  Key := IDEPathsRegistryKey(PathType);
  if RegKeyExists(HKEY_CURRENT_USER, Key) then
    RegWriteString(HKEY_CURRENT_USER, Key, IDEPathIdents[PathType], Value);
end;

{$ENDIF}

end.

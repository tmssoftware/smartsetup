unit UProjectDefinition;
{$i ../../tmssetup.inc}

interface
uses Generics.Collections, SysUtils, UCoreTypes, Deget.CoreTypes;

type
  TApplicationDefinition = class
  private
    FName: string;
    FId: string;
    FDocs: string;
    FDescription: string;
    FUrl: string;
    FCopyright: string;
    FVersion: string;
    FCanAddSourceCodeToLibraryPath: boolean;
    function GetName: string;
  public
    constructor Create;
    property Id: string read FId write FId;
    property Name: string read GetName write FName;
    property Description: string read FDescription write FDescription;
    property Copyright: string read FCopyright write FCopyright;
    property Url: string read FUrl write FUrl;
    property Docs: string read FDocs write FDocs;
    property Version: string read FVersion write FVersion;
    property CanAddSourceCodeToLibraryPath: boolean read FCanAddSourceCodeToLibraryPath write FCanAddSourceCodeToLibraryPath;

    function NameAndVersion: string;

    function Clone: TApplicationDefinition;


  end;

  TPackage = class
  private
    FName: string;
    FFrameworks:  TFrameworkSet;
    FIsRuntime: boolean;
    FIsDesign: boolean;
    FPackageType: TPackageType;
  public
    Constructor Create(const aName: string);
    property Name: string read FName write FName;
    property IsRuntime: boolean read FIsRuntime write FIsRuntime;
    property IsDesign: boolean read FIsDesign write FIsDesign;
    property PackageType: TPackageType read FPackageType write FPackageType;

    property Frameworks: TFrameworkSet read FFrameworks write FFrameworks;
  end;

  TDependency = class
  private
    FId: string;
    FDescription: string;
  public
    property Id: string read FId write FId;
    property Description: string read FDescription write FDescription;
    constructor Create(const aId, aDescription: string);
  end;

  TResolvedState = (Unresolved, Resolving, Resolved);

  TListOfPackages = class (TObjectList<TPackage>)
  end;

  TFrameworkDefinition = class
  private
    FId: TFramework;
    FName: string;
    FIdeSince: TIDEName;
    FIdeUntil: TIDEName;
    FPlatforms: TPlatformSet;
    FSuportsCppBuilder: boolean;
    FDependencies: TObjectList<TDependency>;

  public
    property Id: TFramework read FId;
    property Name: string read FName;
    property IdeSince: TIDEName read FIdeSince write FIdeSince;
    property IdeUntil: TIDEName read FIdeUntil write FIdeUntil;

    property Platforms: TPlatformSet read FPlatforms write FPlatforms;
    property Dependencies: TObjectList<TDependency> read FDependencies;

    property SupportsCppBuilder: boolean read FSuportsCppBuilder write FSuportsCppBuilder;

    constructor Create(const aId: TFramework; const aName: string);
    destructor Destroy; override;
  end;

  TPlatformPaths = record
  public
    Platforms: TPlatformSet;
    Path: string;
    constructor Create(const APlatforms: TPlatformSet; const APath: string);
  end;

  TCompilerPaths = class
  private
    FLibraryPaths: TList<string>;
    FBrowsingPaths: TList<string>;
    FDebugDCUPaths: TList<string>;
    FWebCorePaths: TList<string>;
  public
    property LibraryPaths: TList<string> read FLibraryPaths;
    property BrowsingPaths: TList<string> read FBrowsingPaths;
    property DebugDCUPaths: TList<string> read FDebugDCUPaths;
    property WebCorePaths: TList<string> read FWebCorePaths;

    constructor Create;
    destructor Destroy; override;
  end;

  TCompilerPathsPerPlatform = class
  private
    FLibraryPaths: TList<TPlatformPaths>;
    FBrowsingPaths: TList<TPlatformPaths>;
    FDebugDCUPaths: TList<TPlatformPaths>;
    FWebCorePaths: TList<TPlatformPaths>;

    function GetPaths(const Platform: TPlatform; const Paths: TList<TPlatformPaths>): string;
  public
    property LibraryPaths: TList<TPlatformPaths> read FLibraryPaths;
    property BrowsingPaths: TList<TPlatformPaths> read FBrowsingPaths;
    property DebugDCUPaths: TList<TPlatformPaths> read FDebugDCUPaths;
    property WebCorePaths: TList<TPlatformPaths> read FWebCorePaths;

    constructor Create;
    destructor Destroy; override;

    function GetLibraryPaths(const Platform: TPlatform): string;
    function GetBrowsingPaths(const Platform: TPlatform): string;
    function GetDebugDCUPaths(const Platform: TPlatform): string;
    function GetWebCorePaths(const Platform: TPlatform): string;
  end;

  TShortcutType =(filelink) ;

  TShortcutDefinition = class
  private
    FShortcutType: TShortcutType;
    FName: string;
    FTarget: string;
    FDescription: string;
    FWorkingFolder: string;
  public
    property ShortcutType: TShortcutType read FShortcutType write FShortcutType;
    property Name: string read FName write FName;
    property Target: string read FTarget write FTarget;
    property Description: string read FDescription write FDescription;
    property WorkingFolder: string read FWorkingFolder write FWorkingFolder;

    constructor Create(const aShortcutType: TShortcutType; const aName, aTarget, aDescription, aWorkingFolder: string);
  end;

  TFileLinkDefinition = class
  private
    FFileToLink: string;
    FLinkToFolder: string;
    FOS: TOperatingSystemSet;
  public
    property FileToLink: string read FFileToLink write FFileToLink;
    property LinkToFolder: string read FLinkToFolder write FLinkToFolder;
    property OS: TOperatingSystemSet read FOS write FOS;

    constructor Create; overload;
    constructor Create(const AFileToLink, ALinkToFolder: string; const AOS: TOperatingSystemSet); overload;
  end;

  TPlatformSetArray = Array[TIDEName] of TPlatformSet;

  TPlatsCompiled = record
  private
    FValue : TPlatformSetArray;
    function GetValue(Index: TIDEName): TPlatformSet;
    procedure SetValue(Index: TIDEName; const AValue: TPlatformSet);
  public
    property Value[Index: TIDEName]: TPlatformSet read GetValue write SetValue; default;
    class function Create: TPlatsCompiled; static;
    class function All: TPlatsCompiled; static;
    procedure Clear;
    procedure Add(const Ide: TIDEName; const Plat: TPlatform);
    procedure AddCompiled(const Items: TPlatsCompiled);
  end;

  TRegistryEntryType = (&String, DWord);

  TRegistryEntry = class
  public
    KeyName: string;
    ValueName: string;
    ValueData: string;
    ValueType: TRegistryEntryType;
  end;

  TPackageFolders =  Array[TIDEName] of string;

  TProjectDefinition = class
  private
    FFullPath: string;
    FNaming: string;
    FApplication: TApplicationDefinition;
    FNotSupportedIDEs: TIDENameSet;
    FPackages: TListOfPackages;
    FDefines: TDictionary<string, boolean>;
    FRegistryEntries: TObjectList<TRegistryEntry>;
    FHelpFile: string;
    FDependencies: TObjectList<TDependency>;
    FWeakDependencies: TObjectList<TDependency>;
    FResolvedState: TResolvedState;
    FDefinesFilename: string;
    FNeedsCompiling: TPlatsCompiled;
    FIncludeInBuild: boolean;
    FRegisteredFrameworks: TObjectDictionary<string, TFrameworkDefinition>;
    FRegisteredFrameworkNames: Array[TFramework] of string;
    FExtraPaths: TCompilerPaths;
    FSearchPathsToPreserve: TArray<string>;
    FShortcuts: TObjectList<TShortcutDefinition>;
    FFileLinks: TObjectList<TFileLinkDefinition>;
    FOtherRegistryKeys: TList<string>;
    FPackageFolders: TPackageFolders;
  public
    constructor Create(const aFullPath: string);
    destructor Destroy; override;
    property FullPath: string read FFullPath;

    property Application: TApplicationDefinition read FApplication;
    property NotSupportedIDEs: TIDENameSet read FNotSupportedIDEs write FNotSupportedIDEs;
    property Packages: TListOfPackages read FPackages;
    property DefinesFilename: string read FDefinesFilename write FDefinesFilename;
    property Defines: TDictionary<string, boolean> read FDefines;
    property RegistryEntries: TObjectList<TRegistryEntry> read FRegistryEntries;
    property HelpFile: string read FHelpFile write FHelpFile;
    property Dependencies: TObjectList<TDependency> read FDependencies;
    property WeakDependencies: TObjectList<TDependency> read FWeakDependencies;
    property ResolvedState: TResolvedState read FResolvedState write FResolvedState;

    property Naming: string read FNaming write FNaming;
    property PackageFolders: TPackageFolders read FPackageFolders;

    function SupportsIDE(const dv: TIDEName): boolean;
    function SupportsFramework(const dv: TIDEName; const fr: TFramework): boolean;
    function SupportsPlatform(const plat: TPlatform): boolean;
    function SupportsCpp: boolean;
    function FrameworkSupportsPlatform(const dv: TIDEName; const plat: TPlatform; const fr: TFramework): boolean;
    function FrameworkHasAllWeakDependencies(const ProjectList: THashSet<string>; const dv: TIDEName;
      const plat: TPlatform; const fr: TFramework): string;

    function RootFolder: string;
    function ListDefines: string;

    property NeedsCompiling: TPlatsCompiled read FNeedsCompiling write FNeedsCompiling;
    property IncludeInBuild: boolean read FIncludeInBuild write FIncludeInBuild;

    procedure RegisterFramework(const Name: string; const ErrorInfo: string);
    function GetFramework(const Name: string): TFrameworkDefinition;
    function GetFrameworkName(const Id: TFramework): string;
    function GetAllFrameworks: TFrameworkSet;

    property ExtraPaths: TCompilerPaths read FExtraPaths;
    property SearchPathsToPreserve: TArray<string> read FSearchPathsToPreserve;

    property Shortcuts: TObjectList<TShortcutDefinition> read FShortcuts;
    property FileLinks: TObjectList<TFileLinkDefinition> read FFileLinks;
    property OtherRegistryKeys: TList<string> read FOtherRegistryKeys;

    procedure Validate;

    function IsExe: boolean;
    function FileNameExtension: TArray<string>;

    procedure AddSearchPathToPreserve(const Pattern: string);
    procedure SetPackageFolders(const dv: TIDEName; const Folder: string);

  end;

  TProjectDefinitionList = TList<TProjectDefinition>;


implementation
uses IOUtils, Classes;

{ TProjectDefinition }

function TProjectDefinition.IsExe: boolean;
begin
  // We allow a single package for exes;
  Result := (Packages.Count = 1) and  (Packages[0].PackageType = TPackageType.Exe);

end;

function TProjectDefinition.FileNameExtension: TArray<string>;
begin
  if IsExe then begin Result := ['.dproj', '.dpr', '.cbproj']; exit; end;
  Result := ['.dproj', '.dpk', '.cbproj', BinprojExtension];
end;

procedure TProjectDefinition.AddSearchPathToPreserve(const Pattern: string);
begin
  SetLength(FSearchPathsToPreserve, Length(FSearchPathsToPreserve) + 1); //not efficient, but we won't have too much of those, and having a TList would be even worse.
  FSearchPathsToPreserve[Length(SearchPathsToPreserve) - 1] := Pattern;

end;

constructor TProjectDefinition.Create(const aFullPath: string);
begin
  FFullPath := aFullPath;
  FApplication := TApplicationDefinition.Create;
  FPackages := TListOfPackages.Create;
  FDependencies := TObjectList<TDependency>.Create;
  FWeakDependencies := TObjectList<TDependency>.Create;
  FDefines := TDictionary<string, boolean>.Create;
  FRegistryEntries := TObjectList<TRegistryEntry>.Create;
  FRegisteredFrameworks := TObjectDictionary<string, TFrameworkDefinition>.Create([doOwnsValues]);
  FExtraPaths := TCompilerPaths.Create;
  FShortcuts := TObjectList<TShortcutDefinition>.Create;
  FFileLinks := TObjectList<TFileLinkDefinition>.Create;
  FOtherRegistryKeys := TList<string>.Create;
end;


destructor TProjectDefinition.Destroy;
begin
  FApplication.Free;
  FPackages.Free;

  FDependencies.Free;
  FWeakDependencies.Free;
  FDefines.Free;
  FRegistryEntries.Free;
  FRegisteredFrameworks.Free;
  FExtraPaths.Free;
  FShortcuts.Free;
  FFileLinks.Free;
  FOtherRegistryKeys.Free;
  inherited;
end;

function TProjectDefinition.GetAllFrameworks: TFrameworkSet;
begin
  Result := [];
  for var i := Low(TFramework) to High(TFramework) do
  begin
    if FRegisteredFrameworkNames[i] <> '' then Result := Result + [i];

  end;
end;

function TProjectDefinition.GetFramework(const Name: string): TFrameworkDefinition;
begin
  Result := FRegisteredFrameworks[Name];
end;

function TProjectDefinition.GetFrameworkName(const Id: TFramework): string;
begin
  Result := FRegisteredFrameworkNames[Id];
end;

function TProjectDefinition.ListDefines: string;
begin
  var Defs := Defines.Keys.ToArray;
  TArray.Sort<string>(Defs);
  Result := String.Join(', ', Defs);
end;

procedure TProjectDefinition.RegisterFramework(const Name: string; const ErrorInfo: string);
begin
  if FRegisteredFrameworks.ContainsKey(Name) then raise Exception.Create('Framework "' + Name + '" was already defined. ' + ErrorInfo);
  if FRegisteredFrameworks.Count + 1 > High(TFramework) then raise Exception.Create('Too many frameworks defined. There is a maximum of ' + IntToStr(High(TFramework)) + '. ' + ErrorInfo);

  FRegisteredFrameworkNames[FRegisteredFrameworks.Count] := Name;
  FRegisteredFrameworks.Add(Name, TFrameworkDefinition.Create(FRegisteredFrameworks.Count, Name));
end;

function TProjectDefinition.RootFolder: string;
begin
  Result := TPath.GetDirectoryName(FullPath);
end;


function TProjectDefinition.SupportsIDE(const dv: TIDEName): boolean;
begin
  if dv in NotSupportedIDEs then exit(false);
  for var fr := Low(TFramework) to High(TFramework) do
  begin
    if SupportsFramework(dv, fr) then exit(true);
  end;
  Result := false;
end;

function TProjectDefinition.SupportsFramework(const dv: TIDEName;
  const fr: TFramework): boolean;
begin
    var FrameworkName := FRegisteredFrameworkNames[fr];
    if FrameworkName = '' then exit(false);

    var Framework: TFrameworkDefinition;
    if not FRegisteredFrameworks.TryGetValue(FrameworkName, Framework) then exit(false);
    if (dv < Framework.IdeSince) or (dv > Framework.IdeUntil) then exit(false);

    Result := true;
end;

function TProjectDefinition.SupportsPlatform(const plat: TPlatform): boolean;
begin
  for var Framework in FRegisteredFrameworks.Values do
  begin
    if (plat in Framework.FPlatforms) then exit(true);
  end;
  Result := false;

end;

procedure TProjectDefinition.SetPackageFolders(const dv: TIDEName;
  const Folder: string);
begin
  FPackageFolders[dv] := Folder;
end;

function TProjectDefinition.SupportsCpp: boolean;
begin
  for var Framework in FRegisteredFrameworks.Values do
  begin
    if (Framework.SupportsCppBuilder) then exit(true);
  end;
  Result := false;

end;

function TProjectDefinition.FrameworkSupportsPlatform(const dv: TIDEName; const plat: TPlatform; const fr: TFramework): boolean;
begin
  var FrameworkName := FRegisteredFrameworkNames[fr];
  if FrameworkName = '' then exit(false);

  var Framework: TFrameworkDefinition;
  if not FRegisteredFrameworks.TryGetValue(FrameworkName, Framework) then exit(false);

  if not (plat in Framework.FPlatforms) then exit(false);
  if (dv < Framework.IdeSince) or (dv > Framework.IdeUntil) then exit(false);

  Result := true;

end;


function TProjectDefinition.FrameworkHasAllWeakDependencies(const ProjectList: THashSet<string>; const dv: TIDEName; const plat: TPlatform; const fr: TFramework): string;
begin
  Result := '';

  var FrameworkName := FRegisteredFrameworkNames[fr];
  if FrameworkName = '' then exit;

  var Framework: TFrameworkDefinition;
  if not FRegisteredFrameworks.TryGetValue(FrameworkName, Framework) then exit;

  for var dep in Framework.Dependencies do
  begin
    if dep.Id.StartsWith('~') then
    begin
      if ProjectList.Contains(dep.Id.Substring(1)) then exit(dep.Id);
    end else
    begin
      if not ProjectList.Contains(dep.Id) then exit(dep.Id);
    end;

  end;

end;

procedure TProjectDefinition.Validate;
begin
  //Do everything that can help the user realize there are errors in the definition.

  if (FRegisteredFrameworks.Count = 0) then raise Exception.Create('Project "' + FullPath + '" doesn''t define any framework.' );

  for var Package in FPackages do
  begin
    if (Package.PackageType = TPackageType.Exe) and (FPackages.Count <> 1)
      then raise Exception.Create('Applications must have a single Package which is the Application to build. In Project "' + FullPath + '" there is more than one package and one is an exe.' );

    if (Package.Frameworks = [])
      then raise Exception.Create('The Package "' + Package.Name + '" in Project "' + FullPath + '" doesn''t use any framework.' );
  end;




end;

{ TPackage }
constructor TPackage.Create(const aName: string);
begin
  Name := aName;
end;

{ TDependency }

constructor TDependency.Create(const aId, aDescription: string);
begin
  FId := aId;
  FDescription := aDescription;
end;


{ TFrameworkDefinition }

constructor TFrameworkDefinition.Create(const aId: TFramework;
  const aName: string);
begin
  FId := aId;
  FName := aName;
  FIdeUntil := High(TIDEName);
  FDependencies := TObjectList<TDependency>.Create;
end;

destructor TFrameworkDefinition.Destroy;
begin
  FDependencies.Free;
  inherited;
end;

{ TCompilerPaths }

constructor TCompilerPaths.Create;
begin
  FLibraryPaths := TList<string>.Create;
  FBrowsingPaths := TList<string>.Create;
  FDebugDCUPaths := TList<string>.Create;
  FWebCorePaths := TList<string>.Create;
end;

destructor TCompilerPaths.Destroy;
begin
  FLibraryPaths.Free;
  FBrowsingPaths.Free;
  FDebugDCUPaths.Free;
  FWebCorePaths.Free;

  inherited;
end;

{ TCompilerPathsPerPlatform }

constructor TCompilerPathsPerPlatform.Create;
begin
  FLibraryPaths := TList<TPlatformPaths>.Create;
  FBrowsingPaths := TList<TPlatformPaths>.Create;
  FDebugDCUPaths := TList<TPlatformPaths>.Create;
  FWebCorePaths := TList<TPlatformPaths>.Create;
end;

destructor TCompilerPathsPerPlatform.Destroy;
begin
  FLibraryPaths.Free;
  FBrowsingPaths.Free;
  FDebugDCUPaths.Free;
  FWebCorePaths.Free;

  inherited;
end;

function TCompilerPathsPerPlatform.GetBrowsingPaths(
  const Platform: TPlatform): string;
begin
  Result := GetPaths(Platform, FBrowsingPaths);
end;

function TCompilerPathsPerPlatform.GetDebugDCUPaths(
  const Platform: TPlatform): string;
begin
  Result := GetPaths(Platform, FDebugDCUPaths);
end;

function TCompilerPathsPerPlatform.GetLibraryPaths(
  const Platform: TPlatform): string;
begin
  Result := GetPaths(Platform, FLibraryPaths);
end;

function TCompilerPathsPerPlatform.GetWebCorePaths(
  const Platform: TPlatform): string;
begin
  Result := GetPaths(Platform, FWebCorePaths);
end;

function TCompilerPathsPerPlatform.GetPaths(const Platform: TPlatform; const Paths: TList<TPlatformPaths>): string;
begin
  Result := '';
  for var pt in Paths do
  begin
    if not (Platform in pt.Platforms) then continue;
    if Result <> '' then Result := Result + ';';
    Result := Result + pt.Path;
  end;
end;


{ TApplicationDefinition }

function TApplicationDefinition.Clone: TApplicationDefinition;
begin
  Result := TApplicationDefinition.Create;
  Result.FName := FName;
  Result.FId := FId;
  Result.FDocs := FDocs;
  Result.FDescription := FDescription;
  Result.FUrl := FUrl;
  Result.FCopyright := FCopyright;
  Result.FVersion := FVersion;
  Result.FCanAddSourceCodeToLibraryPath := FCanAddSourceCodeToLibraryPath;
end;

constructor TApplicationDefinition.Create;
begin
  FCanAddSourceCodeToLibraryPath := true;
end;

function TApplicationDefinition.GetName: string;
begin
  Result := FName;
  if (Result = '') then Result := id;
  
end;

function TApplicationDefinition.NameAndVersion: string;
begin
  Result := Name;
  if Version <> '' then Result := Result + ' ' + Version;

end;

{ TShortcutDefinition }

constructor TShortcutDefinition.Create(const aShortcutType: TShortcutType;
  const aName, aTarget, aDescription, aWorkingFolder: string);
begin
  FShortcutType := aShortcutType;
  FName := aName;
  FTarget := aTarget;
  FDescription := aDescription;
  FWorkingFolder := aWorkingFolder;
end;

{ TPlatsCompiled }

procedure TPlatsCompiled.Add(const Ide: TIDEName; const Plat: TPlatform);
begin
  Include(FValue[Ide], Plat);
end;

procedure TPlatsCompiled.AddCompiled(const Items: TPlatsCompiled);
begin
  for var ide := Low(TIDEName) to High(TIDEName) do FValue[ide] := FValue[ide] + Items.FValue[ide];
end;

procedure TPlatsCompiled.Clear;
begin
  for var ide := Low(TIDEName) to High(TIDEName) do FValue[ide] := [];
end;

class function TPlatsCompiled.Create: TPlatsCompiled;
begin
  for var ide := Low(TIDEName) to High(TIDEName) do Result[ide] := [];
end;

class function TPlatsCompiled.All: TPlatsCompiled;
begin
  var AllPlats: TPlatformSet := [];
  for var p := Low(TPlatform) to High(TPlatform) do AllPlats := AllPlats + [p];

  for var ide := Low(TIDEName) to High(TIDEName) do Result[ide] := AllPlats;
end;

function TPlatsCompiled.GetValue(Index: TIDEName): TPlatformSet;
begin
  Result := FValue[index];
end;

procedure TPlatsCompiled.SetValue(Index: TIDEName; const AValue: TPlatformSet);
begin
  FValue[index] := AValue;
end;

{ TFileLinkDefinition }

constructor TFileLinkDefinition.Create(const AFileToLink,
  ALinkToFolder: string; const AOS: TOperatingSystemSet);
begin
  FFileToLink := AFileToLink;
  FLinkToFolder := ALinkToFolder;
  FOS := AOS;
end;

constructor TFileLinkDefinition.Create;
begin

end;

{ TPlatformPaths }

constructor TPlatformPaths.Create(const APlatforms: TPlatformSet;
  const APath: string);
begin
  Platforms := APlatforms;
  Path := APath;
end;

end.

unit UProjectBuildInfo;
{$i ../../tmssetup.inc}

interface
uses Generics.Defaults, Generics.Collections, Deget.CoreTypes, UConfigDefinition, UProjectDefinition,
     UIDEBuildInfo, UPlatformBuildInfo, UPackageBuildInfo, UFileHasher,
     UUninstallInfo, USkippedPlatforms, SysUtils, ULogger;

type
  TPlatformsToUninstall = class
  private
    Plats: Array[TIDEName] of TList<IUninstallInfo>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const Ide: TIDEName; const Data: IUninstallInfo);
    function HasAny: boolean;

    function Get(const Ide: TIDEName): TList<IUninstallInfo>;
  end;

  TNoteType = (SkippedIDE, SkippedPlatform, MissingSDK, SkippedProduct);
  TNoteTypeSet = set of TNoteType;
  TBuildNotes = class
  type
    TBuildNotesData = record
    public
      Data: string;
      IDEName: TIDEName;
      NoteType: TNoteType;
      constructor Create(const aData: string; const aIDEName: TIDEName; const aNoteType: TNoteType);
    end;
  private
    FData: TList<TBuildNotesData>;
    FHasNoteType: TNoteTypeSet;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const s: string; const aIDEName: TIDEName; const aNoteType: TNoteType);
    procedure PrepareLog(const Duplicates: TDictionary<string, boolean>; const ConsolidatedData: TList<TBuildNotesData>; const NoteTypes: TNoteTypeSet);
    class procedure Log(const Logger: TProc<string>; const ConsolidatedData: TList<TBuildNotesData>);
    function HasSkipped: boolean;
    function HasSDK: boolean;
  end;

  TProjectBuildInfo = class
  private
    FConfig: TConfigDefinition;
    FSourceCodeHash: string;
    FIDEsBuildInfo: TObjectList<TIDEBuildInfo>;
    FIDEsToUninstall: TList<IUninstallInfo>;
    FProject: TProjectDefinition;
    FNotes: TBuildNotes;
    FSkipped: TSkippedPlatforms;
    FDependencies: TList<TProjectBuildInfo>;

    FDryRun: boolean;
    FDebugDCUs: boolean;
    FBuildEvents: boolean;
    FAddSourceCodeToLibraryPath: boolean;
    FDefines: TArray<string>;
    FCompileTempFolder: string;
    FBplFolder: string;
    FPreBuildFailed: boolean;
    FPostBuildFailed: boolean;
    FExtraPaths: TCompilerPathsPerPlatform;
    FAlternateRegistryKey: string;
    FPlatformsToUninstall: TPlatformsToUninstall;
    FShortcuts: TObjectList<TShortcutDefinition>;
    FFileLinks: TObjectList<TFileLinkDefinition>;
    FBasePackagesFolder: string;
    FProgress: TProductProgressInfo;
    FNeedsBuild: boolean;

    function EnsureIDE(const IDEName: TIDEName): TIDEBuildInfo;
    function EnsurePlatform(const IDEName: TIDEName; const dp: TPlatform): TPlatformBuildInfo;
    function GetProjectId: string;
    function GetCompilerPaths: TCompilerPathsPerPlatform;
    function GetShortcuts: TObjectList<TShortcutDefinition>;
    function GetFileLinks: TObjectList<TFileLinkDefinition>;

  public
    constructor Create(const aConfig: TConfigDefinition; const aProject: TProjectDefinition);
    destructor Destroy; override;

    property ProjectId: string read GetProjectId;

    property Project: TProjectDefinition read FProject;
    property IDEsBuildInfo: TObjectList<TIDEBuildInfo> read FIDEsBuildInfo;
    property IDEsToUninstall: TList<IUninstallInfo> read FIDEsToUninstall;
    property PlatformsToUninstall: TPlatformsToUninstall read FPlatformsToUninstall;
    property Notes: TBuildNotes read FNotes;
    property SourceCodeHash: string read FSourceCodeHash write FSourceCodeHash;

    property Skipped: TSkippedPlatforms read FSkipped write FSkipped;
    procedure AddBuildInfo(const dv: TIDEName; const BuildPackage: TPackageBuildInfo);

    function ContainsIDE(const ide: TIDEName; out IDEBI: TIDEBuildInfo; out HasSkipped: boolean): boolean;
    function ContainsIDEAndPlatform(const ide: TIDEName; const Platform: TPlatform): boolean;

    function AllPackages(const CountDebug: boolean = false): integer;
    function AllOk: boolean;
    function AllFailed: boolean;
    property PreBuildFailed: boolean read FPreBuildFailed write FPreBuildFailed;
    property PostBuildFailed: boolean read FPostBuildFailed write FPostBuildFailed;

    function HasSomethingToUninstall: boolean;
    function SkipRegistering: TSkipRegistering;

    property Dependencies: TList<TProjectBuildInfo> read FDependencies;
    property DryRun: boolean read FDryRun;
    property DebugDCUs: boolean read FDebugDCUs;
    property BuildEvents: boolean read FBuildEvents;
    property AddSourceCodeToLibraryPath: boolean read FAddSourceCodeToLibraryPath;
    property Defines: TArray<string> read FDefines;

    property CompileTempFolder: string read FCompileTempFolder;
    property BplFolder: string read FBplFolder;
    property AlternateRegistryKey: string read FAlternateRegistryKey;
    property ExtraPaths: TCompilerPathsPerPlatform read FExtraPaths;
    property Shortcuts: TObjectList<TShortcutDefinition> read FShortcuts;
    property FileLinks: TObjectList<TFileLinkDefinition> read FFileLinks;

    property Config: TConfigDefinition read FConfig;

    property BasePackagesFolder: string read FBasePackagesFolder write FBasePackagesFolder;
    property Progress: TProductProgressInfo read FProgress;
    function PackagesFolder(const IDEName: TIDEName): string;

    class function GetPlatformPaths(const RootFolder, ExtraPath:string): TPlatformPaths;

    property NeedsBuild: boolean read FNeedsBuild write FNeedsBuild;

  end;
implementation
uses UConfigKeys, UTmsBuildSystemUtils, IOUtils;

{ TProjectBuildInfo }

constructor TProjectBuildInfo.Create(const aConfig: TConfigDefinition; const aProject: TProjectDefinition);
begin
  FProject := aProject;
  FIDEsBuildInfo := TObjectList<TIDEBuildInfo>.Create;
  FIDEsToUninstall := TList<IUninstallInfo>.Create;
  FNotes := TBuildNotes.Create;
  FDependencies := TList<TProjectBuildInfo>.Create;

  FDryRun := aConfig.DryRun(ProjectId);
  FDebugDCUs := aConfig.ReadBoolProperty(ProjectId, ConfigKeys.DebugDcus, true);
  if aProject.IsExe and not aProject.ExeOptions.ExeDebug then FDebugDCUs := false;
  FBuildEvents := aConfig.ReadBoolProperty(ProjectId, ConfigKeys.BuildEvents, false);

  if aProject.Application.CanAddSourceCodeToLibraryPath then
  begin
    FAddSourceCodeToLibraryPath := aConfig.ReadBoolProperty(ProjectId, ConfigKeys.AddSourceCodeToLibraryPath, false);
  end;
  FDefines := aConfig.GetAllDefines(ProjectId);
  FCompileTempFolder := aConfig.Folders.CompileTempFolder;
  FBplFolder := aConfig.Folders.BplFolder;
  FAlternateRegistryKey := aConfig.AlternateRegistryKey;
  FExtraPaths := GetCompilerPaths;
  FSkipped := TSkippedPlatforms.Create;
  FPlatformsToUninstall := TPlatformsToUninstall.Create;
  FShortcuts := GetShortcuts;
  FFileLinks := GetFileLinks;
  FConfig := aConfig;
  FProgress := TProductProgressInfo.Create(aProject.Application.Id);
end;

destructor TProjectBuildInfo.Destroy;
begin
  FIDEsBuildInfo.Free;
  FIDEsToUninstall.Free;
  FNotes.Free;
  FDependencies.Free;
  FExtraPaths.Free;
  FSkipped.Free;
  FPlatformsToUninstall.Free;
  FShortcuts.Free;
  FFileLinks.Free;
  inherited;
end;



function TProjectBuildInfo.ContainsIDE(const ide: TIDEName;
  out IDEBI: TIDEBuildInfo; out HasSkipped: boolean): boolean;
begin
  HasSkipped := Skipped.ContainsIDE(ide);

  for var IDEInfo in IDEsBuildInfo do
  begin
    if IDEInfo.Name = ide then
    begin
      IDEBI := IDEInfo;
      exit(true);
    end;
  end;

  IDEBI := nil;
  Result := false;
end;

function TProjectBuildInfo.ContainsIDEAndPlatform(const ide: TIDEName; const Platform: TPlatform): boolean;
begin
  Result := false;
  //There are 2 possibilities here:
  // 1. The dep was skipped. Then, Skipped will have a value for all the platforms the dep supports.
  // 2. The dep was not skipped. Then, IDEsBuildInfo will have the info on what that dep supports.
  if Skipped.ContainsPlatform(ide, Platform) then exit(true);
  for var IDEInfo in IDEsBuildInfo do
  begin
    if IDEInfo.Name = ide then
    begin
      for var PlatInfo in IDEInfo.PlatformsBuildInfo do
      begin
        if PlatInfo.Name = Platform then exit(true);
      end;
      exit(false);
    end;
  end;
end;


procedure TProjectBuildInfo.AddBuildInfo(const dv: TIDEName;
  const BuildPackage: TPackageBuildInfo);
begin
  var BuildPlatform := EnsurePlatform(dv, BuildPackage.Platform);
  BuildPlatform.PackagesBuildInfo.Add(BuildPackage);
end;

function TProjectBuildInfo.AllOk: boolean;
begin
  for var ide in IDEsBuildInfo do if not ide.AllOk then exit(false);
  if PostBuildFailed then exit(false);
  Result := true;
end;

function TProjectBuildInfo.AllFailed: boolean;
begin
  for var ide in IDEsBuildInfo do if not ide.AllFailed then exit(false);

  Result := true;
end;

function TProjectBuildInfo.AllPackages(const CountDebug: boolean = false): integer;
begin
  Result := 0;
  for var ide in IDEsBuildInfo do Result := Result + ide.AllPackages;
  if CountDebug and DebugDCUs then Result := Result * 2;

end;

function TProjectBuildInfo.EnsureIDE(const IDEName: TIDEName): TIDEBuildInfo;
begin
  for var ide in IDEsBuildInfo do if ide.Name = IDEName then exit(ide);
  Result := TIDEBuildInfo.Create(IDEName, Project, Config);
  IDEsBuildInfo.Add(Result);
end;

function TProjectBuildInfo.EnsurePlatform(const IDEName: TIDEName;
  const dp: TPlatform): TPlatformBuildInfo;
begin
  var ide := EnsureIDE(IDEName);
  for var plat in ide.PlatformsBuildInfo do if plat.Name = dp then exit(plat);
  Result := TPlatformBuildInfo.Create(dp);
  Ide.PlatformsBuildInfo.Add(Result);
end;

class function TProjectBuildInfo.GetPlatformPaths(const RootFolder, ExtraPath:string) : TPlatformPaths;
begin
  var ep := ExtraPath.Trim;
  if not ep.StartsWith('@') then exit(TPlatformPaths.Create([Low(TPlatform)..High(TPlatform)], CombinePath(RootFolder, ep)));
  var epWithoutAt := ep.SubString(1);
  var EndPlat := epWithoutAt.IndexOf(':');
  if EndPlat < 1 then raise Exception.Create('Extra Path "' + ep + '" doesn''t have a colon to end the platform section.');
  var FinalPath := epWithoutAt.Substring(EndPlat + 1).Trim();
  var PlatformArray := epWithoutAt.Substring(0, EndPlat).Split([','], TStringSplitOptions.ExcludeEmpty);

  var SupportedPlatforms: TPlatformSet := [];
  for var Platform in PlatformArray do
  begin
    var PlatformTrim := Platform.Trim;
    var P := GetPlatformName(PlatformTrim);
    var PInt := Integer(P);
    if (PInt < 0) or (PInt > Integer(High(TPlatform))) then raise Exception.Create('The platform "' + PlatformTrim + '" in the Extra Path "' + ep + '" is not defined.');
    Include(SupportedPlatforms, P);
  end;


  if SupportedPlatforms = [] then raise Exception.Create('The Extra Path "' + ep + '" doesn''t define a platform. Should it not start with "@"?.');

  exit(TPlatformPaths.Create(SupportedPlatforms, CombinePath(RootFolder, FinalPath)));

end;

function TProjectBuildInfo.GetCompilerPaths: TCompilerPathsPerPlatform;
begin
  Result := TCompilerPathsPerPlatform.Create;
  try
    for var ex in Project.ExtraPaths.LibraryPathsBuildAndRegister do
    begin
      Result.LibraryPathsBuildAndRegister.Add(GetPlatformPaths(Project.RootFolder, ex));
    end;
    for var ex in Project.ExtraPaths.LibraryPathsBuildOnly do
    begin
      Result.LibraryPathsBuildOnly.Add(GetPlatformPaths(Project.RootFolder, ex));
    end;
    for var ex in Project.ExtraPaths.BrowsingPaths do
    begin
      Result.BrowsingPaths.Add(GetPlatformPaths(Project.RootFolder, ex));
    end;
    for var ex in Project.ExtraPaths.DebugDCUPaths do
    begin
      Result.DebugDCUPaths.Add(GetPlatformPaths(Project.RootFolder, ex));
    end;
    for var ex in Project.ExtraPaths.WebCorePaths do
    begin
      Result.WebCorePaths.Add(GetPlatformPaths(Project.RootFolder, ex));
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TProjectBuildInfo.GetShortcuts: TObjectList<TShortcutDefinition>;
begin
  Result := TObjectList<TShortcutDefinition>.Create;
  try
    for var shortcut in Project.Shortcuts do
    begin
      var WorkingFolder := '';
      if shortcut.WorkingFolder <> '' then WorkingFolder := CombinePath(Project.RootFolder, shortcut.WorkingFolder);

      Result.Add(TShortcutDefinition.Create(
          shortcut.ShortcutType,
          shortcut.Name,
          CombinePath(Project.RootFolder, shortcut.Target),
          shortcut.Description,
          WorkingFolder));
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TProjectBuildInfo.GetFileLinks: TObjectList<TFileLinkDefinition>;
begin
  Result := TObjectList<TFileLinkDefinition>.Create;
  try
    for var filelink in Project.FileLinks do
    begin
      var LinkToFolder := '';
      if filelink.LinkToFolder <> '' then LinkToFolder := CombinePath(Project.RootFolder, filelink.LinkToFolder);

      Result.Add(TFileLinkDefinition.Create(
          CombinePath(Project.RootFolder, filelink.FileToLink),
          LinkToFolder, filelink.OS));
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TProjectBuildInfo.GetProjectId: string;
begin
  Result := Project.Application.Id;
end;

function TProjectBuildInfo.HasSomethingToUninstall: boolean;
begin
  Result := (FIDEsToUninstall.Count > 0) or FPlatformsToUninstall.HasAny;
end;

function TProjectBuildInfo.PackagesFolder(const IDEName: TIDEName): string;
begin
  var Naming := Config.GetNaming(Project.Naming, Project.FullPath);
  var DelphiFolder := Naming.GetPackageNaming(IdeName, Project.IsExe, Project.PackageFolders[IdeName]);

  Result := TPath.Combine(BasePackagesFolder, DelphiFolder);
end;

function TProjectBuildInfo.SkipRegistering: TSkipRegistering;
begin
  if Config.Unregistering then exit(TSkipRegistering.All);
  
  var Settings := TSkipRegisteringSet(Byte((Config.SkipRegistering(ProjectId, 0))));
  if Project.IsExe then Settings := Settings + [TSkipRegisteringOptions.Packages];

  var SettingsExt := Config.SkipRegisteringExt(ProjectId, TSkipRegisteringOptionsExt_False);

  Result := TSkipRegistering.Create(Settings, SettingsExt);
end;

{ TPlatformsToUninstall }

procedure TPlatformsToUninstall.Add(const Ide: TIDEName;
  const Data: IUninstallInfo);
begin
if (Ide < Low(TIDEName)) or (Ide > High(TIDEName)) then raise Exception.Create('Internal Error: Ide outside bounds.');
Plats[Ide].Add(Data);

end;

constructor TPlatformsToUninstall.Create;
begin
for var ide:= Low(TIDEName) to High(TIDEName) do Plats[ide] := TList<IUninstallInfo>.Create;
end;

destructor TPlatformsToUninstall.Destroy;
begin
  for var ide:= Low(TIDEName) to High(TIDEName) do Plats[ide].Free;
  inherited;
end;

function TPlatformsToUninstall.Get(const Ide: TIDEName): TList<IUninstallInfo>;
begin
  if (Ide < Low(TIDEName)) or (Ide > High(TIDEName)) then raise Exception.Create('Internal Error: Ide outside bounds.');
  Result := Plats[Ide];

end;

function TPlatformsToUninstall.HasAny: boolean;
begin
  for var ide:= Low(TIDEName) to High(TIDEName) do if (Plats[ide].Count > 0) then exit(true);
  Result := false;
end;

{ TBuildNotes }

procedure TBuildNotes.Add(const s: string; const aIDEName: TIDEName; const aNoteType: TNoteType);
begin
  FData.Add(TBuildNotesData.Create(s, aIDEName, aNoteType));
  Include(FHasNoteType, aNoteType);
end;

constructor TBuildNotes.Create;
begin
  FData := TList<TBuildNotesData>.Create;
end;

destructor TBuildNotes.Destroy;
begin
  FData.Free;
  inherited;
end;

function TBuildNotes.HasSDK: boolean;
begin
  Result := TNoteType.MissingSDK in FHasNoteType;
end;

function TBuildNotes.HasSkipped: boolean;
begin
  Result := FData.Count > 0;
end;

class procedure TBuildNotes.Log(const Logger: TProc<string>; const ConsolidatedData: TList<TBuildNotesData>);
begin
  ConsolidatedData.Sort(TComparer<TBuildNotesData>.Construct(
    function(const Left, Right: TBuildNotesData): Integer
    begin
      if Left.IDEName < Right.IDEName then exit(-1);
      if Left.IDEName > Right.IDEName then exit(1);
      if Left.NoteType < Right.NoteType then exit(-1);
      if Left.NoteType > Right.NoteType then exit(1);

      Result := String.Compare(Left.Data, Right.Data);
      if Result <> 0 then exit;
      Result := 0;
    end
  ));

  for var Note in ConsolidatedData do
  begin
    Logger(Note.Data);
  end;

end;

procedure TBuildNotes.PrepareLog(const Duplicates: TDictionary<string, boolean>; const ConsolidatedData: TList<TBuildNotesData>; const NoteTypes: TNoteTypeSet);
begin
  for var Note in FData do
  begin
    if not (Note.NoteType in NoteTypes) then continue;

    if not Duplicates.ContainsKey(Note.Data) then
    begin
      Duplicates.Add(Note.Data, true);
      ConsolidatedData.Add(Note);
    end;
  end;

end;

{ TBuildNotes.TBuildNotesData }

constructor TBuildNotes.TBuildNotesData.Create(const aData: string; const aIDEName: TIDEName; const aNoteType: TNoteType);
begin
  Data := aData;
  IDEName := aIDEName;
  NoteType := aNoteType;
end;

end.

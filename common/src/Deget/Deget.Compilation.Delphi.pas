unit Deget.Compilation.Delphi;

interface

uses
  Deget.CoreTypes,
  Deget.Compilation,
  UProjectDefinition,
  UConfigDefinition,
  Deget.IDEInfo,
  Deget.PackageConfig,
  Deget.IDETypes,
  Deget.Nullable;
type
  TDelphiCompilationSettings = class(TCompilationSettings)
  private
    FTargetPlatform: IDelphiPlatformInfo;
    function GetPathsToPreserve(const Pattern: string): string;
    function GetLibraryPath: string;
  public
    property TargetPlatform: IDelphiPlatformInfo read FTargetPlatform write FTargetPlatform;
    function AddPreservedSearchPaths(const ExistingPath: string): string;

  end;


  TBdsCompilationSettings = class(TDelphiCompilationSettings)
  strict private
    FRegistryName: string;
    FBplFolder: string;
  public
    property RegistryName: string read FRegistryName write FRegistryName;
    property BplFolder: string Read FBplFolder write FBplFolder;
  end;

  TMsBuildCompilationSettings = class(TDelphiCompilationSettings)
  strict private
    FRegistryName: string;
    FPackageLibraryPath: string;
    FIgnoreDefines: boolean;
  public
    property RegistryName: string read FRegistryName write FRegistryName;
    property PackageLibraryPath: string read FPackageLibraryPath write FPackageLibraryPath;
    property IgnoreDefines: boolean read FIgnoreDefines write FIgnoreDefines;
  end;

  TDCC32CompilationSettings = class(TDelphiCompilationSettings)
  private
    FCBuilderOutput: Nullable<boolean>;
    FBplOutputDir: Nullable<string>;
    FDcpOutputDir: Nullable<string>;
    FDcuOutputDir: Nullable<string>;
    FExeOutputDir: Nullable<string>;
    FHppOutputDir: Nullable<string>;
    FObjOutputDir: Nullable<string>;
    FOriginalDpkPath: string;
    FIsDpr: boolean;
  public
    // IMPORTANT NOTE: The properties below are not being considered for the msbuild compiler
    // They are only being used for dcc32 compiler. This is just because for now we are using
    // the package settings (when using msbuild) and thus we didn't take the time to
    property CBuilderOutput: Nullable<boolean> read FCBuilderOutput write FCBuilderOutput;
    property BplOutputDir: Nullable<string> read FBplOutputDir write FBplOutputDir;
    property DcpOutputDir: Nullable<string> read FDcpOutputDir write FDcpOutputDir;
    property DcuOutputDir: Nullable<string> read FDcuOutputDir write FDcuOutputDir;
    property ExeOutputDir: Nullable<string> read FExeOutputDir write FExeOutputDir;
    property HppOutputDir: Nullable<string> read FHppOutputDir write FHppOutputDir;
    property ObjOutputDir: Nullable<string> read FObjOutputDir write FObjOutputDir;
    property OriginalDpkPath: string read FOriginalDpkPath write FOriginalDpkPath;
    property IsDpr: boolean read FIsDpr write FIsDpr;
  end;

  //For Community Edition...
  TBdsCompiler = class
  private
    class var BDSLock: TObject;
  private
    FTempFolder: string;
    class constructor Create;
    class destructor Destroy;
    procedure ModifyConfig(const IDEName: TIDEName; const ProjectFile: string;
      const Settings: TBdsCompilationSettings);
    function GetEnvVariablesForCE(const Settings: TBdsCompilationSettings): TArray<string>;
  public
    function BuildBdsParameters(IDEName: TIDEName; Settings: TBdsCompilationSettings; const ErrFile: string): string;
    procedure DoCompile(const ProjectFile: string; IDEName: TIDEName; Settings: TBdsCompilationSettings);
    property TempFolder: string read FTempFolder write FTempFolder;
  public
    class procedure Build(const ProjectFile: string; IDEName: TIDEName; Settings: TBdsCompilationSettings; ATempFolder: string);
  end;

  TMSBuildCompiler = class
  strict private
    FTempFolder: string;
    function FindVariable(const VariableName: string; const Env: TArray<string>;
      out VariableValue: string; const Level: integer): boolean;
    function ExpandPath(const aPath: string; const Env: TArray<string>; const Level: integer): string;
    function FindMsBuildInPath(const Path: string; const Env: TArray<string>): string;
    function FindMsBuild(const Env: TArray<string>): string;
    function ParseRSVars(const RSVars: string): TArray<string>;
    function AddEnvironmentOverrides(const IDEInfo: IDelphiIDEInfo;
      const ExistingEnv: TArray<string>): TArray<string>;
    procedure DoCompileWithBat(const ProjectFile: string; IDEName: TIDEName; Settings: TMsBuildCompilationSettings);
    function DoCompileDirectly(const ProjectFile: string; IDEName: TIDEName; Settings: TMsBuildCompilationSettings): boolean;
  private
    function InEnvVarOverrides(const Env: string;
      const EnvVarOverrides: TArray<TEnvVar>): boolean;
    function GetEnvVarsForBat(const IDEInfo: IDelphiIDEInfo): string;
    function AddTMPEnv(const TmpVar: string; var ExistingEnv: TArray<string>;
      const TMPFolder: string): TArray<string>;
    function CppSystemIncludePath(const DelphiVersion: TIDEName;
      const DPlat: TPlatform; const ClassicCompiler: boolean): string;
    function CppExtraLinkPath(const DelphiVersion: TIDEName;
      const DPlat: TPlatform; const PlatformId: string; const ClassicCompiler: boolean): string;
    function AddCPPBuilderParameters(const ProjectFileName: string; IDEName: TIDEName;
      Settings: TMsBuildCompilationSettings; LocalSearchPath: string): string;

    function AdaptPathEntry(const FileName, Entry: string; const SkipEntries: TArray<string>): string;

    procedure ReadLibPathsFromBCProj(const ProjectFileName: string;
      const IDEName: TIDEName; out CppProjectIncludePath,
      CppProjectLinkPath: string; out ClassicCompiler: boolean);
    function ExpandMacros(const ExistingEnv: TArray<string>;
      const s: string): string;
    function GetVarName(const s: string): string;
    function GetVarValue(const s: string): string;
  public
    function BuildMsBuildParameters(const ProjectFileName: string; IDEName: TIDEName; Settings: TMsBuildCompilationSettings): string;
    procedure DoCompile(const ProjectFile: string; IDEName: TIDEName; Settings: TMsBuildCompilationSettings);
    property TempFolder: string read FTempFolder write FTempFolder;
  public
    class procedure Build(const ProjectFile: string; IDEName: TIDEName; Settings: TMsBuildCompilationSettings; ATempFolder: string = '');
  end;

  TDcc32Compiler = class
  strict private
    FTempFolder: string;
  public
    function BuildDcc32Parameters(IDEName: TIDEName; Settings: TDcc32CompilationSettings): string;
    procedure DoCompile(ProjectFile: string; IDEName: TIDEName; Settings: TDcc32CompilationSettings);
    property TempFolder: string read FTempFolder write FTempFolder;
  public
    class procedure Build(const ProjectFile: string; IDEName: TIDEName; Settings: TDcc32CompilationSettings; ATempFolder: string = '');
  end;

procedure SetupResinator(const ResinatorPath, TempPath: string);

implementation

uses
  SyncObjs,
  System.SysUtils, System.IOUtils,
  System.StrUtils, Generics.Collections,
  Deget.FileUtils,
  Deget.IDEUtils,
  Deget.DelphiInfo,
  Deget.CommandLine,
  UWindowsPath,
  UTmsBuildSystemUtils,
  Commands.GlobalConfig,
  Deget.Filer.DprojFile,
  Deget.Filer.CBprojFile,
  System.Masks,
  UOSFileLinks,
  UMultiLogger;

{ TDelphiCompilationSettings }

function TDelphiCompilationSettings.GetLibraryPath: string;
begin
  var IDEInfo := TargetPlatform.IDEInfo;
  var ThirdPartyPathsFile := TPath.Combine(IDEInfo.RootDir, 'third-party', 'third-party-paths.txt');
  if IDEInfo.HasCustomCompilerPaths and TFile.Exists(ThirdPartyPathsFile) then
  begin
    Result := '';
    var Paths := TFile.ReadAllLines(ThirdPartyPathsFile);
    for var Path in Paths do
    begin
      Result := Result + TPath.Combine(IDEInfo.RootDir, 'third-party', Path) +';';
    end;
  end
  else
  begin
    Result := TargetPlatform.GetIDEPath(TDelphiPathType.ptLibraryPath);
  end;
end;

function TDelphiCompilationSettings.GetPathsToPreserve(
  const Pattern: string): string;
begin
  var NewPaths := TList<string>.Create;
  try
    var ExistingLibraryPaths := GetLibraryPath;
    var SplitPaths := ExistingLibraryPaths.Split([';']);
    for var Path in SplitPaths do
    begin
      if MatchesMask(Path, Pattern) then
      begin
        //We need to remove the last \, or it will escape the " that goes after the path ends.
        NewPaths.Add(ExcludeTrailingPathDelimiter(Path));
      end;
    end;
    Result := String.Join(';', NewPaths.ToArray);
  finally
    NewPaths.Free;
  end;
end;

function TDelphiCompilationSettings.AddPreservedSearchPaths(
  const ExistingPath: string): string;
begin
  Result := ExistingPath;
  if SearchPathsToPreserve = nil then exit;
  for var Pattern in SearchPathsToPreserve do
  begin
    var PreservedPaths := GetPathsToPreserve(Pattern);
    if PreservedPaths <> '' then
    begin
      Result := Result + ';' + PreservedPaths;
    end;
  end;
end;


function Quote(const S: string): string;
begin
  Result := '"' + S + '"';
end;


{ TMSBuildCompiler }

class procedure TMSBuildCompiler.Build(const ProjectFile: string;
  IDEName: TIDEName; Settings: TMsBuildCompilationSettings; ATempFolder: string = '');
var
  Compiler: TMsBuildCompiler;
begin
  Compiler := TMsBuildCompiler.Create;
  try
    Compiler.TempFolder := ATempFolder;
    Compiler.DoCompile(ProjectFile, IDEName, Settings);
  finally
    Compiler.Free;
  end;
end;

function TMSBuildCompiler.CppSystemIncludePath(const DelphiVersion: TIDEName; const DPlat: TPlatform; const ClassicCompiler: boolean): string;
begin
  Result := ';';

  case DPlat of
    TPlatform.win32intel:
      if DelphiVersion >= TIDEName.Delphi12 then
      begin
        Result := Result + '$(BDSINCLUDE)\windows\vcl;';
        Result := Result + '$(BDSINCLUDE)\windows\fmx;';
        Result := Result + '$(BDSINCLUDE)\windows\rtl;';
        Result := Result + '$(BDSINCLUDE)\windows\crtl;';
        if (DelphiVersion < Delphi12) or ClassicCompiler then //Delphi will never be less than D12 here, but we keep the check because if we needed SystemIncludePath for older versions, it would be like this.
        begin
          Result := Result + '$(BDSINCLUDE)\dinkumware;';
        end
        else
        begin
          Result := Result + '$(BDSINCLUDE)\dinkumware64;';
        end;

        Result := Result + '$(BDSINCLUDE)\windows\sdk';
      end;
    TPlatform.win64intel:
    begin
      Result := Result + '$(BDSINCLUDE)\windows\vcl;';
      Result := Result + '$(BDSINCLUDE)\windows\fmx;';
      Result := Result + '$(BDSINCLUDE)\windows\rtl;';
      Result := Result + '$(BDSINCLUDE)\windows\crtl;';
      Result := Result + '$(BDSINCLUDE)\dinkumware64;';
      Result := Result + '$(BDSINCLUDE)\windows\sdk';
    end;
    TPlatform.win64xintel:
    begin
      Result := Result + '$(BDSINCLUDE);';
      Result := Result + '$(BDSINCLUDE)\x86_64-w64-mingw32\c++\v1;';
      if DelphiVersion <= delphi12 then
      begin
        Result := Result + '$(BDS)\lib\clang\15.0.7\include;';
      end
      else begin
        Result := Result + '$(BDS)\lib\clang\20\include;';
      end;
      Result := Result + '$(BDSINCLUDE)\x86_64-w64-mingw32;';
      Result := Result + '$(BDSINCLUDE)\windows\sdk;';
      Result := Result + '$(BDSINCLUDE)\windows\rtl;';
      Result := Result + '$(BDSINCLUDE)\windows\vcl;';
      Result := Result + '$(BDSINCLUDE)\windows\fmx;';
      Result := Result + '$(BDSCOMMONDIR)\hpp\Win64x;';
    end;
  end;

end;

function TMSBuildCompiler.CppExtraLinkPath(const DelphiVersion: TIDEName; const DPlat: TPlatform; const PlatformId: string; const ClassicCompiler: boolean): string;
var
  clng: string;
begin
  if (not ClassicCompiler) and (DPlat = TPlatform.win32intel) then clng := 'c' else clng :='';
  var Platform := PlatformId + clng;

  case DPlat of
    TPlatform.win32intel,
    TPlatform.win64intel,
    TPlatform.win64xintel:
      Result := ';$(BDS)\lib\' + Platform + '\release\;$(BDS)\lib\' + Platform + '\release\psdk;';

    else Result :='';
  end;
  if (DelphiVersion >= TIDEName.delphi12) and (DPlat = TPlatform.win64xintel) then
  begin
    Result := Result + '$(BDS)\x86_64-w64-mingw32\lib;';
    Result := Result + '$(BDSLIB)\clang\15.0.7\lib\windows;';
  end;

end;

function TMSBuildCompiler.AdaptPathEntry(const FileName, Entry: string; const SkipEntries: TArray<string>): string;
  function DoSkipEntry(const e: string): boolean;
  begin
    for var Skip in SkipEntries do
    begin
      if e = Skip then exit(true);
    end;
    Result := false;
  end;

begin
  var EntryTrim := Entry.Trim;
  if EntryTrim = '' then exit(Entry);

  Result := '';
  var Entries := EntryTrim.Split([';'], TStringSplitOptions.ExcludeEmpty);
  for var e in Entries do
  begin
    if DoSkipEntry(e.Trim) then continue;
    Result := Result + TPath.GetFullPath(TPath.Combine(TPath.GetDirectoryName(FileName), e) ) + ';';

  end;

end;



procedure TMSBuildCompiler.ReadLibPathsFromBCProj(const ProjectFileName: string; const IDEName: TIDEName; out CppProjectIncludePath, CppProjectLinkPath: string; out ClassicCompiler: boolean);
begin
{$IFDEF MSWINDOWS}
  // The include paths in the cbproj file must be passed to the command line, or it won't compile.
  // This is because when we specify a custom /p:IncludePath C++ builder ignores the paths in the project
  // and without them we can't compile. If we ever unify the metadata, we could read this just once.
  var PackData := TCBProjPackageReadData.Create;
  try
    var Reader := TCBprojReader.Create(ProjectFileName, IDEName);
    try
      Reader.ReadData(PackData);
      CppProjectIncludePath := AdaptPathEntry(ProjectFileName, PackData.BaseIncludePath, ['$(IncludePath)']);
      CppProjectLinkPath := AdaptPathEntry(ProjectFileName, PackData.BaseILINK_LibraryPath, ['$(ILINK_LibraryPath)']);
      ClassicCompiler := (IDEName <= TIDEName.delphixe8) or not PackData.Win32ClangCompiler;
    finally
      Reader.Free;
    end;
  finally
    PackData.Free;
  end;
{$ENDIF}
end;

function TMSBuildCompiler.AddCPPBuilderParameters(const ProjectFileName: string; IDEName: TIDEName; Settings: TMsBuildCompilationSettings; LocalSearchPath: string): string;
var
  CppProjectIncludePath, CppProjectLinkPath: string;
  ClassicCompiler: boolean;
begin
  //there is a bug in 12.1 which causes thousands of errors if we include the files in the IncludePath:
  //https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-697
  //so we will add them to the SystemIncludePath instead.
  //But as the bug is no more in Delphi 13, we go back to IncludePath for it.
  //The SystemIncludePath has to be constantly updated: for D12 it includes $(BDS)\lib\clang\15.0.7\include
  //But fo D13 it is $(BDS)\lib\clang\20\include. So if we can avoid modifying it, better.

  ReadLibPathsFromBCProj(ProjectFileName, IDEName, CppProjectIncludePath, CppProjectLinkPath, ClassicCompiler);
  var Lsp := '';
  if Settings.TargetPlatform.IDEInfo.IDEName <> TIDEName.delphi12 then Lsp := LocalSearchPath;

  Result := Result + ' /p:IncludePath=' + Quote( CppProjectIncludePath + Lsp);

  if Settings.TargetPlatform.IDEInfo.IDEName = TIDEName.delphi12 then
  begin
    Result := Result + ' /p:BCC_SystemIncludePath=' +
      Quote(LocalSearchPath + CppSystemIncludePath(Settings.TargetPlatform.IDEInfo.IDEName, Settings.TargetPlatform.PlatType, ClassicCompiler));
  end;

  Result := Result + ' /p:ILINK_LibraryPath=' +
    Quote(
      CppExtraLinkPath(Settings.TargetPlatform.IDEInfo.IDEName, Settings.TargetPlatform.PlatType,  Settings.TargetPlatform.PlatformMacroValue, ClassicCompiler)
      + CppProjectLinkPath
      + LocalSearchPath);

end;


function TMSBuildCompiler.BuildMsBuildParameters(const ProjectFileName: string; IDEName: TIDEName; Settings: TMsBuildCompilationSettings): string;
var
  LocalTargetConfig: string;
  LocalDefines: string;
  LocalBuildMode: string;
  LocalSearchPath: string;
begin
//  Logger.Note('Building MSBuild command line parameters');

  // target config
  if Settings.TargetConfig.IsNull then
  begin
    LocalTargetConfig := 'RELEASE';
//    Logger.Note(Format('TargetConfig not specified. Using "%s" as default target config', [LocalTargetConfig]));
  end
  else
    LocalTargetConfig := Settings.TargetConfig;

  // build all or compile?
  if Settings.BuildMode = TBuildMode.Compile then
    LocalBuildMode := 'Make'
  else
    LocalBuildMode := 'Build';

  // Search Path
  LocalSearchPath := '';
  case Settings.SearchPathMode of
    DelphiLib:
      begin
        //Linux64 (at least in D11) uses release only, and if you link to Debug and use zip it will fail to build with a linker error.
        //See https://quality.embarcadero.com/browse/RSP-36641?jql=text%20~%20%22linux64%20deflate%22
        if SameText(LocalTargetConfig, 'Debug') and (Settings.TargetPlatform.PlatType <> TPlatform.linux64) then
          LocalSearchPath := Settings.TargetPlatform.DebugLibDir + ';' + Settings.TargetPlatform.LibDir
        else
          LocalSearchPath := Settings.TargetPlatform.LibDir;

        // Add redist folder for specific platforms
        if Settings.TargetPlatform.PlatType in [TPlatform.linux64, TPlatform.macos64intel, TPlatform.macos64arm, TPlatform.win64Xintel] then
          LocalSearchPath := LocalSearchPath + ';' + Settings.TargetPlatform.RedistDir;

        if Settings.ExtraSearchPath.HasValue then
          LocalSearchPath := LocalSearchPath + ';' + Settings.ExtraSearchPath;

        LocalSearchPath := Settings.AddPreservedSearchPaths(LocalSearchPath);
        if Settings.PackageLibraryPath <> '' then LocalSearchPath := AdaptPathEntry(ProjectFileName, Settings.PackageLibraryPath, ['$(DCC_UnitSearchPath)']) + LocalSearchPath;

      end;
    ProjectSettings:
      if Settings.ExtraSearchPath.HasValue then
        LocalSearchPath := '$(UnitSearchPath);' + Settings.ExtraSearchPath;
  end;

  // build msbuild command line
  Result := Format(' /target:%s /nologo /p:config=%s /p:Platform="%s"',
    [LocalBuildMode, LocalTargetConfig, Settings.TargetPlatform.BuildName]);

  // Add local search path
  if LocalSearchPath <> '' then
  begin
    Result := Result + ' /p:UnitSearchPath=' + Quote(LocalSearchPath);
    Result := Result + ' /p:DelphiLibraryPath=' + Quote(LocalSearchPath);
  end;

  // optionally add the search path to c++builder search path as well
  // not used for Delphi-only projects
  var IsCppBuilder := TPath.GetExtension(ProjectFileName).ToLowerInvariant = '.cbproj';
  if IsCppBuilder then
  begin
    Result := Result + AddCPPBuilderParameters(ProjectFileName, IDEName, Settings, LocalSearchPath);
  end;


  // add conditional defines
  if Settings.Defines.HasValue and not Settings.IgnoreDefines then
  begin
    LocalDefines := Settings.Defines;
    if SameText(LocalTargetConfig, 'Release') or SameText(LocalTargetConfig, 'Debug') then
      LocalDefines := Uppercase(LocalTargetConfig) + ';' + LocalDefines;
    Result := Result + Format(' /p:DCC_Define="%s;$(DCC_Define)" ', [LocalDefines]);
  end;

  // Warnings as errors
  if Settings.WarningsAsErrors.HasValue and Settings.WarningsAsErrors.Value then
    Result := Result + ' /p:DCC_Warnings="error"';

  // Different registry settings
  if Settings.RegistryName <> '' then
    Result := Result + ' /p:BDSAppDataBaseDir=' + Quote(Settings.RegistryName);
//  Logger.Note(Result);
end;

procedure TMSBuildCompiler.DoCompile(const ProjectFile: string;
  IDEName: TIDEName; Settings: TMsBuildCompilationSettings);
begin
  if DoCompileDirectly(ProjectFile, IDEName, Settings) then exit;

  DoCompileWithBat(ProjectFile, IDEName, Settings);
end;

function TMSBuildCompiler.FindVariable(const VariableName: string; const Env: TArray<string>; out VariableValue: string; const Level: integer): boolean;
begin
  //Probably not worth to use a hashtable here, as entries should be little.
  Result := false;
  VariableValue := '';

  if Level > 10 then exit; //avoid infinite recursion, like a variable referencing itself.

  for var EnvEntry in Env do
  begin
    var idx := EnvEntry.IndexOf('=');
    if idx <= 0 then continue;
    var Name := EnvEntry.Substring(0, idx).Trim;
    if SameText(VariableName, Name) then
    begin
      VariableValue := ExpandPath(EnvEntry.Substring(idx + 1).Trim, Env, Level + 1);
      Result := true;
      exit;
    end;
  end;
end;

function TMSBuildCompiler.ExpandPath(const aPath: string; const Env: TArray<string>; const Level: integer): string;
begin
 var Path := aPath.Trim;
 if SameText(Path, '%PATH%') then exit('');  //We won't search in the user's path. msbuild must be in the paths inside rsvars.

 Result := '';
 var LastIdx := 0;
 while(true) do
 begin
   var idxStart := aPath.IndexOf('%', LastIdx);
   if idxStart < 0 then exit(Result + aPath.Substring(LastIdx));
   var idxEnd := aPath.IndexOf('%', IdxStart + 1);
   if idxEnd < 0 then exit(''); //missing closing %, we will just ignore this entry.

   Result := Result + aPath.Substring(LastIdx, idxStart - LastIdx);
   var VariableName := aPath.Substring(idxStart + 1, idxEnd - idxStart - 1);
   var VariableValue: string;
   if FindVariable(VariableName, Env, VariableValue, Level) then Result := Result + VariableValue
   else Result := Result +  GetEnvironmentVariable(VariableName);

   LastIdx := idxEnd + 1;

 end;

end;

function TMSBuildCompiler.FindMsBuildInPath(const Path: string; const Env: TArray<string>): string;
begin
  Result := '';
  if Path.Trim = '' then exit;
  var Entries := Path.Split([';']);
  if Length(Entries) = 0 then exit;

  for var p in Entries do
  begin
    var ExpandedPath := ExpandPath(p, Env, 0);
    if ExpandedPath.Trim = '' then continue;
    var MsBuildLocation := TPath.Combine(ExpandedPath, 'msbuild.exe');
    if TFile.Exists(MsBuildLocation) then exit(MsBuildLocation);


  end;

end;


function TMSBuildCompiler.FindMsBuild(const Env: TArray<string>): string;
begin
  Result := '';
  for var entry in Env do
  begin
    if SameText(GetVarName(entry), 'PATH') then
    begin
      exit(FindMsBuildInPath(GetVarValue(entry), Env));
    end;
  end;
end;

function TMSBuildCompiler.ParseRSVars(const RSVars: string): TArray<string>;
begin
  Result := nil;
  if not TFile.Exists(RSVars) then exit;
  var Lines := TFile.ReadAllLines(RSVars);
  SetLength(Result, Length(Lines));
  var ResultCount := 0;
  for var i := 0 to High(Lines) do
  begin
    var line := Trim(Lines[i]);
    if line = '' then continue;
    if line.StartsWith('@ECHO ', true) then continue;
    if line.StartsWith('@SET ', true) then
    begin
      Result[ResultCount] := line.Substring(5).Trim;
      inc (ResultCount);
      continue;
    end;
    exit(nil);
  end;

end;

function TMSBuildCompiler.GetVarName(const s: string): string;
begin
  var idx := s.IndexOf('=');
  if idx <= 0 then exit(s);
  Result := s.Substring(0, idx).Trim;
end;

function TMSBuildCompiler.GetVarValue(const s: string): string;
begin
  var idx := s.IndexOf('=');
  if idx <= 0 then exit('');
  Result := s.Substring(idx + 1).Trim;
end;

function TMSBuildCompiler.InEnvVarOverrides(const Env: string; const EnvVarOverrides: TArray<TEnvVar>): boolean;
begin
  Result := false;
  var EnvName := GetVarName(Env);
  for var EnvOver in EnvVarOverrides do
  begin
    if SameText(EnvName, EnvOver.Name.Trim) then exit(True);
  end;
end;

function TMSBuildCompiler.ExpandMacros( const ExistingEnv: TArray<string>; const s: string): string;
begin
  Result := s;
  for var env in ExistingEnv do
  begin
    Result := Result.Replace('$(' + GetVarName(env) + ')', GetVarValue(env), [TReplaceFlag.rfReplaceAll, TReplaceFlag.rfIgnoreCase]);
  end;
end;

function TMSBuildCompiler.AddEnvironmentOverrides(const IDEInfo: IDelphiIDEInfo; const ExistingEnv: TArray<string>): TArray<string>;
begin
  var EnvVarOverrides := IDEInfo.GetEnvVarOverrides;
  Result := nil;
  SetLength(Result, Length(ExistingEnv) + Length(EnvVarOverrides));
  var iResult := 0;
  for var OldEnv in ExistingEnv do
  begin
    if not InEnvVarOverrides(OldEnv, EnvVarOverrides) then
    begin
      Result[iResult] := OldEnv;
      inc(iResult);
    end;
  end;

  for var EnvVarOverride in EnvVarOverrides do
  begin
    Result[iResult] := EnvVarOverride.Name + '=' + ExpandMacros(ExistingEnv, EnvVarOverride.Value);
    inc(iResult);
  end;
  SetLength(Result, iResult);
end;

function TMSBuildCompiler.AddTMPEnv(const TmpVar: string; var ExistingEnv: TArray<string>; const TMPFolder: string): TArray<string>;
begin
  for var i := Low(ExistingEnv) to High(ExistingEnv) do
  begin
    if (ExistingEnv[i].Trim().StartsWith(TmpVar + ' ', true)) or (ExistingEnv[i].Trim().StartsWith(TmpVar + '=', true)) then
    begin
      ExistingEnv[i] := TmpVar + '=' + TMPFolder;
      exit;
    end;
  end;

  SetLength(ExistingEnv, Length(ExistingEnv) + 1);
  ExistingEnv[Length(ExistingEnv) - 1] := TmpVar + '=' + TMPFolder;

end;

function TMSBuildCompiler.DoCompileDirectly(const ProjectFile: string;
  IDEName: TIDEName; Settings: TMsBuildCompilationSettings): boolean;
begin
  var Env := ParseRSVars(Settings.TargetPlatform.IDEInfo.RsvarsFile);
  if Env = nil then exit(false);

  var MsBuild := FindMsBuild(Env);
  if MsBuild = '' then exit(false);

  //Do not add the environment override until we found the path for MsBuild.
  //We want to search for the msbuild path in whatever is in rsvars.bat, not in what
  //the IDE says.
  Env := AddEnvironmentOverrides(Settings.TargetPlatform.IDEInfo, Env);

  //To speed up bcc64 and other compilers. See https://github.com/tmssoftware/tms-smartsetup/issues/184
  var TMPFolder := TPath.Combine(TPath.GetDirectoryName(ProjectFile), GuidToStringN(TGUID.NewGuid));
  AddTMPEnv('TMP', Env, TMPFolder);
  AddTMPEnv('TEMP', Env, TMPFolder); //dcc64 seems to use tmp only, but we set both just in case.
  TDirectory_CreateDirectory(TMPFolder);
  try

    var Output := '';
    if not ExecuteCommand(MsBuild + ' "' + ProjectFile + '" ' + BuildMsBuildParameters(ProjectFile, IDEName, Settings), '', Output, Env) then
      raise Exception.Create('Failed to compile ' + ProjectFile);

  finally
    TryToDeleteAllFilesInFolderIgnoringLocked(TMPFolder, true, true);
  end;

  Result := true;
end;

function TMSBuildCompiler.GetEnvVarsForBat(const IDEInfo: IDelphiIDEInfo): string;
begin
  //This method is not needed when debugging since Delphi will set those variables
  //and the spawned CMD will inherit them. But when running in a standalone
  //command window, if we don't add this fmxlinux will fail.

  Result := '';
  var EnvOverrides := IDEInfo.GetEnvVarOverrides;
  for var Env in EnvOverrides do
  begin
    Result := Result + 'set ' + Env.Name + '=' + Env.Value + #13#10;
  end;
end;

procedure TMSBuildCompiler.DoCompileWithBat(const ProjectFile: string;
  IDEName: TIDEName; Settings: TMsBuildCompilationSettings);
var
  Batch: string;
  BatchFile: string;
begin
//  Logger.Note(Format('Compiling Delphi project using MSBuild: %s', [ProjectFile]));

  // create batch file and compile
  Batch := Format(
    'call "%s" '#13#10 +
    '%s'#13#10 +
    'cd /D %%FrameworkDir%% '#13#10 +
    'msbuild.exe %%*',
    [Settings.TargetPlatform.IDEInfo.RsvarsFile, GetEnvVarsForBat(Settings.TargetPlatform.IDEInfo)]
  );
//  Logger.Note('Batch file built');
//  Logger.Note(Batch);
  BatchFile := GetTempFileNameExt('.bat', TempFolder);
  try
    TDirectory_CreateDirectory(TPath.GetDirectoryName(BatchFile));
    TFile.WriteAllText(BatchFile, Batch);
    if not ExecuteCommand(Format('cmd /C call "%s" "%s" %s', [BatchFile, ProjectFile, BuildMsBuildParameters(ProjectFile, IDEName, Settings)])) then
      raise Exception.Create('Failed to compile ' + ProjectFile);
  finally
     DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, BatchFile);
  end;

//  if WarningsAsErrors and (Pos(' 0 Warning(s)', Output) = 0) then
//    raise ETaskException.Create('Delphi project build failed. Warnings were raised.');
end;

{ TDcc32Compiler }

class procedure TDcc32Compiler.Build(const ProjectFile: string;
  IDEName: TIDEName; Settings: TDcc32CompilationSettings; ATempFolder: string = '');
var
  Compiler: TDcc32Compiler;
begin
  Compiler := TDcc32Compiler.Create;
  try
    Compiler.TempFolder := ATempFolder;
    Compiler.DoCompile(ProjectFile, IDEName, Settings);
  finally
    Compiler.Free;
  end;
end;

function TDcc32Compiler.BuildDcc32Parameters(IDEName: TIDEName;
  Settings: TDcc32CompilationSettings): string;
var
  LocalDefines: string;
  LocalSearchPath: string;
  LB: string;
begin
  LB := ' ';  // uncomment this line to create command-line parameters
//  LB := sLineBreak; // uncomment this line to generate a .cfg file


  // Add default, fixed settings
  Result :=
    // Add all compile directive settings with default values
    '-$A+' + LB +
    '-$B-' + LB +
    '-$C+' + LB +
    '-$D+' + LB +
    '-$G+' + LB +
    '-$H+' + LB +
    '-$I+' + LB +
    '-$J-' + LB +
    '-$K-' + LB +
    '-$L+' + LB +
    '-$M-' + LB +
    '-$N+' + LB +
    '-$O+' + LB +
    '-$P+' + LB +
    '-$Q-' + LB +
    '-$R-' + LB +
    '-$S-' + LB +
    '-$T-' + LB +
    '-$U-' + LB +
    '-$V+' + LB +
    '-$W-' + LB +
    '-$X+' + LB +
    '-$YD' + LB +
    '-$Z1' + LB +
    // default image base address and stack size
    '-$M16384,1048576' + LB +
    '-K4194304' + LB +
    // GUI application
    '-cg' + LB +
    // Default unit aliases
    '-AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE;' + LB +
    // quitet mode
    '-Q' + LB;

  // Build all units?
  case Settings.BuildMode of
    BuildAll:
      Result := Result + '-B' + LB;
  end;

  // add conditional defines
  if SameText(Settings.TargetConfig, 'Debug') then
    LocalDefines := 'DEBUG'
  else
    LocalDefines := 'RELEASE';
  if Settings.Defines.HasValue then
    LocalDefines := LocalDefines + ';' + Settings.Defines;
  Result := Result + '-D' + LocalDefines + LB;

  // Warnings as errors
  if Settings.WarningsAsErrors.HasValue and Settings.WarningsAsErrors.Value then
    Result := Result + '-W^' + LB
  else
    Result := Result + '-W+' + LB;

  // Hints on
  Result := Result + '-H+' + LB;

  // search directories
  case Settings.SearchPathMode of
    ProjectSettings:
      begin
        LocalSearchPath := Settings.TargetPlatform.GetIDEPath(ptLibraryPath);
      end;
    DelphiLib:
      begin
        if SameText(Settings.TargetConfig, 'Debug') then
          LocalSearchPath := Settings.TargetPlatform.DebugLibDir + ';' + Settings.TargetPlatform.LibDir
        else
          LocalSearchPath := Settings.TargetPlatform.LibDir;
      end;
  end;

  // expand variables. No need for Delphi 2007 and up because we will use rsvars.bat
  // that already sets the environment variables
  if IDEName = delphi7 then
    LocalSearchPath := StringReplace(LocalSearchPath, '$(DELPHI)',
      ExcludeTrailingPathDelimiter(Settings.TargetPlatform.IDEInfo.RootDir), [rfReplaceAll, rfIgnoreCase]);

  // add extra search path
  if Settings.ExtraSearchPath.HasValue then
    LocalSearchPath := LocalSearchPath + ';' + Settings.ExtraSearchPath;

  LocalSearchPath := Settings.AddPreservedSearchPaths(LocalSearchPath);

  // Now add search path
  Result := Result +
    '-U' + Quote(LocalSearchPath) + LB +
    '-O' + Quote(LocalSearchPath) + LB +
    '-I' + Quote(LocalSearchPath) + LB +
    '-R' + Quote(Settings.OriginalDpkPath) + ';' + Quote(LocalSearchPath) + LB;

  // Output directories
  if Settings.ExeOutputDir.HasValue then
    Result := Result + '-E' + Quote(Settings.ExeOutputDir) + LB;

  if Settings.BplOutputDir.HasValue then
    Result := Result + '-LE' + Quote(Settings.BplOutputDir) + LB;

  if Settings.DcpOutputDir.HasValue then
  begin
    Result := Result + '-LN' + Quote(Settings.DcpOutputDir) + LB; // .dcp output dir
    Result := Result + '-NB' + Quote(Settings.DcpOutputDir) + LB; // .bpi output dir
  end;

  if Settings.DcuOutputDir.HasValue then
  begin
    if IDEName = delphi7 then
      Result := Result + '-N' + Quote(Settings.DcuOutputDir) + LB
    else
      Result := Result + '-N0' + Quote(Settings.DcuOutputDir) + LB;
  end;

  if Settings.HppOutputDir.HasValue then
    Result := Result + '-NH' + Quote(Settings.HppOutputDir) + LB;

  if Settings.ObjOutputDir.HasValue then
    Result := Result + '-NO' + Quote(Settings.ObjOutputDir) + LB;

  Result := Result +
    '-Z' + LB; // if present, it's "never build"

  if IDEName >= delphi2007 then
  begin
    Result := Result +
      '-JL' + LB;
  end;

//C:\Program Files\CodeGear\RAD Studio\5.0\bin\DCC32.EXE "%delphiproj%"
end;

procedure TDcc32Compiler.DoCompile(ProjectFile: string;
  IDEName: TIDEName; Settings: TDcc32CompilationSettings);
var
  Batch: string;
  BatchFile: string;
  WorkingDir: string;
begin
//  Logger.Note(Format('Compiling Delphi project using DCC32: %s', [ProjectFile]));

  WorkingDir := TPath.GetDirectoryName(ProjectFile);
  // create batch file and compile
  // This batch file is used to call rsvars.bat so that we are sure that environment variables like $BDSLIB
  // and others are available in the environment thus we don't need to expand
  // those variables to pass to command line
  // Note: in Delphi 7 we don't have rsvars.bat but we will use the same approach to unify the code
  Batch := Format(
    '"%s" %%*',
    [Settings.TargetPlatform.IDEInfo.Dcc32File]
  );

  // call rsvars.bat
  if IDEName > delphi7 then
    Batch := Format('call "%s" '#13#10,
      [Settings.TargetPlatform.IDEInfo.RsvarsFile])
      + Batch;

  // Force output directories - dcc32 won't create them...!
  if Settings.BplOutputDir.HasValue then
    TDirectory_CreateDirectory(TPath.Combine(WorkingDir, Settings.BplOutputDir));
  if Settings.DcpOutputDir.HasValue then
    TDirectory_CreateDirectory(TPath.Combine(WorkingDir, Settings.DcpOutputDir));
  if Settings.DcuOutputDir.HasValue then
    TDirectory_CreateDirectory(TPath.Combine(WorkingDir, Settings.DcuOutputDir));
  if Settings.ExeOutputDir.HasValue then
    TDirectory_CreateDirectory(TPath.Combine(WorkingDir, Settings.ExeOutputDir));
  if Settings.HppOutputDir.HasValue then
    TDirectory_CreateDirectory(TPath.Combine(WorkingDir, Settings.HppOutputDir));
  if Settings.ObjOutputDir.HasValue then
    TDirectory_CreateDirectory(TPath.Combine(WorkingDir, Settings.ObjOutputDir));

  // Now effectively run the compiler
//  Logger.Note('Batch file built');
//  Logger.Note(Batch);
  BatchFile := GetTempFileNameExt('.bat', TempFolder);
  try
    TDirectory_CreateDirectory(TPath.GetDirectoryName(BatchFile));
    TFile.WriteAllText(BatchFile, Batch);
    var PkgExt := '.dpk';
    if Settings.IsDpr then PkgExt := '.dpr';

    if SameText(TPath.GetExtension(ProjectFile), '.dproj') then
      ProjectFile := TPath.ChangeExtension(ProjectFile, PkgExt);

    if not ExecuteCommand(
      Format('cmd /C call "%s" "%s" %s', [BatchFile, ProjectFile, BuildDcc32Parameters(IDEName, Settings)]),
      WorkingDir) then
      raise Exception.Create('Failed to compile ' + ProjectFile);

  finally
    DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, BatchFile);
  end;

end;

{ TBdsCompiler }

class procedure TBdsCompiler.Build(const ProjectFile: string; IDEName: TIDEName;
  Settings: TBdsCompilationSettings; ATempFolder: string);
begin
var
  Compiler: TBdsCompiler;
begin
  Compiler := TBdsCompiler.Create;
  try
    Compiler.TempFolder := ATempFolder;
    Compiler.DoCompile(ProjectFile, IDEName, Settings);
  finally
    Compiler.Free;
  end;
end;

end;

function TBdsCompiler.BuildBdsParameters(IDEName: TIDEName;
  Settings: TBdsCompilationSettings; const ErrFile: string): string;
begin
  Result := ' -b -ns -o"' + ErrFile + '" ';
end;

class constructor TBdsCompiler.Create;
begin
  BDSLock := TObject.Create;
end;

class destructor TBdsCompiler.Destroy;
begin
  BDSLock.Free;
end;

procedure TBdsCompiler.ModifyConfig(const IDEName: TIDEName; const ProjectFile: string; const Settings: TBdsCompilationSettings);
begin
{$IFDEF MSWINDOWS}
  var LocalTargetConfig := '';
  if Settings.TargetConfig.IsNull then
  begin
    LocalTargetConfig := 'Release';
  end
  else
    LocalTargetConfig := Settings.TargetConfig.Value;

  // Search Path
  var LocalSearchPath := '';
  case Settings.SearchPathMode of
    DelphiLib:
      begin
        //Linux64 (at least in D11) uses release only, and if you link to Debug and use zip it will fail to build with a linker error.
        //See https://quality.embarcadero.com/browse/RSP-36641?jql=text%20~%20%22linux64%20deflate%22
        if SameText(LocalTargetConfig, 'Debug') and (Settings.TargetPlatform.PlatType <> TPlatform.linux64) then
          LocalSearchPath := Settings.TargetPlatform.DebugLibDir + ';' + Settings.TargetPlatform.LibDir
        else
          LocalSearchPath := Settings.TargetPlatform.LibDir;

        // Add redist folder for specific platforms
        if Settings.TargetPlatform.PlatType in [TPlatform.linux64, TPlatform.macos64intel, TPlatform.macos64arm, TPlatform.win64Xintel] then
          LocalSearchPath := LocalSearchPath + ';' + Settings.TargetPlatform.RedistDir;

        if Settings.ExtraSearchPath.HasValue then
          LocalSearchPath := LocalSearchPath + ';' + Settings.ExtraSearchPath;
        LocalSearchPath := Settings.AddPreservedSearchPaths(LocalSearchPath);
      end;
    ProjectSettings:
      if Settings.ExtraSearchPath.HasValue then
        LocalSearchPath := '$(UnitSearchPath);' + Settings.ExtraSearchPath;
  end;

  var DProjModifier := TDprojModifier.Create(ProjectFile, IDEName); //LocalTargetConfig, LocalSearchPath, );
  try
    var BaseNodeForSearch: IInterface := nil;
    try
    var BaseNodeForDefines: IInterface := nil;
    try
      DProjModifier.LoopOverAllNodes(function (NodeName, NodeText: string; PropGroupIsBase: boolean): string
        begin
          if (NodeName = '/#document/Project/PropertyGroup/Config/#text')  then exit(LocalTargetConfig);

          // Avoid "Do you want to save changes" dialog when closing it. With luck we will never arrive to v99.
          if (NodeName = '/#document/Project/PropertyGroup/ProjectVersion/#text') then exit ('99.0');

          if (NodeName = '/#document/Project/PropertyGroup/Platform/#text') then exit(Settings.TargetPlatform.BuildName);
          if (NodeName = '/#document/Project/PropertyGroup/DCC_UnitSearchPath/#text') then
            begin;
              if PropGroupIsBase then BaseNodeForSearch := nil;
              exit(LocalSearchPath + ';' + NodeText);
            end;

          if (NodeName = '/#document/Project/PropertyGroup/DCC_Define/#text') then
            begin;
              if PropGroupIsBase then BaseNodeForDefines := nil;
              if Settings.Defines.HasValue then exit(Settings.Defines.Value + ';' + NodeText);
            end;

          Result := NodeText;
        end,
        procedure (Node: IInterface)
        begin
          BaseNodeForSearch := Node;
          BaseNodeForDefines := Node;
        end
      );

      if BaseNodeForSearch <> nil then DProjModifier.AddChildNode(BaseNodeForSearch, 'DCC_UnitSearchPath', LocalSearchPath);
      if (BaseNodeForDefines <> nil) and (Settings.Defines.HasValue) then DProjModifier.AddChildNode(BaseNodeForDefines, 'DCC_Define', Settings.Defines.Value);

    finally
      BaseNodeForDefines := nil; //remove the reference so it can be freed before DProjModifier.
    end;
    finally
      BaseNodeForSearch := nil; //remove the reference so it can be freed before DProjModifier.
    end;

     // Avoid "Do you want to save changes" dialog when closing it.
    DProjModifier.SetAttIfExists('/#document/Project/ProjectExtensions/BorlandProject/Deployment', 'Version', '99');
    DProjModifier.Save(ProjectFile);

  finally
    DProjModifier.Free;
  end;
{$ENDIF}
end;

function TBdsCompiler.GetEnvVariablesForCE(const Settings: TBdsCompilationSettings): TArray<string>;
begin
  //Add the win32 bpl to the PATH so delphi doesn't crash when starting to compile the packages.
  //Note that as Delphi is a Win32 app we need to add the win32 path.
  var Win32Platform: IDelphiPlatformInfo := Settings.TargetPlatform.IDEInfo.GetPlatform(TPlatform.win32intel);
  var LinkedFolder := TPath.Combine(Settings.BplFolder, Win32Platform.PlatformMacroValue);
  var ExtPath := AddPaths(GetEnvironmentVariable('PATH'), LinkedFolder, true);
  Result := ['PATH=' + ExtPath];
end;

procedure TBdsCompiler.DoCompile(const ProjectFile: string; IDEName: TIDEName;
  Settings: TBdsCompilationSettings);
begin
  var HasErrors := false;
  var BdsBuild := Settings.TargetPlatform.IDEInfo.BdsFile;
  TDirectory_CreateDirectory(TempFolder);
  var ErrFile := TPath.Combine(TempFolder, GuidToStringN(TGuid.NewGuid) + '.err');
  try
    TMonitor.Enter(BDSLock);
    try
      Logger.Info('Rad Studio CE detected. Disabling multithreaded compilation.');
      ModifyConfig(IDEName, ProjectFile, Settings); //This will modify the file in Parallel folder, so it doesn't matter.
      var DummyOutput := '';
      var Env := GetEnvVariablesForCE(Settings);
      if not ExecuteCommand(
         BdsBuild + ' ' + BuildBdsParameters(IDEName, Settings, ErrFile) + ' "' + ProjectFile + '"',
         '', DummyOutput, Env) then HasErrors :=true;

      var Messages: string;
      if not TFile.Exists(ErrFile) then
      begin
        Messages := 'Error: Can''t find the file "' + ErrFile + '"';
      end
      else begin
        Messages := TFile.ReadAllText(ErrFile);
      end;
      Logger.Trace(Messages);
      if not Messages.Contains(#10'Success'#13) then HasErrors := true;

      if HasErrors then raise Exception.Create('Failed to compile ' + ProjectFile);
    finally
      TMonitor.Exit(BDSLock);
    end;
  finally
    DeleteFile(ErrFile); //no real need to move to locked.
  end;
end;


procedure SetupResinator(const ResinatorPath, TempPath: string);
begin
  //force resinator to extract the windows headers to the folder %localappdata%\temp if needed.
  //This has a global lock because the folder is shared by everyone.
  //See https://github.com/tmssoftware/smartsetup-registry/issues/5
  var Lock := TMutex.Create(nil, false, Mutex_Resinator);
  try
    Lock.Acquire;
    try
      TDirectory_CreateDirectory(TempPath);
      TFile.WriteAllText(TPath.Combine(TempPath, 'resource_empty.rc'), '');
      var ResinatorCommand := '"' + ResinatorPath + '" -v resource_empty.rc -foresource_empty.res';
      ExecuteCommand(ResinatorCommand, TempPath);
    finally
      Lock.Release;
    end;
  finally
    Lock.Free;
  end;

end;
end.

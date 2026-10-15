unit Deget.Compilation.Delphi;

{$IFDEF MSWINDOWS}

interface


uses
  Deget.CoreTypes,
  Deget.Compilation,
  UProjectDefinition,
  UConfigDefinition,
  Deget.IDEInfo,
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
    procedure ModifyConfig(const ProjectFile: string;
      const Settings: TBdsCompilationSettings);
    function GetEnvVariablesForCE(const Settings: TBdsCompilationSettings): TArray<string>;
  public
    function BuildBdsParameters(Settings: TBdsCompilationSettings; const ErrFile: string; const RegEntry: string): string;
    procedure DoCompile(const ProjectFile: string; Settings: TBdsCompilationSettings);
    property TempFolder: string read FTempFolder write FTempFolder;
  public
    class procedure Build(const ProjectFile: string; Settings: TBdsCompilationSettings; ATempFolder: string);
  end;

  TMSBuildCompiler = class
  strict private
    FTempFolder: string;
    function FindMsBuildInPath(const Path: string): string;
    function FindMsBuild(const Env: TArray<string>): string;
    function AddEnvVars(const EnvVars: TArray<TEnvVar>; const ExistingEnv: TArray<string>): TArray<string>;
    function AddEnvironmentOverrides(const IDEInfo: IDelphiIDEInfo; const Platform: TPlatform;
      const ExistingEnv: TArray<string>): TArray<string>;
    function GetMacroEnvVars(const Settings: TMsBuildCompilationSettings): TArray<TEnvVar>;
    function DoCompileDirectly(const ProjectFile: string; Settings: TMsBuildCompilationSettings): boolean;
  private
    function InEnvVarOverrides(const Env: string;
      const EnvVarOverrides: TArray<TEnvVar>): boolean;
    function AddTMPEnv(const TmpVar: string; var ExistingEnv: TArray<string>;
      const TMPFolder: string): TArray<string>;
    function CppSystemIncludePath(const DelphiVersion: TIDEName;
      const DPlat: TPlatform; const ClassicCompiler: boolean): string;
    function CppExtraLinkPath(const DelphiVersion: TIDEName;
      const DPlat: TPlatform; const PlatformId: string; const ClassicCompiler: boolean): string;
    function AddCPPBuilderParameters(const ProjectFileName: string;
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
    function BuildMsBuildParameters(const ProjectFileName: string; Settings: TMsBuildCompilationSettings): string;
    procedure DoCompile(const ProjectFile: string; Settings: TMsBuildCompilationSettings);
    property TempFolder: string read FTempFolder write FTempFolder;
  public
    class procedure Build(const ProjectFile: string; Settings: TMsBuildCompilationSettings; ATempFolder: string = '');
  end;

  TDcc32Compiler = class
  strict private
    FTempFolder: string;
  public
    function BuildDcc32Parameters(Settings: TDcc32CompilationSettings): string;
    procedure DoCompile(ProjectFile: string; Settings: TDcc32CompilationSettings);
    property TempFolder: string read FTempFolder write FTempFolder;
  public
    class procedure Build(const ProjectFile: string; Settings: TDcc32CompilationSettings; ATempFolder: string = '');
  end;

procedure SetupResinator(const ResinatorPath, TempPath: string);

implementation

uses
  SyncObjs,
  Windows,
  System.SysUtils, System.IOUtils,
  System.StrUtils, Generics.Defaults, Generics.Collections,
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
  Deget.Registry,
  UMultiLogger;

procedure SplitEnvEntry(const EnvEntry: string; out VarName, VarValue: string);
begin
  VarName := '';
  VarValue := '';
  var idx := EnvEntry.IndexOf('=');
  if idx <= 0 then exit;
  VarName := EnvEntry.Substring(0, idx).Trim;
  VarValue := EnvEntry.Substring(idx + 1).Trim;
end;

function ReplaceVariables(const VarValue: string; const Variables: TOrderedDictionary<string, string>): string;
begin
  var VarValueTrimmed := VarValue.Trim;
  Result := '';
  var LastIdx := 0;
  while(true) do
  begin
    var idxStart := VarValueTrimmed.IndexOf('%', LastIdx);
    if idxStart < 0 then exit(Result + VarValueTrimmed.Substring(LastIdx));
    var idxEnd := VarValueTrimmed.IndexOf('%', IdxStart + 1);
    if idxEnd < 0 then exit(Result + VarValueTrimmed.Substring(LastIdx)); //missing closing %, we will just ignore this entry.

    var VariableName := VarValueTrimmed.Substring(idxStart + 1, idxEnd - idxStart - 1);
    var ExpandedValue: string;
    if Variables.TryGetValue(VariableName, ExpandedValue) then
    begin
      Result := Result + VarValueTrimmed.Substring(LastIdx, idxStart - LastIdx) + ExpandedValue;
      LastIdx := idxEnd + 1;
    end
    else if SameText(VariableName, 'PATH') then
    begin
      //We won't search in the user's path, or pass it as env variable. msbuild must be in the paths inside rsvars.
      //This isolates more the build of whatever stuff could be in the user's path.
      Result := Result + VarValueTrimmed.Substring(LastIdx, idxStart - LastIdx);
      LastIdx := idxEnd + 1;
    end
    else if Windows.GetEnvironmentVariable(PChar(VariableName), nil, 0) > 0 then
    begin
      Result := Result + VarValueTrimmed.Substring(LastIdx, idxStart - LastIdx) + GetEnvironmentVariable(VariableName);
      LastIdx := idxEnd + 1;
    end
    else
    begin
      //Not a variable (like a literal % in a folder name). Keep the text and let the closing % start a new variable,
      //so "50%off;bin;%PATH%" doesn't swallow "bin".
      Result := Result + VarValueTrimmed.Substring(LastIdx, idxEnd - LastIdx);
      LastIdx := idxEnd;
    end;
  end;
end;

function IsFullyQualifiedPath(const Path: string): boolean;
begin
  //Either C:\... or \\server\... Paths like \bin or C:bin depend on the current drive or folder.
  if Path.StartsWith('\\') then exit(Length(Path) > 2);
  Result := (Length(Path) >= 3) and CharInSet(Path[1], ['A'..'Z', 'a'..'z']) and (Path[2] = ':') and CharInSet(Path[3], ['\', '/']);
end;

function ParseRSVars(const RSVars: string): TArray<string>;
begin
  Result := nil;
  if not TFile.Exists(RSVars) then exit;

  var Variables := TOrderedDictionary<string, string>.Create(TIStringComparer.Ordinal);
  try
    var Lines := TFile.ReadAllLines(RSVars);
    for var i := 0 to High(Lines) do
    begin
      var line := Trim(Lines[i]);
      if line = '' then continue;
      if line.StartsWith('@ECHO ', true) then continue;
      if line.StartsWith('@SET ', true) then
      begin
        var VarName, VarValue: string;
        SplitEnvEntry(line.Substring(5).Trim, VarName, VarValue);
        if VarName <> '' then
        begin
          var ExpandedVarValue := ReplaceVariables(VarValue, Variables);
          if Variables.ContainsKey(VarName) then Variables.Remove(VarName);
          Variables.Add(VarName, ExpandedVarValue);
        end;
        continue;
      end;
      continue; //do not complain in an unknown line. it might be a REM or something that doesn't matter.
    end;
    SetLength(Result, Variables.Count);
    for var i := 0 to High(Result) do
    begin
      Result[i] := Variables.KeyList[i] + '=' + Variables.ValueList[i];
    end;
  finally
    Variables.Free;
  end;

end;

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
  Settings: TMsBuildCompilationSettings; ATempFolder: string = '');
var
  Compiler: TMsBuildCompiler;
begin
  Compiler := TMsBuildCompiler.Create;
  try
    Compiler.TempFolder := ATempFolder;
    Compiler.DoCompile(ProjectFile, Settings);
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
end;

function TMSBuildCompiler.AddCPPBuilderParameters(const ProjectFileName: string; Settings: TMsBuildCompilationSettings; LocalSearchPath: string): string;
var
  CppProjectIncludePath, CppProjectLinkPath: string;
  ClassicCompiler: boolean;
begin
  //there is a bug in 12.1 which causes thousands of errors if we include the files in the IncludePath:
  //https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-697
  //so we will add them to the SystemIncludePath instead.
  //But as the bug is no more in Delphi 13, we go back to IncludePath for it.
  //The SystemIncludePath has to be constantly updated: for D12 it includes $(BDS)\lib\clang\15.0.7\include
  //But for D13 it is $(BDS)\lib\clang\20\include. So if we can avoid modifying it, better.

  ReadLibPathsFromBCProj(ProjectFileName, Settings.TargetPlatform.IDEInfo.IDEName, CppProjectIncludePath, CppProjectLinkPath, ClassicCompiler);
  var Lsp := '';
  if Settings.TargetPlatform.IDEInfo.IDEName <> TIDEName.delphi12 then Lsp := LocalSearchPath;

  Result := Result + ' /p:IncludePath=' + Quote(CppProjectIncludePath + Lsp);

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


function TMSBuildCompiler.BuildMsBuildParameters(const ProjectFileName: string; Settings: TMsBuildCompilationSettings): string;
var
  LocalTargetConfig: string;
  LocalDefines: string;
  LocalBuildMode: string;
  LocalSearchPath: string;
begin
//  Logger.Note('Building MSBuild command line parameters');

  // target config
  LocalTargetConfig := Settings.GetTargetConfig;

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
  Result := Format(' /target:%s /nologo /p:config=%s /p:Platform="%s" /p:ProductVersion="%s"',
    [LocalBuildMode, LocalTargetConfig, Settings.TargetPlatform.BuildName, DelphiProductVersion[Settings.TargetPlatform.IDEInfo.IDEName]]);

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
    Result := Result + AddCPPBuilderParameters(ProjectFileName, Settings, LocalSearchPath);
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

  if Settings.ExtraCompilerParameters <> '' then Result := Result + ' ' + Settings.ExtraCompilerParameters;


//  Logger.Note(Result);
end;

procedure TMSBuildCompiler.DoCompile(const ProjectFile: string;
  Settings: TMsBuildCompilationSettings);
begin
  if DoCompileDirectly(ProjectFile, Settings) then exit;
  raise Exception.Create('Can''t find msbuild.exe in "' + Settings.TargetPlatform.IDEInfo.RsvarsFile
    + '". The file must exist, contain only @SET and @ECHO lines, and its PATH must include the folder where msbuild.exe is.');
end;

function TMSBuildCompiler.FindMsBuildInPath(const Path: string): string;
begin
  Result := '';
  if Path.Trim = '' then exit;
  var Entries := Path.Split([';'], TStringSplitOptions.ExcludeEmpty);
  if Length(Entries) = 0 then exit;

  for var pUntrimmed in Entries do
  begin
    var p := pUntrimmed.Trim;
    if not IsFullyQualifiedPath(p) then continue; //relative entries (like from an undefined variable) would resolve against the current folder.
    var MsBuildLocation := TPath.Combine(p, 'msbuild.exe');
    if TFile.Exists(MsBuildLocation) then exit(MsBuildLocation);


  end;

end;


function TMSBuildCompiler.FindMsBuild(const Env: TArray<string>): string;
begin
  Result := '';
  for var i := High(Env) downto Low(Env) do
  begin
    if SameText(GetVarName(Env[i]), 'PATH') then
    begin
      exit(FindMsBuildInPath(GetVarValue(Env[i])));
    end;
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

function TMSBuildCompiler.AddEnvVars(const EnvVars: TArray<TEnvVar>; const ExistingEnv: TArray<string>): TArray<string>;
begin
  Result := nil;
  SetLength(Result, Length(ExistingEnv) + Length(EnvVars));
  var iResult := 0;
  for var OldEnv in ExistingEnv do
  begin
    if not InEnvVarOverrides(OldEnv, EnvVars) then
    begin
      Result[iResult] := OldEnv;
      inc(iResult);
    end;
  end;

  for var EnvVar in EnvVars do
  begin
    Result[iResult] := EnvVar.Name + '=' + ExpandMacros(ExistingEnv, EnvVar.Value);
    inc(iResult);
  end;
  SetLength(Result, iResult);
end;

function TMSBuildCompiler.AddEnvironmentOverrides(const IDEInfo: IDelphiIDEInfo; const Platform: TPlatform; const ExistingEnv: TArray<string>): TArray<string>;
begin
  Result := AddEnvVars(IDEInfo.GetEnvVarOverrides(Platform), ExistingEnv);
end;

function TMSBuildCompiler.GetMacroEnvVars(const Settings: TMsBuildCompilationSettings): TArray<TEnvVar>;
begin
  //MSBuild doesn't expand macros like $(Platform) in the /p: parameters, but the Delphi targets
  //will expand them from the environment. So we set them to the values of the platform we are building.
  Result := [
    TEnvVar.Create('ProductVersion', DelphiProductVersion[Settings.TargetPlatform.IDEInfo.IDEName]),
    TEnvVar.Create('Platform', Settings.TargetPlatform.BuildName),
    TEnvVar.Create('Config', Settings.GetTargetConfig)];
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
  Settings: TMsBuildCompilationSettings): boolean;
begin
  var Env := ParseRSVars(Settings.TargetPlatform.IDEInfo.RsvarsFile);
  if Env = nil then exit(false);

  var MsBuild := FindMsBuild(Env);
  if MsBuild = '' then exit(false);

  //Do not add the environment override until we found the path for MsBuild.
  //We want to search for the msbuild path in whatever is in rsvars.bat, not in what
  //the IDE says.
  //Macros go first, so the IDE overrides can use them too.
  Env := AddEnvVars(GetMacroEnvVars(Settings), Env);
  for var Platform in TDelphiIDEInfo.IDEPlatforms do
  begin
    //Env vars can be in 'Environment Variables' or 'Environment Variables (x64)' entries.
    //We will search in both.
    Env := AddEnvironmentOverrides(Settings.TargetPlatform.IDEInfo, Platform, Env);
  end;

  //To speed up bcc64 and other compilers. See https://github.com/tmssoftware/tms-smartsetup/issues/184
  var TMPFolder := TPath.Combine(TPath.GetDirectoryName(ProjectFile), GuidToStringN(TGUID.NewGuid));
  AddTMPEnv('TMP', Env, TMPFolder);
  AddTMPEnv('TEMP', Env, TMPFolder); //dcc64 seems to use tmp only, but we set both just in case.
  TDirectory_CreateDirectory(TMPFolder);
  try

    var Output := '';
    if not ExecuteCommand(QuoteCmd(MsBuild) + ' "' + ProjectFile + '" ' + BuildMsBuildParameters(ProjectFile, Settings), '', Output, Env) then
      raise Exception.Create('Failed to compile ' + ProjectFile);

  finally
    TryToDeleteAllFilesInFolderIgnoringLocked(TMPFolder, true, true);
  end;

  Result := true;
end;

{ TDcc32Compiler }

class procedure TDcc32Compiler.Build(const ProjectFile: string;
  Settings: TDcc32CompilationSettings; ATempFolder: string = '');
var
  Compiler: TDcc32Compiler;
begin
  Compiler := TDcc32Compiler.Create;
  try
    Compiler.TempFolder := ATempFolder;
    Compiler.DoCompile(ProjectFile, Settings);
  finally
    Compiler.Free;
  end;
end;

function TDcc32Compiler.BuildDcc32Parameters(Settings: TDcc32CompilationSettings): string;
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

  // expand variables. Only Delphi 7 uses $(DELPHI) in its paths.
  if Settings.TargetPlatform.IDEInfo.IDEName = delphi7 then
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
    if Settings.TargetPlatform.IDEInfo.IDEName = delphi7 then
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

  if Settings.TargetPlatform.IDEInfo.IDEName >= delphi2007 then
  begin
    Result := Result +
      '-JL' + LB;
  end;

//C:\Program Files\CodeGear\RAD Studio\5.0\bin\DCC32.EXE "%delphiproj%"
end;

procedure TDcc32Compiler.DoCompile(ProjectFile: string;
  Settings: TDcc32CompilationSettings);
var
  WorkingDir: string;
begin
//  Logger.Note(Format('Compiling Delphi project using DCC32: %s', [ProjectFile]));

  WorkingDir := TPath.GetDirectoryName(ProjectFile);

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
  var PkgExt := '.dpk';
  if Settings.IsDpr then PkgExt := '.dpr';

  if SameText(TPath.GetExtension(ProjectFile), '.dproj') then
    ProjectFile := TPath.ChangeExtension(ProjectFile, PkgExt);

  var Env := ParseRSVars(Settings.TargetPlatform.IDEInfo.RsvarsFile);

  var Output := '';
  if not ExecuteCommand(
    QuoteCmd(Settings.TargetPlatform.IDEInfo.Dcc32File) + ' ' + Quote(ProjectFile) + ' ' + BuildDcc32Parameters(Settings),
    WorkingDir, Output, Env) then
    raise Exception.Create('Failed to compile ' + ProjectFile);

end;

{ TBdsCompiler }

class procedure TBdsCompiler.Build(const ProjectFile: string;
  Settings: TBdsCompilationSettings; ATempFolder: string);
begin
var
  Compiler: TBdsCompiler;
begin
  Compiler := TBdsCompiler.Create;
  try
    Compiler.TempFolder := ATempFolder;
    Compiler.DoCompile(ProjectFile, Settings);
  finally
    Compiler.Free;
  end;
end;

end;

function TBdsCompiler.BuildBdsParameters(Settings: TBdsCompilationSettings; const ErrFile: string; const RegEntry: string): string;
begin
  Result := ' -b -ns -o"' + ErrFile + '" ';
  if RegEntry <> '' then Result := ' "-r' + RegEntry + '" ' + Result;

end;

class constructor TBdsCompiler.Create;
begin
  BDSLock := TObject.Create;
end;

class destructor TBdsCompiler.Destroy;
begin
  BDSLock.Free;
end;

procedure TBdsCompiler.ModifyConfig(const ProjectFile: string; const Settings: TBdsCompilationSettings);
begin
  var LocalTargetConfig := Settings.GetTargetConfig;

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

  var DProjModifier := TDprojModifier.Create(ProjectFile, Settings.TargetPlatform.IDEInfo.IDEName); //LocalTargetConfig, LocalSearchPath, );
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
    DProjModifier.EnsureAllPlatforms; //with incomplete platforms, you get a "do you want to save changes" dialog too
    DProjModifier.Save(ProjectFile);

  finally
    DProjModifier.Free;
  end;
end;

function TBdsCompiler.GetEnvVariablesForCE(const Settings: TBdsCompilationSettings): TArray<string>;
begin
  //Add the win32 bpl to the PATH so delphi doesn't crash when starting to compile the packages.
  //Note that as Delphi is a Win32 app we need to add the win32 path.
  var Win32Platform: IDelphiPlatformInfo := Settings.TargetPlatform.IDEInfo.GetPlatform(TPlatform.win32intel);
  var LinkedFolder := TPath.Combine(Settings.BplFolder, Win32Platform.PlatformMacroValue);
  var ExtPath := AddPaths(GetEnvironmentVariable('PATH'), LinkedFolder, true);
  Result := ['PATH=' + ExtPath];

  //not sure it helps, but just in case
  Result := Result + ['DBK_ENABLE_WAITCHAIN=0'];
end;

function BdsLogHasSuccess(const Messages: string): boolean;
const
  // The IDE writes a line with the localized "Success" string at the end of a successful build.
  // The IDE is only localized to English, German, French and Japanese. Strings taken from the
  // coreideXXX.bpl/.de/.fr/.ja resources, and they are the same from Delphi 10.4 to Delphi 13.
  SuccessStrings: array[0..3] of string = ('Success', 'Erfolg', 'Succ'#$00E8's', #$6210#$529F);
begin
  MatchStr(Messages, SuccessStrings);
  for var s in SuccessStrings do
  begin
    if Messages.Contains(#10 + s + #13) or Messages.Contains(#10 + s + #10) then exit(true);
  end;
  Result := false;
end;

procedure TBdsCompiler.DoCompile(const ProjectFile: string;
  Settings: TBdsCompilationSettings);
const
  RegEntryRoot = '$tmssmartsetup-tmp';
begin
  var HasErrors := false;
  var BdsBuild := Settings.TargetPlatform.IDEInfo.BdsFile;
  TDirectory_CreateDirectory(TempFolder);
  var ErrFile := TPath.Combine(TempFolder, GuidToStringN(TGuid.NewGuid) + '.err');
  try
    TMonitor.Enter(BDSLock);
    try
      Logger.Info('Rad Studio CE detected. Disabling multithreaded compilation.');
      ModifyConfig(ProjectFile, Settings); //This will modify the file in Parallel folder, so it doesn't matter.
      var DummyOutput := '';
      var Env := GetEnvVariablesForCE(Settings);
      var RegEntry := RegEntryRoot + '\' + GuidToStringN(TGUID.NewGuid);
      try
        if not ExecuteCommand(
           QuoteCmd(BdsBuild) + ' ' + BuildBdsParameters(Settings, ErrFile, RegEntry) + ' "' + ProjectFile + '"',
           '', DummyOutput, Env) then HasErrors :=true;
      finally
        var RootReg := TPath.GetDirectoryName(TPath.GetDirectoryName(Settings.TargetPlatform.IDEInfo.BaseKey));
        //Technically, we could just delete RegEntry, not RegEntryRoot.
        //It might even be needed if we wanted to do parallel bds compiling.
        //But deleting the full branch ensures we never have leftover from failed compilations
        RegDeleteKey(HKEY_CURRENT_USER, PChar(RootReg + '\' + RegEntryRoot));
      end;

      var Messages: string;
      if not TFile.Exists(ErrFile) then
      begin
        Messages := 'Error: Can''t find the file "' + ErrFile + '"';
      end
      else begin
        Messages := TFile.ReadAllText(ErrFile);
      end;
      Logger.Trace(Messages);
      if not BdsLogHasSuccess(Messages) then HasErrors := true;

      if HasErrors then raise Exception.Create('Failed to compile ' + ProjectFile);
    finally
      TMonitor.Exit(BDSLock);
    end;
  finally
    System.SysUtils.DeleteFile(ErrFile); //no real need to move to locked.
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
      try
        var ResinatorCommand := QuoteCmd(ResinatorPath) + ' -v -fo resource_empty.res resource_empty.rc ';
        ExecuteCommand(ResinatorCommand, TempPath);
      finally
        TryDeleteFileAndRemoveParentFolderIfEmpty(Config.Folders.LockedFilesFolder, TPath.Combine(TempPath, 'resource_empty.rc'));
        TryDeleteFileAndRemoveParentFolderIfEmpty(Config.Folders.LockedFilesFolder, TPath.Combine(TempPath, 'resource_empty.res'));
      end;
    finally
      Lock.Release;
    end;
  finally
    Lock.Free;
  end;

end;

{$ELSE}
interface
implementation
{$ENDIF}

end.

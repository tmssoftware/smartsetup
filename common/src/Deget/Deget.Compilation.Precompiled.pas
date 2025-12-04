unit Deget.Compilation.Precompiled;

interface

uses
  Deget.CoreTypes,
  Deget.Nullable,
  Deget.IDETypes,
  Deget.Compilation;

type
  TPrecompiledCompilationSettings = class(TCompilationSettings)
  private
    FIDEName: TIDEName;
    FPlatform: TPlatform;
    FPrecompiledSource: string;
    FHasMultiIDEPackages: boolean;
  public
    property IDEName: TIDEName read FIDEName write FIDEName;
    property Platform: TPlatform read FPlatform write FPlatform;
    property PrecompiledSource: string read FPrecompiledSource write FPrecompiledSource;
    property HasMultiIDEPackages: boolean read FHasMultiIDEPackages write FHasMultiIDEPackages;
  end;

  TPrecompiledCompiler = class
  private
    class var PrecompiledLock: TObject;
  private
    class constructor Create;
    class destructor Destroy;
  public
    procedure DoCompile(const ProjectFile: string; IDEName: TIDEName; Settings: TPrecompiledCompilationSettings);
  public
    class procedure Build(const ProjectFile: string; IDEName: TIDEName; Settings: TPrecompiledCompilationSettings); static;
    class function SupportsPlatform(const RootFolder: string; const IDEName: TIDEName; const Platform: TPlatform): boolean; static;
  end;

implementation

uses
  Classes,
  SysUtils,
  IOUtils,
  UTmsBuildSystemUtils,
  Commands.GlobalConfig,
  UOSFileLinks,
  UMultiLogger;

{ TPrecompiledCompiler }

class procedure TPrecompiledCompiler.Build(const ProjectFile: string;
  IDEName: TIDEName; Settings: TPrecompiledCompilationSettings);
var
  Compiler: TPrecompiledCompiler;
begin
  Compiler := TPrecompiledCompiler.Create;
  try
    Compiler.DoCompile(ProjectFile, IDEName, Settings);
  finally
    Compiler.Free;
  end;
end;

class constructor TPrecompiledCompiler.Create;
begin
  PrecompiledLock := TObject.Create;
end;

class destructor TPrecompiledCompiler.Destroy;
begin
  PrecompiledLock.Free;
end;

procedure TPrecompiledCompiler.DoCompile(const ProjectFile: string;
  IDEName: TIDEName; Settings: TPrecompiledCompilationSettings);
begin
  var LocalTargetConfig := '';
  if Settings.TargetConfig.IsNull then
  begin
    LocalTargetConfig := 'Release';
  end
  else
    LocalTargetConfig := Settings.TargetConfig.Value;

  var SourceFolder := TPath.Combine(Settings.PrecompiledSource, DelphiSuffixes[Settings.IDEName], PlatformMacroString(Settings.IDEName, Settings.Platform) , LocalTargetConfig);
  var BaseProjectFolder := TPath.GetDirectoryName(ProjectFile);
  if Settings.HasMultiIDEPackages then BaseProjectFolder := TPath.Combine(BaseProjectFolder, DelphiProductVersion[IDEName]);

  var DestFolder := TPath.Combine(BaseProjectFolder, PlatformMacroString(Settings.IDEName, Settings.Platform) , LocalTargetConfig);

  //This will be called by different packages, and in all of them we just
  //hard-link the full thing. So if a file has been hardlinked already, we assume we have already been called and exit.
  //We protect this with a lock since different threads might be wanting to link the same folder.
  TMonitor.Enter(PrecompiledLock);
  try
    if TDirectory.Exists(DestFolder) then exit;
    TDirectory_CreateDirectory(DestFolder);
  finally
    TMonitor.Exit(PrecompiledLock);
  end;

  ScanFiles(SourceFolder, ['*'], [], ['*'], [],
    procedure(FileName, RelPath: string)
    begin
      var DestFileName := TPath.Combine(DestFolder, RelPath);
      var FullDestFolderName := TPath.GetDirectoryName(DestFileName);
      if not TDirectory.Exists(FullDestFolderName) then TDirectory_CreateDirectory(FullDestFolderName);
      CreateFileLink(Config.Folders.LockedFilesFolder, FileName, DestFileName, TFileLinkType.HardLink, Logger.TraceProc()); //always use hardlinks here.
    end,
    true);

end;

class function TPrecompiledCompiler.SupportsPlatform(const RootFolder: string; const IDEName: TIDEName; const Platform: TPlatform): boolean;
begin
  var PrecompiledSource := TPath.Combine(RootFolder, 'BinPackages');
  var SourceFolder := TPath.Combine(PrecompiledSource, DelphiSuffixes[IDEName], PlatformMacroString(IDEName, Platform));
  Result := TDirectory.Exists(SourceFolder);
end;
end.

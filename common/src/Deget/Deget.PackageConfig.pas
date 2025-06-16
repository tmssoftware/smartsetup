unit Deget.PackageConfig;

{$IFDEF MSWINDOWS}

interface

uses
  SysUtils, IOUtils, Deget.CoreTypes, Deget.IDETypes, Deget.DelphiInfo, Deget.DpkFileFormat,
  Deget.IDEInfo, Deget.Filer.ProjectFactory;

type
  IDelphiPackageInfo = interface
  ['{E5DE3752-D03E-490F-A97C-7E5D6E748750}']
    function PlatformInfo: IDelphiPlatformInfo;

    // returns full file name of generated binary package file.
    // note that this function doesn't work for now with bdscommondir
    // so if BplOutputDir is empty, it will NOT return the correct place where bpl is placed
    // Example of result values:
    //   C:\product\packages\bpl\win32\package1.bpl
    //   C:\product\packages\bpl\osx32\package1.so
    function BinaryPackageFileName(const ABuildConfig: string): string;
    function BinaryTempPackageFileName(const ProductId, ParallelFolder, ABuildConfig: string): string;

    // Returns the directory where dcu files are located (or should be generated to), relative to install dir
    //  for the specified platform, delphi version and build config
    // if basedir is specified, it will be combined with result
    // Example of result values:
    //   "C:\product\packages\dxe\win32\release" (for Basedir = "C:\product"
    //   "packages\dxe\win32\release" (for BaseDir = "")
    function ExpandedDcuOutputDir(const ABuildConfig: string): string;
    function ExpandedTempDcuOutputDir(const ProductId, ParallelFolder, ABuildConfig: string): string;

    function UnexpandedExeOutputDir(const ABuildConfig: string): string;
    function ExpandedExeOutputDir(const ABuildConfig: string): string;
    function UnexpandedTempExeOutputDir(const ABuildConfig: string): string;
    function ExpandedTempExeOutputDir(const ProductId, ParallelFolder, ABuildConfig: string): string;

    // returns the location of package file name (.dproj or .dpk) for the specified delphi version
    // Value is relative to product install dir
    // If base dir is specified, then it will combine base dir to provide full location
    // Example: for basedir "C:\product" and IDEName=delphixe, return would be "C:\product\packages\dxe\package1.dproj';
    //   without base dir, return out be "packages\dxe\package1.dproj"
    function OrigPackageFileName: string;

    // when building in parallel, we might build to a different temp package than the one in OrigPackageFileName.
    function TempPackageFileName(const ProductId, ParallelFolder, BuildConfig: string): string;

    // returns the directory where package files should be located, relative to a base directory (install dir)
    // and for the specified delphi version
    // Example: for basedir "C:\product" and IDEName=delphixe, return would be "C:\product\packages\dxe';
    // Example: for basedir empty, return would be "packages\dxe".
    function OrigPackageDirectory: string;

    // For parallel building. We might build into a temporary folder to avoid
    // different threads creating the same files in the Package folder.
    function TempPackageDirectory(const ProductId, ParallelFolder, BuildConfig: string): string;

    // returns the name of C++ static library file generated for the specified platform.
    // Example: it will return "product.lib" (win32), "product.a" (win64) or "libproduct.a" (ios)
    function ExpandedLibFileName(const ABuildConfig: string): string;

    // Returns a list of semi-comma-separated strings with folder names. The folder names is a consolidation of all directories
    // containing source code, extracted from the package .dpk file.
    function SourcePathFromDpk: string;

    // Returns the full file name of generated .dcp file, based on platform, delphi version and build config
    // if base dir is specified, it will be combined
    // Example of result values:
    //   "C:\product\packages\dxe\win32\package1.dcp" (basedir = "C:\product")
    //   "packages\dxe\win32\package1.dcp" (basedir = "")
    function ExpandedDcpFileName(const ABuildConfig: string): string;
    function ExpandedTempDcpFileName(const ProductId, ParallelFolder, ABuildConfig: string): string;

    function ExpandedCppBpiOutputDir(const ABuildConfig: string): string;
    function ExpandedCppHppOutputDir(const ABuildConfig: string): string;
    function ExpandedCppObjOutputDir(const ABuildConfig: string): string;
    function UnexpandedOutputDir: string;

    function Defines(const ABuildConfig: string): string;
    function IsPrecompiled: boolean;
  end;

  TPackageConfig = class(TInterfacedObject, IDelphiPackageInfo)
  strict private
    FPlatformInfo: IDelphiPlatformInfo;
    FBaseDir: string;
    FPackageName: string;
    FBplOutputDir: string;
    FDcpOutputDir: string;
    FDcuOutputDir: string;
    FCppHppOutputDir: string;
    FCppBpiOutputDir: string;
    FCppObjOutputDir: string;
    FOrigPackageDirectory: string;
    FPackageExtension: string;
    FIsExe: boolean;
    FLibSuffixes: TLibSuffixes;
    // replace $(Platform) and $(Config)
    function ExpandDirectoryMacros(const S, ABuildConfig: string): string;
    function ExpandConfigMacro(const S, ABuildConfig: string): string;
    function ExpandPlatformMacro(const S: string): string;
    function PackageExtension: string;
  strict
  private
    function AddExtension(const Name, Extension: string): string;
    function DpkFileName: string; protected
(*    // Indicates the directory where the bpl file associated with this package should be found
    // It's also used to update the package (.dproj) files
    // You can use macro $(Platform)
    // If the Delphi version doesn't support different platforms, "Win32" will be used for $(Platform)
    // If the Delphi version doesn't support configs, "Release" will be used for $(Config)
    // You must use relative paths
    // Paths are relative to the directory where package source code file (.dproj or .dpk) is located
    property BplOutputDir: string read FBplOutputDir;

    // Outputdir for .dcp file. If empty, indicates default Delphi DCP directory
    // Supports macros $(Platform) and $(Config). See BplOutputDir for more info.
    // You must use relative paths.
    // Paths are relative to the directory where package source code file (.dproj or .dpk) is located
    property DcpOutputDir: string read FDcpOutputDir;

    // Outputdir for .dcu file. Supports macros $(Platform) and $(Config). See BplOutputDir for more info.
    // You must use relative paths.
    // Paths are relative to the directory where package source code file (.dproj or .dpk) is located
    property DcuOutputDir: string read FDcuOutputDir;

    property CppBpiOutputDir: string read FCppBpiOutputDir;
    property CppHppOutputDir: string read FCppHppOutputDir;
    property CppObjOutputDir: string read FCppObjOutputDir; *)
  public
    { IDelphiPackageInfo }
    function PlatformInfo: IDelphiPlatformInfo;
    function ExpandedDcuOutputDir(const ABuildConfig: string): string;
    function ExpandedTempDcuOutputDir(const ProductId, ParallelFolder, ABuildConfig: string): string;
    function UnexpandedExeOutputDir(const ABuildConfig: string): string;
    function ExpandedExeOutputDir(const ABuildConfig: string): string;
    function UnexpandedTempExeOutputDir(const ABuildConfig: string): string;
    function ExpandedTempExeOutputDir(const ProductId, ParallelFolder, ABuildConfig: string): string;
    function BinaryPackageFileName(const ABuildConfig: string): string;
    function BinaryTempPackageFileName(const ProductId, ParallelFolder, ABuildConfig: string): string;
    function TempPackageFileName(const ProductId, ParallelFolder, BuildConfig: string): string;
    function OrigPackageFileName: string;
    function StaticLibraryName: string;
    function SourcePathFromDpk: string;
    function ExpandedDcpFileName(const ABuildConfig: string): string;
    function ExpandedTempDcpFileName(const ProductId, ParallelFolder, ABuildConfig: string): string;
    function ExpandedLibFileName(const ABuildConfig: string): string;

    function ExpandedCppBpiOutputDir(const ABuildConfig: string): string;
    function ExpandedCppHppOutputDir(const ABuildConfig: string): string;
    function ExpandedCppObjOutputDir(const ABuildConfig: string): string;
    function UnExpandedOutputDir: string;

    function OrigPackageDirectory: string;
    function TempPackageDirectory(const ProductId, ParallelFolder, BuildConfig: string): string;
    function IsPrecompiled: boolean;

    function ExeOutputDir(const ABuildConfig: string): string;
    function Defines(const ABuildConfig: string): string;


  public
    // Indicates if the package supports the specified platform and delphi version (if it must be compiled for the specified platform)
    function IsSupported(IDEName: TIDEName; Plat: TPlatform): boolean;
  public
    constructor Create(const APackageName, ABaseDir: string; APlatformInfo: IDelphiPlatformInfo; const PlatformFolder: string; const AIsExe: boolean; const APackageExtension: string; const ALibSuffixes: TLibSuffixes);

    property PackageName: string read FPackageName;
  end;

  function TempProjectDirectory(const ProductId, ParallelFolder, BuildConfig: string; const IDEName: TIDEName; const Platform: TPlatform): string;

implementation

uses
  Deget.IDEUtils,
  Deget.Filer.DprojFile,
  Deget.Filer.Types,
  Deget.Nullable,
  UMultiLogger;

function TempProjectDirectory(const ProductId, ParallelFolder, BuildConfig: string; const IDEName: TIDEName; const Platform: TPlatform): string;
begin
  Result := Format('%s_%s_%s_%s', [ProductId, DelphiSuffixes[IDEName], PlatformID[Platform], BuildConfig]);
  Result := TPath.Combine(ParallelFolder, Result);
  Result := TPath.GetFullPath(Result);
end;


{ TPackageConfig }

constructor TPackageConfig.Create(const APackageName, ABaseDir: string; APlatformInfo: IDelphiPlatformInfo; const PlatformFolder: string; const AIsExe: boolean; const APackageExtension: string; const ALibSuffixes: TLibSuffixes);
begin
  inherited Create;
  FBaseDir := ABaseDir;
  FPackageName := APackageName;
  FPlatformInfo := APlatformInfo;
  // default settings for packages
  // this is for now hard coded. We have to revise and test code to see if changing to other values would work
  FBplOutputDir := '.\$(Platform)\$(Config)';
  FDcuOutputDir := '.\$(Platform)\$(Config)';
  FDcpOutputDir := FDcuOutputDir;
  FCppBpiOutputDir := FDcuOutputDir;
  FCppHppOutputDir := FDcuOutputDir;
  FCppObjOutputDir := FDcuOutputDir;

  FOrigPackageDirectory := PlatformFolder;
  FIsExe := AIsExe;
  FLibSuffixes := ALibSuffixes;
  FPackageExtension := APackageExtension;
end;

function TPackageConfig.DpkFileName: string;
begin
  Result := TPath.ChangeExtension(OrigPackageFileName, '.dpk');
end;

function TPackageConfig.Defines(const ABuildConfig: string): string;
begin
  var MetaData := TDelphiProjectFactory.GetPackageReadData(OrigPackageFileName, PlatformInfo.IDEInfo.IDEName);
  if MetaData = nil then
  begin
    exit('');
  end;

  Result := MetaData.PropertyGroups.GetValueWithInherited(PlatformInfo.PlatType, ABuildConfig, '$(DCC_Define)',
    function(Entry: TPropertyGroupEntry): Nullable<string>
    begin
      Result := Entry.Defines;
    end, ';');
end;

function TPackageConfig.ExeOutputDir(const ABuildConfig: string): string;
begin
  var MetaData := TDelphiProjectFactory.GetPackageReadData(OrigPackageFileName, PlatformInfo.IDEInfo.IDEName);
  if MetaData = nil then
  begin
    exit(FDcuOutputDir);
  end;

  var ResultNullable := MetaData.PropertyGroups.GetValue(PlatformInfo.PlatType, ABuildConfig,
    function(Entry: TPropertyGroupEntry): Nullable<string>
    begin
      Result := Entry.ExeOutputPath;
    end);

  if not ResultNullable.HasValue or (ResultNullable.Value = '') then exit(FDcuOutputDir);
  Result := ResultNullable.Value;

end;

function TPackageConfig.ExpandConfigMacro(const S, ABuildConfig: string): string;
begin
  Result := StringReplace(S, '$(Config)', ABuildConfig, [rfReplaceAll, rfIgnoreCase]);
end;

function TPackageConfig.ExpandDirectoryMacros(const S: string; const ABuildConfig: string): string;
begin
  Result := S;
  Result := ExpandConfigMacro(Result, ABuildConfig);
  Result := ExpandPlatformMacro(Result);
end;

function TPackageConfig.ExpandPlatformMacro(const S: string): string;
begin
  Result := StringReplace(S, '$(Platform)', FPlatformInfo.PlatformMacroValue, [rfReplaceAll, rfIgnoreCase]);
end;

function TPackageConfig.ExpandedCppBpiOutputDir(const ABuildConfig: string): string;
begin
  Result := TPath.Combine(OrigPackageDirectory, FCppBpiOutputDir);
  Result := ExpandDirectoryMacros(Result, ABuildConfig);
  Result := TPath.GetFullPath(Result);
end;

function TPackageConfig.ExpandedCppHppOutputDir(const ABuildConfig: string): string;
begin
  Result := TPath.Combine(OrigPackageDirectory, FCppHppOutputDir);
  Result := ExpandDirectoryMacros(Result, ABuildConfig);
  Result := TPath.GetFullPath(Result);
end;

function TPackageConfig.ExpandedCppObjOutputDir(const ABuildConfig: string): string;
begin
  Result := TPath.Combine(OrigPackageDirectory, FCppObjOutputDir);
  Result := ExpandDirectoryMacros(Result, ABuildConfig);
  Result := TPath.GetFullPath(Result);
end;

function TPackageConfig.ExpandedDcpFileName(const ABuildConfig: string): string;
begin
  // Get output locations
  Result := TPath.Combine(OrigPackageDirectory, FDcpOutputDir);
  Result := TPath.Combine(Result, AddExtension(PackageName, '.dcp'));
  Result := ExpandDirectoryMacros(Result, ABuildConfig);
  Result := TPath.GetFullPath(Result);
//  Logger.Info('Full Dcp File Name Result: %s', [Result]);
end;

function TPackageConfig.ExpandedTempDcpFileName(const ProductId, ParallelFolder, ABuildConfig: string): string;
begin
  // Get output locations
  Result := TPath.Combine(TempPackageDirectory(ProductId, ParallelFolder, ABuildConfig), FDcpOutputDir);
  Result := TPath.Combine(Result, AddExtension(PackageName, '.dcp'));
  Result := ExpandDirectoryMacros(Result, ABuildConfig);
  Result := TPath.GetFullPath(Result);
//  Logger.Info('Full Dcp File Name Result: %s', [Result]);
end;

function TPackageConfig.ExpandedDcuOutputDir(const ABuildConfig: string): string;
begin
  // Get output locations
  Result := TPath.Combine(OrigPackageDirectory, FDcuOutputDir);
  Result := ExpandDirectoryMacros(Result, ABuildConfig);
  Result := TPath.GetFullPath(Result);
//  Logger.Info('Full Dcu Output Dir Result: %s', [Result]);
end;

function TPackageConfig.ExpandedTempDcuOutputDir(const ProductId, ParallelFolder, ABuildConfig: string): string;
begin
  // Get output locations
  Result := TPath.Combine(TempPackageDirectory(ProductId, ParallelFolder, ABuildConfig), FDcuOutputDir);
  Result := ExpandDirectoryMacros(Result, ABuildConfig);
  Result := TPath.GetFullPath(Result);
//  Logger.Info('Full Dcu Output Dir Result: %s', [Result]);
end;

function TPackageConfig.UnexpandedExeOutputDir(const ABuildConfig: string): string;
begin
  // Get output locations
  Result := ExeOutputDir(ABuildConfig);
end;

function TPackageConfig.ExpandedExeOutputDir(const ABuildConfig: string): string;
begin
  // Get output locations
  Result := TPath.Combine(OrigPackageDirectory, ExeOutputDir(ABuildConfig));
  Result := ExpandDirectoryMacros(Result, ABuildConfig);
  Result := TPath.GetFullPath(Result);
//  Logger.Info('Full Dcu Output Dir Result: %s', [Result]);
end;

function TPackageConfig.UnexpandedTempExeOutputDir(const ABuildConfig: string): string;
begin
  //For the temp folder, we don't care about where the user set it to be.
  //We will then move this to the real folder anyway.
  Result := '.\bin\$(Platform)\$(Config)';

  // Delphi XE and below does not understand the "$(Platform)"  macro
  if FPlatformInfo.IDEInfo.IDEName <= TIDEName.delphixe then
    Result := ExpandPlatformMacro(Result);

end;

function TPackageConfig.ExpandedTempExeOutputDir(const ProductId, ParallelFolder, ABuildConfig: string): string;
begin
  // Get output locations
  Result := TPath.Combine(TempPackageDirectory(ProductId, ParallelFolder, ABuildConfig), UnexpandedTempExeOutputDir(ABuildConfig));
  Result := ExpandDirectoryMacros(Result, ABuildConfig);
  Result := TPath.GetFullPath(Result);
//  Logger.Info('Full Dcu Output Dir Result: %s', [Result]);
end;

function TPackageConfig.ExpandedLibFileName(const ABuildConfig: string): string;
begin
  // Get output locations
  Result := TPath.Combine(OrigPackageDirectory, FCppObjOutputDir);
  Result := TPath.Combine(Result, StaticLibraryName);
  Result := ExpandDirectoryMacros(Result, ABuildConfig);
  Result := TPath.GetFullPath(Result);
//  Logger.Info('Full Dcp File Name Result: %s', [Result]);
end;

function TPackageConfig.AddExtension(const Name, Extension: string): string;
begin
  Result := Name + Extension;
end;

function TPackageConfig.BinaryPackageFileName(const ABuildConfig: string): string;
begin
  // Get output locations
  if FIsExe then Result := TPath.Combine(OrigPackageDirectory, ExeOutputDir(ABuildConfig))
    else Result := TPath.Combine(OrigPackageDirectory, FBplOutputDir);

  Result := TPath.Combine(Result, FPlatformInfo.BinaryPackageName(PackageName, True, FIsExe, FLibSuffixes));
  Result := ExpandDirectoryMacros(Result, ABuildConfig);
  Result := TPath.GetFullPath(Result);
//  Logger.Info('Binary package file location: %s', [Result]);
end;

function TPackageConfig.BinaryTempPackageFileName(const ProductId, ParallelFolder, ABuildConfig: string): string;
begin
  // Get output locations
  if FIsExe then Result := TPath.Combine(TempPackageDirectory(ProductId, ParallelFolder, ABuildConfig), UnexpandedTempExeOutputDir(ABuildConfig))
    else Result := TPath.Combine(TempPackageDirectory(ProductId, ParallelFolder, ABuildConfig), FBplOutputDir);
  Result := TPath.Combine(Result, FPlatformInfo.BinaryPackageName(PackageName, True, FIsExe, FLibSuffixes));
  Result := ExpandDirectoryMacros(Result, ABuildConfig);
  Result := TPath.GetFullPath(Result);
//  Logger.Info('Binary package file location: %s', [Result]);
end;


function TPackageConfig.OrigPackageDirectory: string;
begin
  Result := FOrigPackageDirectory;
end;

function TPackageConfig.TempPackageDirectory(const ProductId, ParallelFolder, BuildConfig: string): string;
begin
  Result := TempProjectDirectory(ProductId, ParallelFolder, BuildConfig, FPlatformInfo.IDEInfo.IDEName, PlatformInfo.PlatType);
end;

function TPackageConfig.TempPackageFileName(const ProductId, ParallelFolder, BuildConfig: string): string;
begin
  Result := AddExtension(PackageName, PackageExtension);

  Result := TPath.Combine(TempPackageDirectory(ProductId, ParallelFolder, BuildConfig), Result);
  Result := TPath.GetFullPath(Result);
end;

function TPackageConfig.UnExpandedOutputDir: string;
begin
  Result := FDcuOutputDir;

  // Delphi XE and below does not understand the "$(Platform)"  macro
  if FPlatformInfo.IDEInfo.IDEName <= TIDEName.delphixe then
    Result := ExpandPlatformMacro(Result);
end;

function TPackageConfig.OrigPackageFileName: string;
begin
  Result := AddExtension(PackageName, PackageExtension);

  Result := TPath.Combine(OrigPackageDirectory, Result);
  Result := TPath.GetFullPath(Result);
end;

function TPackageConfig.PackageExtension: string;
begin
  Result := FPackageExtension;
end;

function TPackageConfig.PlatformInfo: IDelphiPlatformInfo;
begin
  Result := FPlatformInfo;
end;

function TPackageConfig.SourcePathFromDpk: string;
begin
  Result := '';
  if not TFile.Exists(DpkFileName) then exit;

  for var FileName in GetFilesFromDpk(DpkFileName, OrigPackageDirectory, false) do
    Result := AddPaths(Result, TPath.GetFullPath(TPath.GetDirectoryName(FileName)));
end;

function TPackageConfig.StaticLibraryName: string;
begin
  case PlatformInfo.PlatType of
    Win32Intel: Result := Format('%s.lib', [PackageName]);
    Win64xIntel: Result := Format('%s.lib', [PackageName]);
    Win64Intel, macOS32Intel, macOS64Intel: Result := Format('%s.a', [PackageName]);
    iOSDevice32, Android32, iOSDevice64, Linux64, Android64:
      if PlatformInfo.IDEInfo.IDEName >= delphixe6 then
        Result := Format('lib%s.a', [PackageName])
      else
        Result := Format('%s.a', [PackageName]);
  else
    Result := '';
  end;
end;

function TPackageConfig.IsPrecompiled: boolean;
begin
  Result := SameText(TPath.GetExtension(OrigPackageFileName), BinprojExtension);
end;

function TPackageConfig.IsSupported(IDEName: TIDEName; Plat: TPlatform): boolean;
begin
  Result := True;
end;

{$ELSE}
interface
implementation
{$ENDIF}

end.

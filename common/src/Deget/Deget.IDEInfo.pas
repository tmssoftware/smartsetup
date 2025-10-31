unit Deget.IDEInfo;

interface

uses
  Deget.CoreTypes,
  Deget.IDETypes;

type
  IDelphiIDEInfo = interface;

  TDelphiPathType = (ptLibraryPath, ptBrowsingPath, ptDebugDcuPath,
                     ptCppLibraryPath, ptCppIncludePath, ptCppBrowsingPath,
                     ptCppClang32LibraryPath, ptCppClang32IncludePath, ptCppClang32BrowsingPath
                    );

  TEnvVar = record
    Name: string;
    Value: string;
  end;


  IDelphiPlatformInfo = interface
  ['{DF1393FF-A263-4B62-92B7-EE6CA86B5D34}']
    function PlatType: TPlatform;

    function IDEInfo: IDelphiIDEInfo;

    // retrieves the contents for the specified path configured in the IDE
    // (ex. Delphi library path, CPP Include Path, etc.
    function GetIDEPath(PathType: TDelphiPathType): string;

    // sets the contents for the specified path configured in the IDE
    // (ex. Delphi library path, CPP Include Path, etc.
    procedure SetIDEPath(PathType: TDelphiPathType; const Value: string);

    // Add a new path (PathToAdd) to the specified IDE path configuration
    // If the path already exists, it will NOT add another one
    procedure AddIDEPath(PathType: TDelphiPathType; const PathsToAdd: string);

    // Removes a path (PathToRemove) from the IDE path configuration
    // if the path doesn't exist, it will do nothing
    procedure RemoveIDEPath(PathType: TDelphiPathType; const PathsToRemove: string);

    // Returns the value of "$(Platform)" macro
    function PlatformMacroValue: string;

    // Returns the value of "$(ProductVersion)" macro
    function ProductVersionMacroValue: string;

    // Returns the name of generated binary package file name given the package base name
    // it will automatically add the suffix if AddLibSuffix is true.
    // Example: base name is "aurelius", and function will return aurelius210.bpl for windows in XE7
    // or bplaurelius220.dylib for MacOS in XE8
    function BinaryPackageName(const BaseName: string; AddLibSuffix: Boolean; const IsExe: boolean; const NonStandardSuffixes: TLibSuffixes): string;

    // Returns Delphi lib directory. Note: this is NOT the root lib dir, but instead
    // the directory which contains library files (dcp/dcu, etc.), for the specified platform,
    // using release config. Can be used for manual compilation
    // Example: "C:\Program Files (x86)\Embarcadero\Studio\15.0\Lib\win32\release"
    function LibDir: string;

    // Returns Delphi Redist directory for the specified platform.
    // Example: "C:\Program Files (x86)\Embarcadero\Studio\22.0\Redist\osx64
    function RedistDir: string;

    // Same as LibDir but using Debug binaries (they are different when compiling for iOS/Android
    function DebugLibDir: string;

    // Returns the name of MSBuild platform Build used to build for that platform
    function BuildName: string;
  end;

  IDelphiIDEInfo = interface
  ['{A581B09B-A3E5-47B7-BCE2-D00EE0A633FC}']
    function IDEName: TIDEName;
    function GetPlatform(PlatType: TPlatform): IDelphiPlatformInfo;
    procedure RegisterPackage(const BinaryFileName: string; const IDEPlatform: TPlatform; const Description: string = '');
    procedure UnregisterPackage(const BinaryFileName: string; const IDEPlatform: TPlatform);


    function GetPathOverride: string;

    // Modifies IDE PATH environment variable override by adding the Path to the list of paths (at the beginning by default)
    procedure AddFolderToPathOverride(const PathsToAdd: string; AddToBeginning: Boolean = True);

    // Modifies IDE PATH environment variable override by removing the Path from the list of paths
    procedure RemoveFolderFromPathOverride(const PathsToRemove: string);

    // Returns the full file name of rsvars.bat file
    // Example: "C:\Program Files (x86)\Embarcadero\Studio\15.0\bin\rsvars.bat"
    function RsvarsFile: string;

    // Returns the location of Delphi Root Directory (install directory)
    // Example: "C:\Program Files (x86)\Embarcadero\Studio\15.0"
    function RootDir: string;

    //True if we aren't reading the compiler paths from the registry.
    function HasCustomCompilerPaths: boolean;

    // Returns the full file name of dcc32.exe file
    // Example: "C:\Program Files (x86)\Embarcadero\Studio\15.0\bin\dcc32.exe"
    function Dcc32File: string;

    // Returns the full file name of brcc32.exe file
    function Brcc32File: string;

    // Returns the full file name of bds.exe file
    function BdsFile: string;

    // Location of resinator.exe
    function ResinatorFile: string;

    procedure ForceIDEUpdate;

    function GetEnvVarOverrides: TArray<TEnvVar>;

    function BaseKey(const SubKey: string = ''): string;

  end;

implementation

end.

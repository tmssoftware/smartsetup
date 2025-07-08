unit UProjectLoaderStateMachine;
{$i ../../tmssetup.inc}

interface
uses BBClasses, UProjectDefinition, SysUtils, Generics.Collections, UCoreTypes, Deget.CoreTypes;
type
  TSectionDef = class(TSection)
  protected
    Project: TProjectDefinition;
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
  end;

  TMainSectionDef = class(TSectionDef)
  public
    constructor Create(const aProject: TProjectDefinition);
    class function SectionNameStatic: string; override;
    function VersionNumberGreaterThanApp(const s: string; const ErrorInfo: TErrorInfo): boolean;
  end;

  TApplicationSectionDef = class(TSectionDef)
  private
    function ReadVersionFile(const FileName: string; const ErrorInfo: TErrorInfo): string;
    function GetVCSProtocol(const s: string;
      const ErrorInfo: TErrorInfo): string;
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
    class function SectionNameStatic: string; override;
  end;

  TNotSupportedFrameworksSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
    class function SectionNameStatic: string; override;

    procedure AddDelphiVersion(const dv: TIDEName);
    function Capture(const dv: TIDEName): TAction;
  end;


  TSupportedFrameworksSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
    class function SectionNameStatic: string; override;

  end;

  TFrameworkVersionSectionDef = class(TSectionDef)
  var
    Framework: string;
    FrameworkDef: TFrameworkDefinition;
  private
    function ListIDENames: string;
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition; const aFrameworkName: string);
    class function SectionNameStatic: string; override;
    function SectionName: string; override;
    function GetIDEName(const value: string; const ErrorInfo: TErrorInfo): TIDEName;
  end;

  TPackagesSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
    function Capture(const fr: TFramework): TAction;

    class function SectionNameStatic: string; override;
  end;

  TPackageOptionsSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
    class function SectionNameStatic: string; override;
  end;

  TPackageFoldersSectionDef = class(TSectionDef)
  private
    function Capture(const dv: TIDEName): TAction;
    function CapturePlus(const dv: TIDEName): TAction;
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);

    class function SectionNameStatic: string; override;
  end;

  TLibSuffixesSectionDef = class(TSectionDef)
  private
    function Capture(const dv: TIDEName): TAction;
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);

    class function SectionNameStatic: string; override;
  end;

  TPackageExtraDefinesSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);

    class function SectionNameStatic: string; override;
  end;

  THelpSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);

    class function SectionNameStatic: string; override;
  end;

  TDependenciesSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);

    class function SectionNameStatic: string; override;
  end;

  TBuildingSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);

    class function SectionNameStatic: string; override;
  end;

  TDefinesSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);

    class function SectionNameStatic: string; override;
  end;

  TSupportedDefinesSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);

    class function SectionNameStatic: string; override;
  end;

  TRegistryKeysSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);

    class function SectionNameStatic: string; override;
  end;

  TRegistryEntrySectionDef = class(TSectionDef)
  private
    KeyName: string;
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition; const aKeyName: string);
    class function SectionNameStatic: string; override;
    function SectionName: string; override;
  end;

  TRegistryEntryValueSectionDef = class (TSectionDef)
  private
  const
    RegistryTypes: Array[TRegistryEntryType] of string = ('String', 'DWORD');
    function ListValueTypes: string;
    procedure AddNewEntry(const aKeyName, aValueName: string);
  var
    Entry: TRegistryEntry;
    function GetValueType(const value: string;
      const ErrorInfo: TErrorInfo): TRegistryEntryType;
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition; const aKeyName: string);

    class function SectionNameStatic: string; override;

  end;


  TSectionFrameworkPlatformsDef = class(TSectionDef)
  private
    Framework: TFrameworkDefinition;
    procedure AddPlatform(const dp: TPlatform; const value: boolean);
    function Capture(const dp: TPlatform): TAction;
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition; const aFramework: TFrameworkDefinition);

    class function SectionNameStatic: string; override;
  end;

  TSectionFrameworkDependenciesDef = class(TSectionDef)
  private
    Framework: TFrameworkDefinition;
    procedure AddDependency(const depId: string; const depDescription: string);
    function DependencyAlreadyAdded(const depId: string): boolean;
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition; const aFramework: TFrameworkDefinition);

    class function SectionNameStatic: string; override;
  end;

  TPackageDefinitionsSectionDef = class(TSectionDef)
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
    class function SectionNameStatic: string; override;
  private
    function FindPackage(const Name: string;
      const Packages: TListOfPackages): TPackage;
  end;

  TPackageDefinitionSectionDef = class(TSectionDef)
  private
    FName: string;
    FPackage: TPackage;

    function ValidateDelphiFrameworkType(const value: string; const ErrorInfo: TErrorInfo): string;
    function GetRequires(const value: string;
      const ErrorInfo: TErrorInfo): TArray<string>;
    function GetMasks(const Masks: string): TArray<string>;
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition; const aPackage: TPackage);
    class function SectionNameStatic: string; override;
    function SectionName: string; override;

  end;

  TPathsSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
    class function SectionNameStatic: string; override;
  end;

  TExtraLibraryPathSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
    class function SectionNameStatic: string; override;
  end;

  TBuildOnlyLibraryPathSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
    class function SectionNameStatic: string; override;
  end;

  TExtraBrowsingPathSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
    class function SectionNameStatic: string; override;
  end;

  TExtraDebugDCUPathSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
    class function SectionNameStatic: string; override;
  end;

  TWebCorePathSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
    class function SectionNameStatic: string; override;
  end;

  TSearchPathsToPreserveSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
    class function SectionNameStatic: string; override;
  end;


  TLinksSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
    class function SectionNameStatic: string; override;
  end;

  TLinkSectionDef = class(TSectionDef)
  private
    function GetShortcutType(const value: string;
      const ErrorInfo: TErrorInfo): TShortcutType;
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
    class function SectionNameStatic: string; override;
  end;

  TFileLinksSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
    class function SectionNameStatic: string; override;
  end;

  TFileLinkSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
    class function SectionNameStatic: string; override;
  end;

  TOtherVersionsSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition);
    class function SectionNameStatic: string; override;
  end;

  TStandardFilesSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition;
      const AddFolder, SetIncludeFolderMask, SetExcludeFolderMask, SetIncludeFileMask, SetExcludeFileMask: TProc<string>;
      const SetRecursive: TProc<boolean>);
    class function SectionNameStatic: string; override;
  end;

  TStandardFilesSourceSectionDef = class(TSectionDef)
  public
    constructor Create(const aParent: TSection; const aProject: TProjectDefinition;
      const AddFolder, SetIncludeFolderMask, SetExcludeFolderMask, SetIncludeFileMask, SetExcludeFileMask: TProc<string>;
      const SetRecursive: TProc<boolean>);
    class function SectionNameStatic: string; override;
  end;

implementation
uses Classes, UTmsBuildSystemUtils, IOUtils, Deget.Version;

{ TSectionDef }

constructor TSectionDef.Create(const aParent: TSection; const aProject: TProjectDefinition);
begin
  inherited Create(aParent);
  Project := aProject;
end;

{ TMainSectionDef }

constructor TMainSectionDef.Create(const aProject: TProjectDefinition);
const
  {$i ../../../Version.inc}

begin
  inherited Create(nil, aProject);
  ChildSections.Add(TApplicationSectionDef.SectionNameStatic, TApplicationSectionDef.Create(Self, aProject));
  ChildSections.Add(TSupportedFrameworksSectionDef.SectionNameStatic, TSupportedFrameworksSectionDef.Create(Self, aProject));
  ChildSections.Add(TPackagesSectionDef.SectionNameStatic, TPackagesSectionDef.Create(Self, aProject));
  ChildSections.Add(TPackageOptionsSectionDef.SectionNameStatic, TPackageOptionsSectionDef.Create(Self, aProject));
  ChildSections.Add(TPackageDefinitionsSectionDef.SectionNameStatic, TPackageDefinitionsSectionDef.Create(Self, aProject));
  ChildSections.Add(THelpSectionDef.SectionNameStatic, THelpSectionDef.Create(Self, aProject));
  ChildSections.Add(TDependenciesSectionDef.SectionNameStatic, TDependenciesSectionDef.Create(Self, aProject));
  ChildSections.Add(TBuildingSectionDef.SectionNameStatic, TBuildingSectionDef.Create(Self, aProject));
  ChildSections.Add(TDefinesSectionDef.SectionNameStatic, TDefinesSectionDef.Create(Self, aProject));
  ChildSections.Add(TRegistryKeysSectionDef.SectionNameStatic, TRegistryKeysSectionDef.Create(Self, aProject));
  ChildSections.Add(TPathsSectionDef.SectionNameStatic, TPathsSectionDef.Create(Self, aProject));
  ChildSections.Add(TLinksSectionDef.SectionNameStatic, TLinksSectionDef.Create(Self, aProject));
  ChildSections.Add(TFileLinksSectionDef.SectionNameStatic, TFileLinksSectionDef.Create(Self, aProject));
  ChildSections.Add(TOtherVersionsSectionDef.SectionNameStatic, TOtherVersionsSectionDef.Create(Self, aProject));

  Actions := TListOfActions.Create;
  Actions.Add('minimum required tmsbuild version', procedure(value: string; ErrorInfo: TErrorInfo)
    begin
      if (VersionNumberGreaterThanApp(value, ErrorInfo)) then raise Exception.Create('Project "'
          + aProject.FullPath + '" requires version ' + value + ' of SmartSetup. The current version is ' + TMSVersion + '. Please update SmartSetup to the latest version and retry.');
    end);

end;

class function TMainSectionDef.SectionNameStatic: string;
begin
  Result := 'root';
end;


function TMainSectionDef.VersionNumberGreaterThanApp(const s: string;
  const ErrorInfo: TErrorInfo): boolean;
const
  {$i ../../../Version.inc}
begin
  Result := TVersion(s) > TVersion(TMSVersion);
end;

{ TApplicationSectionDef }

procedure ValidateId(const Value: string);
begin
  for var v in Value do
  begin
    if (v = '.') or (v = '_') or (v = '-') then continue;
    if (v >= '0') and ( v <= '9') then continue;
    if (v >= 'A') and ( v <= 'Z') then continue;
    if (v >= 'a') and ( v <= 'z') then continue;
    raise Exception.Create('The Project Id "' + Value + '" is invalid. It contains characters that are not numbers, letters or basic punctuation.');
  end;

  if Value.EndsWith('.') then raise Exception.Create('The Project Id "' + Value + '" is invalid. It must not end with a dot');
  if Value.StartsWith('.') then raise Exception.Create('The Project Id "' + Value + '" is invalid. It must not start with a dot');

end;

constructor TApplicationSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);

  Actions := TListOfActions.Create;
  Actions.Add('id', procedure(value: string; ErrorInfo: TErrorInfo) begin ValidateId(value); Project.Application.Id := value; end);
  Actions.Add('name', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.Application.Name := value; end);
  Actions.Add('description', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.Application.Description := value; end);
  Actions.Add('copyright', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.Application.Copyright := value; end);
  Actions.Add('url', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.Application.Url := value; end);
  Actions.Add('docs', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.Application.Docs := value; end);
  Actions.Add('version file', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.Application.Version := ReadVersionFile(value, ErrorInfo); end);
  Actions.Add('company name', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.Application.CompanyName := value; end);
  Actions.Add('can add source code to library path', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.Application.CanAddSourceCodeToLibraryPath := GetBool(value, ErrorInfo); end);
  Actions.Add('vcs protocol', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.Application.VCSProtocol := GetVCSProtocol(value, ErrorInfo); end);
end;

function TApplicationSectionDef.GetVCSProtocol(const s: string;
  const ErrorInfo: TErrorInfo): string;
begin
  if SameText(s, 'svn') then exit('svn');
  if (s = '') or SameText(s, 'git') then exit('git');
  raise Exception.Create('"' + s + '" is not a valid VCS Protocol value. It must be "git" or "svn". ' + ErrorInfo.ToString);
end;

function TApplicationSectionDef.ReadVersionFile(const FileName: string;
  const ErrorInfo: TErrorInfo): string;
begin
  if (FileName = '') or (ErrorInfo.IgnoreOtherFiles) then exit('');
  var FullFileName := CombinePath(Project.RootFolder, FileName);
  if not TFile.Exists(FullFileName) then raise Exception.Create('The version file "' + FullFileName + '" doesn''t exist. ' + ErrorInfo.ToString);
  var Lines := TFile.ReadAllLines(FullFileName);
  if Length(Lines) < 1 then raise Exception.Create('The version file "' + FullFileName + '" is empty. ' + ErrorInfo.ToString);
  var colon := Lines[0].IndexOf(':');
  if (colon < 0) or (colon = Lines[0].Length - 1) then raise Exception.Create('The version file "' + FullFileName + '" is invalid. It should have a ":" in line 1, and after that the version number. ' + ErrorInfo.ToString);

  Result := Lines[0].Substring(colon + 1).Trim;
end;

class function TApplicationSectionDef.SectionNameStatic: string;
begin
  Result := 'application';
end;


{ TSupportedFrameworksSectionDef }

constructor TSupportedFrameworksSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  ChildSectionAction :=
    function(Name: string; ErrorInfo: TErrorInfo): TSection
    begin
      if ChildSections.TryGetValue(Name, Result) then exit;
      Result := TFrameworkVersionSectionDef.Create(Self, aProject, Name);
      ChildSections.Add(Name, Result);
    end;

  ChildSections.Add(TNotSupportedFrameworksSectionDef.SectionNameStatic, TNotSupportedFrameworksSectionDef.Create(Self, aProject));

end;

class function TSupportedFrameworksSectionDef.SectionNameStatic: string;
begin
  Result := 'supported frameworks';
end;

{ TPackagesSectionDef }

function TPackagesSectionDef.Capture(const fr: TFramework): TAction;
begin
  Result := procedure(value: string; ErrorInfo: TErrorInfo) begin Project.Packages.Last.Frameworks := Project.Packages.Last.Frameworks + [fr]; end;
end;

constructor TPackagesSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  Duplicated := TDictionary<string, boolean>.Create;

  ArrayMainAction := procedure (name, value: string; ErrorInfo: TErrorInfo)
    begin
      Project.Packages.Add(TPackage.Create(name));
      for var fr := Low(TFramework) to High(TFramework) do
      begin
        var FrameworkName := Project.GetFrameworkName(fr);
        if FrameworkName = '' then continue;
        ArrayActions.AddOrSetValue(FrameworkName, Capture(fr));
      end;

    end;
  ArrayActions := TListOfActions.Create;
  ArrayActions.Add('design', procedure (value: string; ErrorInfo: TErrorInfo) begin Project.Packages.Last.IsDesign := true; end);
  ArrayActions.Add('runtime', procedure (value: string; ErrorInfo: TErrorInfo) begin Project.Packages.Last.IsRuntime := true; end);
  ArrayActions.Add('exe', procedure (value: string; ErrorInfo: TErrorInfo) begin Project.Packages.Last.PackageType := TPackageType.Exe; end);
end;


class function TPackagesSectionDef.SectionNameStatic: string;
begin
  Result := 'packages';
end;

{ TPackageOptionsSectionDef }

constructor TPackageOptionsSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  ChildSections.Add(TPackageFoldersSectionDef.SectionNameStatic, TPackageFoldersSectionDef.Create(Self, aProject));
  ChildSections.Add(TLibSuffixesSectionDef.SectionNameStatic, TLibSuffixesSectionDef.Create(Self, aProject));
  ChildSections.Add(TPackageExtraDefinesSectionDef.SectionNameStatic, TPackageExtraDefinesSectionDef.Create(Self, aProject));
  Actions := TListOfActions.Create;
  Actions.Add('ignore dproj platforms', procedure(value: string; ErrorInfo: TErrorInfo)
    begin
      Project.IgnoreDprojPlatforms := GetBoolEx(value, ErrorInfo);
    end);
  Actions.Add('root package folder', procedure(value: string; ErrorInfo: TErrorInfo)
    begin
      Project.RootPackageFolder := value;
    end);
  Actions.Add('add libsuffix', procedure(value: string; ErrorInfo: TErrorInfo)
    begin
      Project.AddLibSuffix := GetBoolEx(value, ErrorInfo);
    end);

end;



class function TPackageOptionsSectionDef.SectionNameStatic: string;
begin
  Result := 'package options';
end;


{ TPackageFoldersSectionDef }

function TPackageFoldersSectionDef.Capture(const dv: TIDEName): TAction;
begin
  Result := procedure(value: string; ErrorInfo: TErrorInfo)
    begin
      Project.SetPackageFolders(dv, value);
    end;
end;

function TPackageFoldersSectionDef.CapturePlus(const dv: TIDEName): TAction;
begin
  Result := procedure(value: string; ErrorInfo: TErrorInfo)
    begin
      for var dvi := dv to High(TIDEName) do
      begin
        Project.SetPackageFolders(dvi, value);
      end;
    end;
end;

constructor TPackageFoldersSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  Actions := TListOfActions.Create;
  for var dv := Low(TIDEName) to High(TIDEName) do
  begin
    Actions.Add(IDEId[dv], Capture(dv));
    Actions.Add(IDEId[dv] + '+', CapturePlus(dv));
  end;
end;


class function TPackageFoldersSectionDef.SectionNameStatic: string;
begin
  Result := 'package folders';
end;

{ TLibSuffixesSectionDef }

function TLibSuffixesSectionDef.Capture(const dv: TIDEName): TAction;
begin
  Result := procedure(value: string; ErrorInfo: TErrorInfo)
    begin
      Project.SetLibSuffixes(dv, value);
    end;
end;

constructor TLibSuffixesSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  Actions := TListOfActions.Create;
  for var dv := Low(TIDEName) to High(TIDEName) do
  begin
    Actions.Add(IDEId[dv], Capture(dv));
  end;
end;


class function TLibSuffixesSectionDef.SectionNameStatic: string;
begin
  Result := 'lib suffixes';
end;

{ THelpSectionDef }

constructor THelpSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);

  Actions := TListOfActions.Create;
  Actions.Add('file', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.HelpFile := value; end);
end;

class function THelpSectionDef.SectionNameStatic: string;
begin
  Result := 'help';
end;

{ TDependenciesSectionDef }

constructor TDependenciesSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);

  ArrayMainAction := procedure(name, value: string; ErrorInfo: TErrorInfo) begin Project.Dependencies.Add(TDependency.Create(name, value)); end;
end;

class function TDependenciesSectionDef.SectionNameStatic: string;
begin
  Result := 'dependencies';
end;

{ TBuildingSectionDef }

constructor TBuildingSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);

  Actions := TListOfActions.Create;
  Actions.Add('naming', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.Naming := value; end);

end;

class function TBuildingSectionDef.SectionNameStatic: string;
begin
  Result := 'building';
end;

{ TNotSupportedFrameworksSectionDef }

procedure TNotSupportedFrameworksSectionDef.AddDelphiVersion(const dv: TIDEName);
begin
  Project.NotSupportedIDEs := Project.NotSupportedIDEs + [dv];
end;

function TNotSupportedFrameworksSectionDef.Capture(
  const dv: TIDEName): TAction;
begin
  Result := procedure(value: string; ErrorInfo: TErrorInfo) begin AddDelphiVersion(dv); end;
end;

constructor TNotSupportedFrameworksSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
var
  dv: TIDEName;
begin
  inherited Create(aParent, aProject);
  SectionValueTypes := TSectionValueTypes.NoValues;
  ContainsArrays := true;

  Actions := TListOfActions.Create;
  for dv := Low(TIDEName) to High(TIDEName) do
  begin
    Actions.Add(IDEId[dv], Capture(dv));
  end;

end;

class function TNotSupportedFrameworksSectionDef.SectionNameStatic: string;
begin
  Result := 'not supported';
end;

{ TSupportedDefinesSectionDef }

constructor TSupportedDefinesSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  Duplicated := TDictionary<string, boolean>.Create;
  SectionValueTypes := TSectionValueTypes.NoValues;
  ArrayMainAction := procedure (name, value: string; ErrorInfo: TErrorInfo) begin
    Project.Defines.AddOrSetValue(name, true);
  end;
end;

class function TSupportedDefinesSectionDef.SectionNameStatic: string;
begin
  Result := 'available defines'
end;

{ TDefinesSectionDef }

constructor TDefinesSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  ChildSections.Add(TSupportedDefinesSectionDef.SectionNameStatic, TSupportedDefinesSectionDef.Create(Self, aProject));
  Actions := TListOfActions.Create;
  Actions.Add('defines filename', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.DefinesFilename := value; end);
end;

class function TDefinesSectionDef.SectionNameStatic: string;
begin
  Result := 'defines'
end;

{ TRegistryKeysSectionDef }

constructor TRegistryKeysSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  ContainsArrays := true;
  ChildSectionAction :=
    function(Name: string; ErrorInfo: TErrorInfo): TSection
    begin
      if ChildSections.TryGetValue(Name, Result) then exit;
      Result := TRegistryEntrySectionDef.Create(Self, aProject, Name);
      ChildSections.Add(Name, Result);
    end;
end;

class function TRegistryKeysSectionDef.SectionNameStatic: string;
begin
  Result := 'registry keys'
end;

{ TRegistryEntrySectionDef }

constructor TRegistryEntrySectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition; const aKeyName: string);
begin
  inherited Create(aParent, aProject);
  KeyName := aKeyName;

  ChildSections.Add(TRegistryEntryValueSectionDef.SectionNameStatic, TRegistryEntryValueSectionDef.Create(Self, aProject, KeyName));
end;

function TRegistryEntrySectionDef.SectionName: string;
begin
  Result := KeyName;
end;

class function TRegistryEntrySectionDef.SectionNameStatic: string;
begin
  Result := '';
end;

{ TRegistryEntryValueSectionDef }

constructor TRegistryEntryValueSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition; const aKeyName: string);
begin
  inherited Create(aParent, aProject);
  Actions := TListOfActions.Create;
  Actions.Add('name', procedure(value: string; ErrorInfo: TErrorInfo) begin AddNewEntry(aKeyName, value); end);
  Actions.Add('data', procedure(value: string; ErrorInfo: TErrorInfo)
      begin
        if Entry = nil then raise Exception.Create('The value "data" should come after a "name" entry. '  + ErrorInfo.ToString);
        Entry.ValueData := value;
      end);
  Actions.Add('type', procedure(value: string; ErrorInfo: TErrorInfo)
      begin
        if Entry = nil then raise Exception.Create('The value "type" should come after a "name" entry. '  + ErrorInfo.ToString);
        Entry.ValueType := GetValueType(value, ErrorInfo);
      end);
end;

procedure TRegistryEntryValueSectionDef.AddNewEntry(const aKeyName, aValueName: string);
begin
  Entry := TRegistryEntry.Create;
  Entry.ValueName := aValueName;
  Entry.KeyName := aKeyName;
  Project.RegistryEntries.Add(Entry);
end;

function TRegistryEntryValueSectionDef.ListValueTypes: string;
begin
  Result := '';
  var Sep := '';
  for var dv := Low(TRegistryEntryType) to High(TRegistryEntryType) do
  begin
    Result := Result + Sep + RegistryTypes[dv];
    Sep := ', ';
  end;

end;


function TRegistryEntryValueSectionDef.GetValueType(const value: string;
  const ErrorInfo: TErrorInfo): TRegistryEntryType;
begin
  for var et := Low(TRegistryEntryType) to High(TRegistryEntryType) do
  begin
    if SameText(RegistryTypes[et], value) then exit(et);
  end;
  raise Exception.Create('"' + value + '" is not a recognized registry type. It must be one of: [' + ListValueTypes + ']. Verify that this tms version is the latest. ' + ErrorInfo.ToString );
end;


class function TRegistryEntryValueSectionDef.SectionNameStatic: string;
begin
  Result := 'value'
end;



{ TFrameworkVersionSectionDef }

constructor TFrameworkVersionSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition; const aFrameworkName: string);
begin
  inherited Create(aParent, aProject);
  Framework := aFrameworkName;
  Project.RegisterFramework(aFrameworkName,'');
  FrameworkDef := Project.GetFramework(Framework);
  Actions := TListOfActions.Create;
  Actions.Add('ide since', procedure(value: string; ErrorInfo: TErrorInfo)
  begin
    FrameworkDef.IdeSince := GetIDEName(value, ErrorInfo);
  end);
  Actions.Add('ide until', procedure(value: string; ErrorInfo: TErrorInfo)
  begin
    FrameworkDef.IdeUntil := GetIDEName(value, ErrorInfo);
  end);
  actions.Add('c++ builder support', procedure(value: string; ErrorInfo: TErrorInfo)
  begin
    FrameworkDef.SupportsCppBuilder := GetBoolEx(value, ErrorInfo);
  end);

  ChildSections.Add(TSectionFrameworkPlatformsDef.SectionNameStatic, TSectionFrameworkPlatformsDef.Create(Self, aProject, FrameworkDef));
  ChildSections.Add(TSectionFrameworkDependenciesDef.SectionNameStatic, TSectionFrameworkDependenciesDef.Create(Self, aProject, FrameworkDef));
end;

function TFrameworkVersionSectionDef.GetIDEName(const value: string;
  const ErrorInfo: TErrorInfo): TIDEName;
begin
  for var dv := Low(TIDEName) to High(TIDEName) do
  begin
    if IDEId[dv] = value then exit(dv);
  end;
  raise Exception.Create('"' + value + '" is not a recognized IDE version. It must be one of: [' + ListIDENames + ']. Verify that this tms version is the latest. ' + ErrorInfo.ToString );
end;

function TFrameworkVersionSectionDef.ListIDENames: string;
begin
  Result := '';
  var Sep := '';
  for var dv := Low(TIDEName) to High(TIDEName) do
  begin
    Result := Result + Sep + IDEId[dv];
    Sep := ', ';
  end;

end;

function TFrameworkVersionSectionDef.SectionName: string;
begin
  Result := Framework;
end;

class function TFrameworkVersionSectionDef.SectionNameStatic: string;
begin
  Result := 'Platforms';
end;

{ TSectionFrameworkPlatformsDef }

constructor TSectionFrameworkPlatformsDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition; const aFramework: TFrameworkDefinition);
var
  dp: TPlatform;
begin
  inherited Create(aParent, aProject);
  Framework := aFramework;
  SectionValueTypes := TSectionValueTypes.Both;
  ContainsArrays := true;

  Actions := TListOfActions.Create;
  for dp := Low(TPlatform) to High(TPlatform) do
  begin
    Actions.Add(PlatformId[dp], Capture(dp));
  end;
  Actions.Add('all', procedure(value: string; ErrorInfo: TErrorInfo)
  begin
    if GetBoolEx(value, ErrorInfo) then
    begin
      for var plat := Low(TPlatform) to High(TPlatform) do Framework.Platforms := Framework.Platforms + [plat];
    end
    else Framework.Platforms := [];
  end);

end;

procedure TSectionFrameworkPlatformsDef.AddPlatform(
  const dp: TPlatform; const value: Boolean);
begin
  if value then Framework.Platforms := Framework.Platforms + [dp]
  else Framework.Platforms := Framework.Platforms - [dp];
end;

function TSectionFrameworkPlatformsDef.Capture(
  const dp: TPlatform): TAction;
begin
  Result := procedure(value: string; ErrorInfo: TErrorInfo) begin AddPlatform(dp, GetBoolEx(value, ErrorInfo)); end;
end;


class function TSectionFrameworkPlatformsDef.SectionNameStatic: string;
begin
  Result := 'platforms';
end;

{ TSectionFrameworkDependenciesDef }

constructor TSectionFrameworkDependenciesDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition; const aFramework: TFrameworkDefinition);
begin
  inherited Create(aParent, aProject);
  Framework := aFramework;
  SectionValueTypes := TSectionValueTypes.Both;
  ContainsArrays := true;

  ArrayMainAction := procedure (name, value: string; ErrorInfo: TErrorInfo)
    begin
      AddDependency(name, value);
    end;

end;

function TSectionFrameworkDependenciesDef.DependencyAlreadyAdded(const depId: string): boolean;
begin
  for var dep in Project.Dependencies do if dep.Id = depId then exit(true);
  for var dep in Project.WeakDependencies do if dep.Id = depId then exit(true);


  Result := false;
end;

procedure TSectionFrameworkDependenciesDef.AddDependency(const depId: string; const depDescription: string);
begin
  var dd := depDescription;
  if dd.Trim = '' then dd := depId;

  Framework.Dependencies.Add(TDependency.Create(depId, dd));
  if not (depId.StartsWith('~')) and not DependencyAlreadyAdded(depId) then
  begin
    Project.WeakDependencies.Add(TDependency.Create(depId, dd));
  end;
end;


class function TSectionFrameworkDependenciesDef.SectionNameStatic: string;
begin
  Result := 'dependencies';
end;

{ TPathsSectionDef }

constructor TPathsSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  ChildSections.Add(TExtraLibraryPathSectionDef.SectionNameStatic, TExtraLibraryPathSectionDef.Create(Self, aProject));
  ChildSections.Add(TBuildOnlyLibraryPathSectionDef.SectionNameStatic, TBuildOnlyLibraryPathSectionDef.Create(Self, aProject));
  ChildSections.Add(TExtraBrowsingPathSectionDef.SectionNameStatic, TExtraBrowsingPathSectionDef.Create(Self, aProject));
  ChildSections.Add(TExtraDebugDCUPathSectionDef.SectionNameStatic, TExtraDebugDCUPathSectionDef.Create(Self, aProject));
  ChildSections.Add(TWebCorePathSectionDef.SectionNameStatic, TWebCorePathSectionDef.Create(Self, aProject));
  ChildSections.Add(TSearchPathsToPreserveSectionDef.SectionNameStatic, TSearchPathsToPreserveSectionDef.Create(Self, aProject));

end;

class function TPathsSectionDef.SectionNameStatic: string;
begin
  Result := 'paths';
end;

{ TExtraDebugDCUPathSectionDef }

constructor TExtraDebugDCUPathSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  Duplicated := TDictionary<string, boolean>.Create;
  SectionValueTypes := TSectionValueTypes.NoValues;

  ArrayMainAction := procedure (name, value: string; ErrorInfo: TErrorInfo)
    begin
      Project.ExtraPaths.DebugDCUPaths.Add(name);
    end;

end;

class function TExtraDebugDCUPathSectionDef.SectionNameStatic: string;
begin
  Result := 'extra debug dcu path';
end;

{ TExtraLibraryPathSectionDef }

constructor TExtraLibraryPathSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  Duplicated := TDictionary<string, boolean>.Create;
  SectionValueTypes := TSectionValueTypes.NoValues;

  ArrayMainAction := procedure (name, value: string; ErrorInfo: TErrorInfo)
    begin
      Project.ExtraPaths.LibraryPathsBuildAndRegister.Add(name);
    end;

end;

class function TExtraLibraryPathSectionDef.SectionNameStatic: string;
begin
  Result := 'extra library paths';
end;

{ TBuildOnlyLibraryPathSectionDef }

constructor TBuildOnlyLibraryPathSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  Duplicated := TDictionary<string, boolean>.Create;
  SectionValueTypes := TSectionValueTypes.NoValues;

  ArrayMainAction := procedure (name, value: string; ErrorInfo: TErrorInfo)
    begin
      Project.ExtraPaths.LibraryPathsBuildOnly.Add(name);
    end;

end;

class function TBuildOnlyLibraryPathSectionDef.SectionNameStatic: string;
begin
  Result := 'build-only library paths';
end;

{ TExtraBrowsingPathSectionDef }

constructor TExtraBrowsingPathSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  Duplicated := TDictionary<string, boolean>.Create;
  SectionValueTypes := TSectionValueTypes.NoValues;

  ArrayMainAction := procedure (name, value: string; ErrorInfo: TErrorInfo)
    begin
      Project.ExtraPaths.BrowsingPaths.Add(name);
    end;

end;

class function TExtraBrowsingPathSectionDef.SectionNameStatic: string;
begin
  Result := 'extra browsing paths';
end;

{ TWebCorePathSectionDef }

constructor TWebCorePathSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  Duplicated := TDictionary<string, boolean>.Create;
  SectionValueTypes := TSectionValueTypes.NoValues;

  ArrayMainAction := procedure (name, value: string; ErrorInfo: TErrorInfo)
    begin
      Project.ExtraPaths.WebCorePaths.Add(name);
    end;

end;

class function TWebCorePathSectionDef.SectionNameStatic: string;
begin
  Result := 'web core paths';
end;

{ TSearchPathsToPreserve }

constructor TSearchPathsToPreserveSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  Duplicated := TDictionary<string, boolean>.Create;
  SectionValueTypes := TSectionValueTypes.NoValues;

  ArrayMainAction := procedure (name, value: string; ErrorInfo: TErrorInfo)
    begin
      Project.AddSearchPathToPreserve(name);
    end;

end;

class function TSearchPathsToPreserveSectionDef.SectionNameStatic: string;
begin
  Result := 'search paths to preserve';
end;



{ TLinksSectionDef }

constructor TLinksSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  ContainsArrays := true;
  ChildSections.Add(TLinkSectionDef.SectionNameStatic, TLinkSectionDef.Create(Self, aProject));

  ChildSectionAction :=
    function(Name: string; ErrorInfo: TErrorInfo): TSection
    begin
      if ChildSections.TryGetValue(Name, Result) then
      begin
        Project.Shortcuts.Add(TShortcutDefinition.Create(TShortcutType.filelink, '', '', '', ''));
        exit;
      end;
      Result := nil;
    end;

end;

class function TLinksSectionDef.SectionNameStatic: string;
begin
  Result := 'links';

end;

{ TLinkSectionDef }

function TLinkSectionDef.GetShortcutType(const value: string;
  const ErrorInfo: TErrorInfo): TShortcutType;
begin
  if value = 'file' then exit (TShortcutType.filelink);

  raise Exception.Create('"' + value + '" is not a recognized shortcut type. It must be "file". ' + ErrorInfo.ToString );
end;


constructor TLinkSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  Actions := TListOfActions.Create;
  Actions.Add('type', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.Shortcuts.Last.ShortcutType := GetShortcutType(value, ErrorInfo); end );
  Actions.Add('name', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.Shortcuts.Last.Name := value; end);
  Actions.Add('target', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.Shortcuts.Last.Target := value; end);
  Actions.Add('description', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.Shortcuts.Last.Description := value; end);
  Actions.Add('working folder', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.Shortcuts.Last.WorkingFolder := value; end);

end;

class function TLinkSectionDef.SectionNameStatic: string;
begin
  Result := 'link';
end;

{ TFileLinksSectionDef }

constructor TFileLinksSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  ContainsArrays := true;
  ChildSections.Add(TFileLinkSectionDef.SectionNameStatic, TFileLinkSectionDef.Create(Self, aProject));

  ChildSectionAction :=
    function(Name: string; ErrorInfo: TErrorInfo): TSection
    begin
      if ChildSections.TryGetValue(Name, Result) then
      begin
        Project.FileLinks.Add(TFileLinkDefinition.Create);
        exit;
      end;
      Result := nil;
    end;

end;

class function TFileLinksSectionDef.SectionNameStatic: string;
begin
  Result := 'file links';
end;

{ TFileLinkSectionDef }

constructor TFileLinkSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  Actions := TListOfActions.Create;
  Actions.Add('file to link', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.FileLinks.Last.FileToLink := value; end );
  Actions.Add('link to folder', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.FileLinks.Last.LinkToFolder := value; end);
  Actions.Add('os', procedure (value: string; ErrorInfo: TErrorInfo)
  begin
    GetArray(value, ArrayActions, procedure(value: string; ErrorInfo: TErrorInfo)
    begin
    end, ErrorInfo);
  end);

  ArrayActions := TListOfActions.Create;
  ArrayActions.Add('windows', procedure(value: string; ErrorInfo: TErrorInfo)
  begin
    Project.FileLinks.Last.OS := Project.FileLinks.Last.OS + [TOperatingSystem.windows];
  end);
  ArrayActions.Add('linux', procedure(value: string; ErrorInfo: TErrorInfo)
  begin
    Project.FileLinks.Last.OS := Project.FileLinks.Last.OS + [TOperatingSystem.linux];
  end);
  ArrayActions.Add('mac', procedure(value: string; ErrorInfo: TErrorInfo)
  begin
    Project.FileLinks.Last.OS := Project.FileLinks.Last.OS + [TOperatingSystem.mac];
  end);
end;

class function TFileLinkSectionDef.SectionNameStatic: string;
begin
  Result := 'link';
end;

{ TOtherVersionsSectionDef }

constructor TOtherVersionsSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  ContainsArrays := true;
  Actions := TListOfActions.Create;
  Actions.Add('reg', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.OtherRegistryKeys.Add(value); end );

end;

class function TOtherVersionsSectionDef.SectionNameStatic: string;
begin
  Result := 'other versions';
end;

{ TPackageDefinitionsSectionDef }

constructor TPackageDefinitionsSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  ChildSectionAction :=
    function(Name: string; ErrorInfo: TErrorInfo): TSection
    begin
      if ChildSections.TryGetValue(Name, Result) then exit;
      var Package := FindPackage(Name, Project.Packages);
      if Package = nil then raise Exception.Create('Cannot find the package ' + Name + ' in the section "packages". ' + ErrorInfo.ToString);

      Result := TPackageDefinitionSectionDef.Create(Self, aProject, Package);
      ChildSections.Add(Name, Result);
    end;
end;

function TPackageDefinitionsSectionDef.FindPackage(const Name: string; const Packages: TListOfPackages): TPackage;
begin
  for var Package in Packages do if SameText(Package.Name, Name) then exit(Package);
  Result := nil;
end;

class function TPackageDefinitionsSectionDef.SectionNameStatic: string;
begin
  Result := 'package definitions';
end;

{ TPackageDefinitionSectionDef }
constructor TPackageDefinitionSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition; const aPackage: TPackage);
begin
  inherited Create(aParent, aProject);
  FPackage := aPackage;
  FName := aPackage.Name;
  Actions := TListOfActions.Create;
  Actions.Add('description', procedure(value: string; ErrorInfo: TErrorInfo)
    begin
      aPackage.Description := value;
    end);

  Actions.Add('framework type', procedure(value: string; ErrorInfo: TErrorInfo)
    begin
      aPackage.DelphiFrameworkType := ValidateDelphiFrameworkType(value, ErrorInfo);
    end);
  Actions.Add('requires', procedure(value: string; ErrorInfo: TErrorInfo)
    begin
      aPackage.Requires := GetRequires(value, ErrorInfo);
    end);

    ChildSections.Add(TStandardFilesSectionDef.SectionNameStatic,
       TStandardFilesSectionDef.Create(Self, aProject,
         procedure(Folder: string) begin FPackage.FileMasks.AddFolder(Folder); end,
         procedure(Masks: string) begin FPackage.FileMasks.SetIncludeFolders(GetMasks(Masks)); end,
         procedure(Masks: string) begin FPackage.FileMasks.SetExcludeFolders(GetMasks(Masks)); end,
         procedure(Masks: string) begin FPackage.FileMasks.SetIncludeFiles(GetMasks(Masks)); end,
         procedure(Masks: string) begin FPackage.FileMasks.SetExcludeFiles(GetMasks(Masks)); end,
         procedure(value: boolean) begin FPackage.FileMasks.SetRecursive(value); end
         )
       );
end;

function TPackageDefinitionSectionDef.GetMasks(const Masks: string): TArray<string>;
begin
  Result := Masks.Split([';']);
end;

function TPackageDefinitionSectionDef.ValidateDelphiFrameworkType(const value: string; const ErrorInfo: TErrorInfo): string;
begin
  if SameText(value, 'none') then exit(value.ToUpper);
  if SameText(value, 'vcl') then exit(value.ToUpper);
  if SameText(value, 'fmx') then exit(value.ToUpper);

  raise Exception.Create('"' + value + '" is not a valid value for framework type. Must be NONE, VCL or FMX. ' + ErrorInfo.ToString);

end;

function TPackageDefinitionSectionDef.GetRequires(const value: string; const ErrorInfo: TErrorInfo): TArray<string>;
begin
  Result := value.Split([';'], TStringSplitOptions.ExcludeEmpty);
  for var i := 0 to High(Result) do Result[i] := Result[i].Trim + '.dcp';

end;

function TPackageDefinitionSectionDef.SectionName: string;
begin
  Result := FName;
end;

class function TPackageDefinitionSectionDef.SectionNameStatic: string;
begin
  Result := '';
end;

{ TStandardFilesSectionDef }

constructor TStandardFilesSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition;
  const AddFolder, SetIncludeFolderMask, SetExcludeFolderMask, SetIncludeFileMask, SetExcludeFileMask: TProc<string>;
  const SetRecursive: TProc<boolean>);
begin
  inherited Create(aParent, aProject);
  SectionValueTypes := TSectionValueTypes.NoValues;
  ContainsArrays := true;

  ChildSections.Add(TStandardFilesSourceSectionDef.SectionNameStatic, TStandardFilesSourceSectionDef.Create(Self, aProject,
    AddFolder, SetIncludeFolderMask, SetExcludeFolderMask, SetIncludeFileMask, SetExcludeFileMask, SetRecursive));
end;

class function TStandardFilesSectionDef.SectionNameStatic: string;
begin
  Result := 'files';
end;

{ TStandardFilesSourceSectionDef }

constructor TStandardFilesSourceSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition;
  const AddFolder, SetIncludeFolderMask, SetExcludeFolderMask, SetIncludeFileMask, SetExcludeFileMask: TProc<string>;
  const SetRecursive: TProc<boolean>);
begin
  inherited Create(aParent, aProject);
  Actions := TListOfActions.Create;


  Actions.Add('folder', procedure(value: string; ErrorInfo: TErrorInfo) begin AddFolder(value);  end);
  Actions.Add('include folder mask', procedure(value: string; ErrorInfo: TErrorInfo) begin SetIncludeFolderMask(value); end);
  Actions.Add('exclude folder mask', procedure(value: string; ErrorInfo: TErrorInfo) begin SetExcludeFolderMask(value); end);
  Actions.Add('include file mask', procedure(value: string; ErrorInfo: TErrorInfo) begin SetIncludeFileMask(value); end);
  Actions.Add('exclude file mask', procedure(value: string; ErrorInfo: TErrorInfo) begin SetExcludeFileMask(value); end);
  Actions.Add('recursive', procedure(value: string; ErrorInfo: TErrorInfo) begin SetRecursive(GetBool(value, ErrorInfo)); end);
end;

class function TStandardFilesSourceSectionDef.SectionNameStatic: string;
begin
  Result := 'source';
end;

{ TPackageExtraDefinesSectionDef }

constructor TPackageExtraDefinesSectionDef.Create(const aParent: TSection;
  const aProject: TProjectDefinition);
begin
  inherited Create(aParent, aProject);
  ContainsArrays := true;
  Actions := TListOfActions.Create;
  Actions.Add('add', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.PackageExtraDefines.Add(value);  end);
  Actions.Add('remove', procedure(value: string; ErrorInfo: TErrorInfo) begin Project.PackageExtraDefines.Add('-'+ value);  end);
end;

class function TPackageExtraDefinesSectionDef.SectionNameStatic: string;
begin
  Result := 'extra defines';
end;

end.

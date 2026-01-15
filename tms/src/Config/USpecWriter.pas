unit USpecWriter;

interface
uses Classes, SysUtils,
     BBYaml.Types, BBYaml.Writer, BBStrings, BBArrays, BBClasses, UProjectDefinition, 
     Generics.Collections, Deget.CoreTypes, UCoreTypes;
type
  TSpecWriter = class
  private
    FProduct: TProjectDefinition;

    function OnMember(const Sender: TBBYamlWriter;
      const FullName: string; const ArrayIndex: integer): TYamlValue;
    function GetPatternMembers(const Sender: TBBYamlWriter; const Id:string; const ArrayIndex: integer): TArray<TNameAndComment>;
    function OnComment(const Sender: TBBYamlWriter; const  FullName :string; const Comment: string): string;
    function IsAddReplacePrefixedProperty(const FullName: string): boolean;
    function GetAddReplacePrefix(const FullName: string): string;
    function GetSupportedFrameworks: TArray<TNameAndComment>;
    function GetSupportedFrameworksDef(const s: string; const ArrayIndex: integer): TYamlValue;
    function GetPlatforms(const Platforms: TPlatformSet): TArray<string>;
    function GetPackages: TArray<TYamlValue>;
    function GetDependencies(const Dependencies: TObjectList<TDependency>; const IncludeDescription: boolean): TYamlValue;
    function GetFrameworksForPackage(const ArrayIndex: integer;
      const PackageName: string): TYamlValue;
    function GetDepencenciesForPackage(const Dependencies: TObjectList<TDependency>; const ArrayIndex: integer;
      const DependencyName: string): TYamlValue;
    function GetPackageFolder(const FolderName: string): TYamlValue;
    function GetPackageFolders: TArray<string>;
    function GetLibSuffix(const FolderName: string): TYamlValue;
    function GetLibSuffixes: TArray<string>;
    function GetExtraDefine(const ArrayIndex: integer;
      const ExtraDefineName: string): TYamlValue;
    function GetExtraDefines: TArray<TYamlValue>;
    function GetShortcut(const ArrayIndex: integer;
      const PropName: string): TYamlValue;
    function GetShortcuts: TArray<TYamlValue>;
    function GetFileLink(const ArrayIndex: integer;
      const PropName: string): TYamlValue;
    function GetFileLinks: TArray<TYamlValue>;
    function GetOtherVersions: TArray<TYamlValue>;
    function GetOtherVersion(const ArrayIndex: integer;
      const PropName: string): TYamlValue;
    function GetRegistryKey(const ArrayIndex: integer;
      const PropName: string): TYamlValue;
    function GetRegistryKeys: TArray<TYamlValue>;
    function GetPackageDefinition(const ArrIndex: integer;const PropName: string): TYamlValue;
    function GetPackageDefinitions: TArray<TNameAndComment>;
    function GetPackage(const Name: string): TPackage;
    function GetFileMasks(const FileMasks: TFileMasksList): TArray<TYamlValue>;

  public
    constructor Create(const aProduct: TProjectDefinition);
    destructor Destroy; override;


    procedure Save(const FileName: string; const WritingFormat: TWritingFormat; const UseJSON, CmdSyntax: boolean);
    procedure SaveToStream(const TextWriter: TStreamWriter; const WritingFormat: TWritingFormat; const UseJSON, CmdSyntax: boolean);
  end;

implementation
uses Types, JSON, UTmsBuildSystemUtils;



{ TSpecWriter }

function GetData(const Stream: TStream): string;
begin
  var Buffer: TBytes;
  SetLength(Buffer, Stream.Size);
  Stream.Position := 0;
  Stream.ReadBuffer(Buffer, Stream.Size);
  Result := TEncoding.UTF8.GetString(Buffer);
end;

constructor TSpecWriter.Create(const aProduct: TProjectDefinition);
begin
  FProduct := aProduct;
end;

destructor TSpecWriter.Destroy;
begin
  inherited;
end;

function TSpecWriter.GetAddReplacePrefix(const FullName: string): string;
begin
  Result := '';
end;

function TSpecWriter.GetSupportedFrameworks: TArray<TNameAndComment>;
begin
  Result := nil;
  SetLength(Result, FProduct.GetFrameworkCount);
  for var i := Low(Result) to High(Result) do
  begin
    Result[i].Name := FProduct.GetFrameworkName(i);
  end;
end;

function TSpecWriter.GetPatternMembers(const Sender: TBBYamlWriter;
  const Id: string; const ArrayIndex: integer): TArray<TNameAndComment>;
begin
  if Id.StartsWith('supported frameworks') then exit(GetSupportedFrameworks);
  if Id.StartsWith('package definitions:') then exit(GetPackageDefinitions);
  Result := nil;
end;

function TSpecWriter.IsAddReplacePrefixedProperty(
  const FullName: string): boolean;
begin
  Result := false;
end;

function TSpecWriter.OnComment(const Sender: TBBYamlWriter; const FullName,
  Comment: string): string;
begin
  Result := Comment;
end;

function Comment(const s: string): TYamlValue;
begin
  if s = '' then exit('product.id');
  exit(s);

end;

function Nullable(const s: string): TYamlValue; overload;
begin
  if s = '' then exit(TYamlValue.MakeNull);
  exit(s);

end;

function Nullable(const s: boolean): TYamlValue; overload;
begin
  if not s then exit(TYamlValue.MakeNull);
  exit(s);

end;

function Nullable(const s: TExeCompileWith): TYamlValue; overload;
begin
  case s of
    TExeCompileWith.Latest: exit('latest');
    TExeCompileWith.Earliest: exit('earliest');
  end;
  exit(TYamlValue.MakeNull);
end;

function Nullable(const s: TRegistryEntryType): TYamlValue; overload;
begin
  case s of
    TRegistryEntryType.String: exit(TYamlValue.MakeNull);
    TRegistryEntryType.DWord: exit('dword');
  end;
  raise Exception.Create('Unknown registry enumeration value');
end;

function Nullable(const s: TArray<string>): TYamlValue; overload;
begin
  if s = nil then exit(TYamlValue.MakeNull);
  Result := TYamlValue.MakeArray(s, false);
end;

function NullableObject(const s: TArray<string>): TYamlValue; overload;
begin
  if s = nil then exit(TYamlValue.MakeNull);
  Result := TYamlValue.MakeObject(s);
end;

function Nullable(const s: TArray<TYamlValue>): TYamlValue; overload;
begin
  if s = nil then exit(TYamlValue.MakeNull);
  Result := TYamlValue.MakeArray(s, false);
end;

function TSpecWriter.GetPlatforms(const Platforms: TPlatformSet): TArray<string>;
begin
  Result := nil;
  for var Platform := Low(TPlatform) to High(TPlatform) do
  begin
    if Platform in Platforms then
    begin
      Result := Result + [PlatformId[Platform]];
    end;
  end;
    
end;

function TSpecWriter.GetSupportedFrameworksDef(const s: string; const ArrayIndex: integer): TYamlValue;
begin
  var idx := s.IndexOf(':');
  if (idx < 0) or (idx = s.Length -1) then exit(TYamlValue.MakeObject);
  var id := s.Substring(0, idx);
  var id2 := s.Substring(idx + 1);

  if id2 = 'ide since:' then exit(IDEId[FProduct.GetFramework(id).IdeSince]);
  if (id2 = 'ide until:') then
  begin
    if FProduct.GetFramework(id).HasIdeUntil then exit(IDEId[FProduct.GetFramework(id).IdeUntil]);
    exit(TYamlValue.MakeNull);
  end;
  if id2 = 'platforms:' then exit(TYamlValue.MakeArray(GetPlatforms(FProduct.GetFramework(id).Platforms), false));
  if id2 = 'dependencies:' then exit(GetDependencies(FProduct.GetFramework(id).Dependencies, false));
  if id2.StartsWith('dependencies:') then exit(GetDepencenciesForPackage(FProduct.GetFramework(id).Dependencies, ArrayIndex, id2.Substring(Length('dependencies:'))));
  if id2 = 'c++ builder support:' then exit(Nullable(FProduct.GetFramework(id).SupportsCppBuilder));

  
  raise Exception.Create('Unknown property: ' + s);
end;

function TSpecWriter.GetPackages: TArray<TYamlValue>;
begin
  Result := nil;
  SetLength(Result, FProduct.Packages.Count);
  for var i := Low(Result) to High(Result) do
  begin
    Result[i] := TYamlValue.MakeObject([FProduct.Packages[i].Name]);
  end;
end;

function TSpecWriter.GetFrameworksForPackage(const ArrayIndex: integer; const PackageName: string): TYamlValue;
begin
  var Frameworks: TArray<string> := nil;
  var F := FProduct.Packages[ArrayIndex];
  if F.Name + ':' <> PackageName then raise Exception.Create('Internal error: Package name was "' + PackageName + '" and we expected "' + F.Name + '"');
  if F.PackageType = TPackageType.Exe then Frameworks := Frameworks + ['exe']
  else
  begin
    if F.IsRuntime then Frameworks := Frameworks + ['runtime'];
    if F.IsDesign then Frameworks := Frameworks + ['design'];
  end;

  for var Framework in F.Frameworks do
  begin
    Frameworks := Frameworks + [FProduct.GetFrameworkName(Framework)];
  end;

  Result := TYamlValue.MakeArray(Frameworks, true);
end;

function TSpecWriter.GetDependencies(const Dependencies: TObjectList<TDependency>; const IncludeDescription: boolean): TYamlValue;
begin
  if Dependencies.Count = 0 then exit(TYamlValue.MakeNull);
  
  var Items: TArray<TYamlValue> := nil;
  SetLength(Items, Dependencies.Count);
  for var i := Low(Items) to High(Items) do
  begin
    if IncludeDescription then Items[i] := TYamlValue.MakeObject([Dependencies[i].Id])
    else Items[i] := Dependencies[i].Id;
  end;

  Result := TYamlValue.MakeArray(Items, false);
end;

function TSpecWriter.GetDepencenciesForPackage(const Dependencies: TObjectList<TDependency>; const ArrayIndex: integer; const DependencyName: string): TYamlValue;
begin
  var D := Dependencies[ArrayIndex];
  if D.Id + ':' <> DependencyName then raise Exception.Create('Internal error: Dependency name was "' + DependencyName + '" and we expected "' + D.Id + '"');
  Result := D.Description;
end;

function TSpecWriter.GetPackageFolders: TArray<string>;
begin
  Result := nil;
  for var dv := Low(TIDEName) to High(TIDEName) do
  begin
    if FProduct.PackageFolders[dv].Value = '' then continue;
    case FProduct.PackageFolders[dv].PlusState of
      TPlusState.Single: Result := Result + [IDEId[dv]];
      TPlusState.Plus: Result := Result + [IDEId[dv] + '+'];
    end;

  end;
end;

function TSpecWriter.GetPackageFolder(const FolderName: string): TYamlValue;
begin
  for var dv := Low(TIDEName) to High(TIDEName) do
  begin
    if (IDEId[dv] + ':' = FolderName) or (IDEId[dv] + '+:' = FolderName) then exit(Nullable(FProduct.PackageFolders[dv].Value));
  end;

  Result := TYamlValue.MakeNull;

end;

function TSpecWriter.GetLibSuffixes: TArray<string>;
begin
  Result := nil;
  for var dv := Low(TIDEName) to High(TIDEName) do
  begin
    if FProduct.LibSuffixes[dv] = '' then continue;
    Result := Result + [IDEId[dv]];
  end;
end;

function TSpecWriter.GetLibSuffix(const FolderName: string): TYamlValue;
begin
  for var dv := Low(TIDEName) to High(TIDEName) do
  begin
    if (IDEId[dv] + ':' = FolderName) then exit(FProduct.LibSuffixes[dv]);
  end;

  Result := TYamlValue.MakeNull;
end;

function TSpecWriter.GetExtraDefines: TArray<TYamlValue>;
begin
  Result := nil;
  SetLength(Result, FProduct.PackageExtraDefines.Count);
  for var i := Low(Result) to High(Result) do
  begin
    if FProduct.PackageExtraDefines[i].StartsWith('-') then  Result[i] := TYamlValue.MakeObject(['remove']) else Result[i] := TYamlValue.MakeObject(['add']);
  end;
end;

function TSpecWriter.GetExtraDefine(const ArrayIndex: integer; const ExtraDefineName: string): TYamlValue;
begin
  var s := FProduct.PackageExtraDefines[ArrayIndex];
  if s.StartsWith('-') then s := s.Substring(1);
  Result := s;
end;


function TSpecWriter.GetShortcuts: TArray<TYamlValue>;
begin
  Result := nil;
  SetLength(Result, FProduct.Shortcuts.Count);
  for var i := Low(Result) to High(Result) do
  begin
    Result[i] := TYamlValue.MakeObject();
  end;
end;

function GetShortcutType(const s: TShortcutType): string;
begin
  case s of
    TShortcutType.filelink: exit('file');
  end;
  raise Exception.Create('Unknown shortcut type');
end;

function TSpecWriter.GetShortcut(const ArrayIndex: integer; const PropName: string): TYamlValue;
begin
  var s := FProduct.Shortcuts[ArrayIndex];
  if PropName = 'type:' then exit(GetShortcutType(s.ShortcutType));
  if PropName = 'name:' then exit(s.Name);
  if PropName = 'target:' then exit(s.Target);
  if PropName = 'description:' then exit(s.Description);
  if PropName = 'working folder:' then exit(s.WorkingFolder);

  raise Exception.Create('Unknown property in shortcut: ' + PropName);

end;


function TSpecWriter.GetFileLinks: TArray<TYamlValue>;
begin
  Result := nil;
  SetLength(Result, FProduct.FileLinks.Count);
  for var i := Low(Result) to High(Result) do
  begin
    Result[i] := TYamlValue.MakeObject();
  end;
end;

function GetOS(const os: TOperatingSystemSet): TYamlValue;
begin
  var Items: TArray<string>;
  for var x in os do
  begin
    case x of
      TOperatingSystem.windows: Items := Items + ['windows'];
      TOperatingSystem.linux: Items := Items + ['linux'];
      TOperatingSystem.mac: Items := Items + ['mac'];
    end;
  end;
  Result := TYamlValue.MakeArray(Items, true);
end;

function TSpecWriter.GetFileLink(const ArrayIndex: integer; const PropName: string): TYamlValue;
begin
  var s := FProduct.FileLinks[ArrayIndex];
  if PropName = 'file to link:' then exit(s.FileToLink);
  if PropName = 'link to folder:' then exit(s.LinkToFolder);
  if PropName = 'os:' then exit(GetOS(s.OS));

  raise Exception.Create('Unknown property in file link: ' + PropName);
end;

function TSpecWriter.GetOtherVersions: TArray<TYamlValue>;
begin
  Result := nil;
  SetLength(Result, FProduct.OtherRegistryKeys.Count);
  for var i := Low(Result) to High(Result) do
  begin
    Result[i] := TYamlValue.MakeObject();
  end;
end;

function GetUnique(const Entries: TList<TRegistryEntry>): TArray<TArray<TRegistryEntry>>;
begin
  Result := nil;
  var Existing := TDictionary<string, integer>.Create;
  try
    for var Entry in Entries do
    begin
      var idx := -1;
      if not Existing.TryGetValue(Entry.KeyName, idx) then
      begin
        idx := Length(Result);
        Result := Result + [[Entry]];
        Existing.Add(Entry.KeyName, idx);
      end
      else
      begin
        Result[idx] := Result[idx] + [Entry];
      end;

    end;
  finally
    Existing.Free;
  end;
end;

function TSpecWriter.GetRegistryKeys: TArray<TYamlValue>;
begin
  Result := nil;
  var UniqueEntries := GetUnique(FProduct.RegistryEntries);
  SetLength(Result, Length(UniqueEntries));
  for var i := Low(Result) to High(Result) do
  begin
    Result[i] := TYamlValue.MakeObject([UniqueEntries[i][0].KeyName]);
  end;
end;

function FindEntry(const UniqueEntries: TArray<TArray<TRegistryEntry>>; const s: string): TArray<TRegistryEntry>;
begin
  for var Entry in UniqueEntries do if Entry[0].KeyName = s then exit(Entry);
  raise Exception.Create('Cannot find unique entry ' + s);

end;

function TSpecWriter.GetRegistryKey(const ArrayIndex: integer; const PropName: string): TYamlValue;
begin
  var UniqueEntries := GetUnique(FProduct.RegistryEntries);

  var idx := PropName.IndexOf(':');
  if idx = Length(PropName) - 1 then
  begin
    var Entry := UniqueEntries[ArrayIndex];

    Result := TYamlValue.MakeArray(Length(Entry), function(index: integer): TYamlValue begin Result := TYamlValue.MakeObject(['value']); end, false);
    exit;
  end;

  var id2 := PropName.Substring(idx + 1);
  if id2 = 'value:' then exit(TYamlValue.MakeObject(['name', 'type', 'data']));

  var Entry := FindEntry(UniqueEntries, PropName.Substring(0, idx))[ArrayIndex];
  if id2 = 'value:name:' then exit(Entry.ValueName);
  if id2 = 'value:type:' then exit(Nullable(Entry.ValueType));
  if id2 = 'value:data:' then exit(Entry.ValueData);

  raise Exception.Create('Unexpected name: ' + PropName);
end;

function TSpecWriter.GetPackageDefinitions: TArray<TNameAndComment>;
begin
  Result := nil;
  SetLength(Result, FProduct.Packages.Count);
  for var i := Low(Result) to High(Result) do
  begin
    Result[i] := TNameAndComment.Create(FProduct.Packages[i].Name, '');
  end;
end;

function TSpecWriter.GetPackage(const Name: string): TPackage;
begin
  for var Pkg in FProduct.Packages do
  begin
    if Pkg.Name = Name then exit(Pkg);
  end;

  raise Exception.Create('Cannot find package: ' + Name);

end;

function GetRequires(const Requires: TArray<string>): string;
begin
  Result := '';
  for var r in Requires do
  begin
    if Result <> '' then Result := Result + ';';
    if r.EndsWith('.dcp') then Result := Result + r.Substring(0, r.Length - Length('.dcp'));
  end;

end;

function TSpecWriter.GetFileMasks(const FileMasks: TFileMasksList): TArray<TYamlValue>;
begin
  Result := nil;
  SetLength(Result, Length(FileMasks.FileMasks));
  for var i := Low(Result) to High(Result) do Result[i] := TYamlValue.MakeObject(['source']);

end;

function GetMask(const Masks: TArray<string>): TYamlValue;
begin
  Result := Nullable(String.Join(';', Masks));
end;

function TSpecWriter.GetPackageDefinition(const ArrIndex: integer; const PropName: string): TYamlValue;
begin
  var idx := PropName.IndexOf(':');
  if idx = PropName.Length - 1 then exit(TYamlValue.MakeObject);
  var NewPropName := PropName.Substring(idx + 1);
  var Pkg := GetPackage(PropName.Substring(0, idx));
  if Pkg.FileMasks.Empty then exit(TYamlValue.MakeNull);

  if NewPropName = 'framework type:' then exit(Pkg.DelphiFrameworkType);
  if NewPropName = 'description:' then exit(Pkg.Description);
  if NewPropName = 'requires:' then exit(GetRequires(Pkg.Requires));
  if NewPropName = 'files:' then exit(TYamlValue.MakeArray(GetFileMasks(Pkg.FileMasks), false));
  if NewPropName = 'files:source:' then exit(TYamlValue.MakeObject);
  if NewPropName = 'files:source:folder:' then exit(Pkg.FileMasks.FileMasks[ArrIndex].BaseFolder);
  if NewPropName = 'files:source:include folder mask:' then exit(GetMask(Pkg.FileMasks.FileMasks[ArrIndex].IncludeFolders));
  if NewPropName = 'files:source:exclude folder mask:' then exit(GetMask(Pkg.FileMasks.FileMasks[ArrIndex].ExcludeFolders));
  if NewPropName = 'files:source:include file mask:' then exit(GetMask(Pkg.FileMasks.FileMasks[ArrIndex].IncludeFiles));
  if NewPropName = 'files:source:exclude file mask:' then exit(GetMask(Pkg.FileMasks.FileMasks[ArrIndex].ExcludeFiles));
  if NewPropName = 'files:source:recursive:' then exit(Pkg.FileMasks.FileMasks[ArrIndex].Recursive);

  raise Exception.Create('Unknown property: ' + PropName);
end;


function TSpecWriter.GetOtherVersion(const ArrayIndex: integer; const PropName: string): TYamlValue;
begin
  var s := FProduct.OtherRegistryKeys[ArrayIndex];
  if PropName = 'reg:' then exit(s);

  raise Exception.Create('Unknown property in other versions: ' + PropName);
end;


function TSpecWriter.OnMember(const Sender: TBBYamlWriter;
  const FullName: string; const ArrayIndex: integer): TYamlValue;
const
  {$i ../../../Version.inc}
begin
  if FullName = 'minimum required tmsbuild version:' then exit(TMSVersion);

  //application
  if FullName = 'application:' then exit(TYamlValue.MakeObject);
  if FullName = 'application:id:' then exit(Comment(FProduct.Application.Id));
  if FullName = 'application:name:' then exit(Nullable(FProduct.Application.Name));
  if FullName = 'application:description:' then exit(Nullable(FProduct.Application.Description));
  if FullName = 'application:company name:' then exit(Nullable(FProduct.Application.CompanyName));
  if FullName = 'application:copyright:' then exit(Nullable(FProduct.Application.Copyright));
  if FullName = 'application:url:' then exit(Nullable(FProduct.Application.Url));
  if FullName = 'application:vcs protocol:' then exit(Nullable(FProduct.Application.VCSProtocol));
  if FullName = 'application:docs:' then exit(Nullable(FProduct.Application.Docs));
  if FullName = 'application:version file:' then exit(Nullable(FProduct.Application.VersionFile));

  var sf := 'supported frameworks:';
  if FullName = sf then exit(TYamlValue.MakeObject);
  if FullName.StartsWith(sf) then exit(GetSupportedFrameworksDef(FullName.Substring(sf.Length), ArrayIndex));

  if FullName = 'packages:' then exit(TYamlValue.MakeArray(GetPackages, false));
  if FullName.StartsWith('packages:') then exit(GetFrameworksForPackage(ArrayIndex, FullName.Substring(Length('packages:'))));


  if FullName = 'dependencies:' then exit(GetDependencies(FProduct.Dependencies, true));
  if FullName.StartsWith('dependencies:') then exit(GetDepencenciesForPackage(FProduct.Dependencies, ArrayIndex, FullName.Substring(Length('dependencies:'))));

  //package options
  if FullName = 'package options:' then exit(TYamlValue.MakeObject);
  if FullName = 'package options:ignore dproj platforms:' then exit(Nullable(FProduct.IgnoreDprojPlatforms));
  if FullName = 'package options:root package folder:' then exit(Nullable(FProduct.RootPackageFolder));
  if FullName = 'package options:add libsuffix:' then exit(Nullable(FProduct.AddLibSuffix));

  if FullName = 'package options:package folders:' then exit(NullableObject(GetPackageFolders));
  if FullName.StartsWith('package options:package folders:') then exit(GetPackageFolder(FullName.Substring(Length('package options:package folders:'))));
  if FullName = 'package options:lib suffixes:' then exit(TYamlValue.MakeObject(GetLibSuffixes));
  if FullName.StartsWith('package options:lib suffixes:') then exit(GetLibSuffix(FullName.Substring(Length('package options:lib suffixes:'))));
  if FullName = 'package options:extra defines:' then exit(Nullable(GetExtraDefines));
  if FullName.StartsWith('package options:extra defines:') then exit(GetExtraDefine(ArrayIndex, FullName.Substring(Length('package options:lib defines:'))));

  //package definitions:
  if FullName = 'package definitions:' then exit(TYamlValue.MakeObject);
  if FullName.StartsWith('package definitions:') then exit(GetPackageDefinition(ArrayIndex, FullName.Substring(Length('package definitions:'))));

  //exe options
  if FullName = 'exe options:' then exit(TYamlValue.MakeObject);
  if FullName = 'exe options:delphi versions:' then exit(Nullable(FProduct.ExeOptions.CompileWith));
  if FullName = 'exe options:compile with:' then exit(Nullable(FProduct.ExeOptions.CompileWith));
  if FullName = 'exe options:debug exes:' then exit(Nullable(FProduct.ExeOptions.ExeDebug));

  //paths:
  if FullName = 'paths:' then exit(TYamlValue.MakeObject);
  if FullName = 'paths:extra library paths:' then exit(Nullable(FProduct.ExtraPaths.LibraryPathsBuildAndRegister.ToArray));
  if FullName = 'paths:extra browsing paths:' then exit(Nullable(FProduct.ExtraPaths.BrowsingPaths.ToArray));
  if FullName = 'paths:extra debug dcu path:' then exit(Nullable(FProduct.ExtraPaths.DebugDCUPaths.ToArray));
  if FullName = 'paths:web core paths:' then exit(Nullable(FProduct.ExtraPaths.WebCorePaths.ToArray));
  if FullName = 'paths:search paths to preserve:' then exit(Nullable(FProduct.SearchPathsToPreserve));
  if FullName = 'paths:build-only library paths:' then exit(Nullable(FProduct.ExtraPaths.LibraryPathsBuildOnly.ToArray));

  //defines:
  if FullName = 'defines:' then exit(TYamlValue.MakeObject);
  if FullName = 'defines:defines filename:' then exit(FProduct.DefinesFilename);
  if FullName = 'defines:available defines:' then exit(TYamlValue.MakeArray(FProduct.Defines.Keys.ToArray, false));

  //registry keys:
  if FullName = 'registry keys:' then exit(TYamlValue.MakeArray(GetRegistryKeys, false));
  if FullName.StartsWith('registry keys:') then exit(GetRegistryKey(ArrayIndex, FullName.Substring(Length('registry keys:'))));

  //help:
  if FullName = 'help:' then exit(TYamlValue.MakeObject);
  if FullName = 'help:file:' then exit(FProduct.HelpFile);

  //links:
  if FullName = 'links:' then exit(TYamlValue.MakeArray(GetShortcuts, false));
  if FullName = 'links:link:' then exit(TYamlValue.MakeObject);
  if FullName.StartsWith('links:link:') then exit(GetShortcut(ArrayIndex, FullName.Substring(Length('links:link:'))));

  //file links:
  if FullName = 'file links:' then exit(TYamlValue.MakeArray(GetFileLinks, false));
  if FullName = 'file links:link:' then exit(TYamlValue.MakeObject);
  if FullName.StartsWith('file links:link:') then exit(GetFileLink(ArrayIndex, FullName.Substring(Length('file links:link:'))));

  //other versions:
  if FullName = 'other versions:' then exit(TYamlValue.MakeArray(GetOtherVersions, false));
  if FullName.StartsWith('other versions:') then exit(GetOtherVersion(ArrayIndex, FullName.Substring(Length('other versions:'))));

  raise Exception.Create('Unknown property: ' + FullName);
end;

procedure TSpecWriter.Save(const FileName: string; const WritingFormat: TWritingFormat; const UseJSON, CmdSyntax: boolean);
begin
  var TextWriter := TStreamWriter.Create(FileName, false, TUTF8NoBOMEncoding.Instance);
  try
    SaveToStream(TextWriter, WritingFormat, UseJSON, CmdSyntax);
  finally
    TextWriter.Free;
  end;
end;


procedure TSpecWriter.SaveToStream(const TextWriter: TStreamWriter; const WritingFormat: TWritingFormat; const UseJSON, CmdSyntax: boolean);
begin
  var SchemaStream := TResourceStream.Create(HInstance, 'TmsBuildSchema', RT_RCDATA);
  try
    var BBWriter := TBBYamlWriter.Create(WritingFormat, '', UseJSON, CmdSyntax, '-s');
    try
      BBWriter.OnMember := OnMember;
      BBWriter.OnComment := OnComment;
      BBWriter.GetPatternMembers := GetPatternMembers;
      BBWriter.IsAddReplacePrefixedProperty := IsAddReplacePrefixedProperty;
      BBWriter.GetAddReplacePrefix := GetAddReplacePrefix;

      var Schema := TJSONObject.ParseJSONValue(GetData(SchemaStream), true, true) as TJSONObject;
      try
        BBWriter.Save(TextWriter, Schema,
          'https://raw.githubusercontent.com/tmssoftware/smartsetup/refs/heads/main/tms/example-config/tmsbuild.schema.json',
          'TMS Smart Setup configuration file for product ' + FProduct.Application.Name +#10'See https://doc.tmssoftware.com/smartsetup/guide/creating-bundles.html#creating-a-tmsbuildyaml-file'
          );
      finally
        Schema.Free;
      end;
    finally
      BBWriter.Free;
    end;

  finally
    SchemaStream.Free;
  end;

end;

end.

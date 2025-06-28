unit Package.Creator;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, Deget.IDETypes, IOUtils, UTmsBuildSystemUtils, UProjectDefinition, UProjectList;

const
  NewLine = #13#10;

procedure CreatePackages(const Projects: TProjectList);

implementation
uses SysUtils, Classes, Zip, System.Types, Util.Replacer, Deget.Filer.DprojFile,
     Deget.Version, Generics.Collections;

//No need to escape non-ascii characters, this will be saved as utf-8
function XmlEscape(const s: string): string;
begin
  Result := StringReplace(s, '&lt;', '<', [rfReplaceAll]);
  Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);
  Result := StringReplace(Result, '&amp', '&', [rfReplaceAll]);
end;

//No need to escape non-ascii characters, this will be saved as utf-8
function DelphiEscape(const s: string): string;
begin
  Result := StringReplace(s, '''', '''''', [rfReplaceAll]);
end;

function GetDpkSection(const Data: TArray<string>; const Transform: TFunc<string, string>): string;
begin
  Result := '';
  for var Entry in Data do
  begin
    var EntryTransformed := Transform(Entry);
    if EntryTransformed = '' then continue;

    if Result <> '' then Result := Result + ',' + NewLine;
    Result := Result + '  ' + EntryTransformed;
  end;

  if Result <> '' then Result := Result + ';' + NewLine;

end;

function GetDpkContain(const s: string): string;
begin
  //We aren't handling forms here. Those are 'a in b.pas {Form1}'
  //But we don't know the form name. We could try to guess it by opening the dfm,
  //but dfms can be binary. We could ask for them in the yaml, but it is a little too much work.
  //In reality the packages will compile the same, so it doesn't seem worth the extra effort to specify the forms.
  if not SameText(TPath.GetExtension(s), '.pas') then exit('');
  Result := DelphiEscape(TPath.GetFileNameWithoutExtension(s)) + ' in ''' + s + '''';
end;

function Matches(const s: string; const aMatches: array of string): boolean;
begin
  for var m in aMatches do if SameText(s, m) then exit(true);
  Result := false;
end;

function DCCReference(const s: string): string;
begin
  // Forms are like:
  //        <DCCReference Include="Unit3.pas">
  //          <Form>Form33</Form>
  //          <FormType>dfm</FormType>
  //      </DCCReference>
  // FormType might be dfm or fmx
  // But, same as in the dpk, it seems to work even without this.
  if Matches(TPath.GetExtension(s), ['.dfm', '.fmx']) then exit('');

  if Matches(TPath.GetExtension(s), ['.dcp', '.pas', '.dcr']) then exit('        <DCCReference Include="' + XmlEscape(s) + '"/>');
  if SameText(TPath.GetExtension(s), '.rc') then exit(Format(
  '''
        <RcCompile Include="%s">
            <Form>%s</Form>
        </RcCompile>
  ''', [XmlEscape(s), XmlEscape(TPath.GetFileNameWithoutExtension(s)) + '.res']));
  Result := '<None Include="' + XmlEscape(s) + '"/>';
end;

function GetDccReferences(const Data: array of TArray<string>): string;
begin
  Result := '';
  for var List in Data do
  begin
    for var Entry in List do
    begin
      var DccRef := DCCReference(Entry);
      if DccRef = '' then continue;

      Result := Result + DccRef + NewLine;
    end;
  end;
end;

function GetCppOutput(const value: boolean): string;
begin
  if Value then exit('All');
  exit('None');
end;

function TargetedPlatforms(const IDEName: TIDEName; Platforms: TPlatformSet): string;
begin
  //we want to include newer platforms, even if the package supports older delphi versions that don't have it.
  //For example, D11 doesn't have Win64x, but a package for D11+ should include it.
  Platforms := Platforms * PlatformsInDelphi[High(TIDEName)];

  Result := IntToStr(PlatformsToInteger(Platforms));

end;

function HasPlatform(const Platform: TPlatform; const Platforms: TPlatformSet): string;
begin
  if Platform in Platforms then exit('True');
  exit('False');
end;

function GetAndroidVersionCode(const Version: TVersion): integer;
begin
  //biggest version code Google Play supports is 2100000000. //https://developer.android.com/studio/publish/versioning
  //we will just use the first 2 numbers, and multiply major * 100. So v 1.3 will have a version code of 103
  Result := Version.Major * 100 + Version.Minor;
end;

function GetFirstId(const id: string): string;
begin
  var Idx := id.IndexOf('.');
  if Idx < 0 then exit(id);
  Result := id.Substring(0, Idx);
end;

function GetPackagePlatforms(const Project: TProjectDefinition; const Package: TPackage): TPlatformSet;
begin
  Result := [];
  for var fr in Package.Frameworks do
  begin
    var Framework := Project.GetFramework(Project.GetFrameworkName(fr));
    Result := Result + Framework.Platforms;
  end;
end;

function SupportsCppBuilder(const Project: TProjectDefinition; const Package: TPackage): boolean;
begin
  for var fr in Package.Frameworks do
  begin
    var Framework := Project.GetFramework(Project.GetFrameworkName(fr));
    if Framework.SupportsCppBuilder then exit(true);
  end;

  Result := false;
end;

function GetFiles(const Project: TProjectDefinition; const Masks: TFileMasksList): TArray<string>;
begin
  var ListResult := TList<string>.Create;
  try
    for var Mask in Masks.FileMasks do
    begin
      var RootFolder := TPath.GetFullPath(TPath.Combine(Project.RootFolder, Mask.BaseFolder));
      if FolderIsOutside(RootFolder, [Project.RootFolder])
        then raise Exception.Create('The sources of the package can''t be outside the root folder of the project.' +
        ' Source folder is: "' + RootFolder + '" and the root folder is "' + Project.RootFolder + '"');

      ScanFiles(RootFolder, Mask.IncludeFolders, Mask.ExcludeFolders, Mask.IncludeFiles, Mask.ExcludeFiles,
      procedure(FileName, RelPath: string)
      begin
        ListResult.Add(TPath.Combine('..', '..', Mask.BaseFolder, RelPath));
      end,
      Mask.Recursive);
    end;

    Result := ListResult.ToArray;
  finally
    ListResult.Free;
  end;

end;

function ReplaceVariable(const varName: string; const IDEName: TIDEName; const Project: TProjectDefinition; const Package: TPackage; out IsEscaped: boolean): string;
begin
  IsEscaped := false;
  if varName = 'package-name' then exit(Package.Name);
  if varName = 'description' then exit(Package.Description);
  if varName = 'design-or-runtime-dpk' then
    if Package.IsRuntime then
      if Package.IsDesign then exit('')
      else exit('{$RUNONLY}')
    else if Package.IsDesign then exit('{$DESIGNONLY}')
    else exit('');

  if varName = 'design-only' then if Package.IsDesign and not Package.IsRuntime then exit('true') else exit('false');
  if varName = 'runtime-only' then if Package.IsRuntime and not Package.IsDesign then exit('true') else exit('false');

  if varName = 'lib-suffix-dpk' then exit(GetLibSuffix(IDEName).ToUpperInvariant);
  if varName = 'lib-suffix-dproj' then exit(GetLibSuffixDproj(IDEName));
  if varName = 'requires' then begin IsEscaped := true; exit(GetDpkSection(Package.Requires, function(s: string): string begin Result := DelphiEscape(TPath.GetFileNameWithoutExtension(s)); end)); end;

  var PackagePlatforms := GetPackagePlatforms(Project, Package);
  var PackageFiles := GetFiles(Project, Package.FileMasks);
  if varName = 'contains' then begin IsEscaped := true; exit(GetDpkSection(PackageFiles, function(s: string): string begin Result := GetDpkContain(s); end));end;
  if varName = 'guid' then exit(GuidToString(TGuid.NewGuid));
  if varName = 'framework-type' then exit(Package.DelphiFrameworkType);
  if varName = 'targeted-platforms' then exit(TargetedPlatforms(IDEName, PackagePlatforms));
  if varName = 'cpp-output' then exit(GetCppOutput(SupportsCppBuilder(Project, Package)));
  if varName = 'dcc-references' then begin IsEscaped := true; exit(GetDccReferences([Package.Requires, PackageFiles])); end;

  if varName = 'plat-android' then exit(HasPlatform(TPlatform.android32, PackagePlatforms));
  if varName = 'plat-android64' then exit(HasPlatform(TPlatform.android64, PackagePlatforms));
  if varName = 'plat-iosdevice64' then exit(HasPlatform(TPlatform.iosdevice64, PackagePlatforms));
  if varName = 'plat-iossimarm64' then exit(HasPlatform(TPlatform.iossimulator64arm, PackagePlatforms));
  if varName = 'plat-linux64' then exit(HasPlatform(TPlatform.linux64, PackagePlatforms));
  if varName = 'plat-osx64' then exit(HasPlatform(TPlatform.macos64intel, PackagePlatforms));
  if varName = 'plat-osxarm64' then exit(HasPlatform(TPlatform.macos64arm, PackagePlatforms));
  if varName = 'plat-win32' then exit(HasPlatform(TPlatform.win32intel, PackagePlatforms));
  if varName = 'plat-win64' then exit(HasPlatform(TPlatform.win64intel, PackagePlatforms));
  if varName = 'plat-win64x' then exit(HasPlatform(TPlatform.win64Xintel, PackagePlatforms));

  //verinfo
  if varName = 'verinfo-package' then exit('com.' + GetFirstId(Project.Application.Id) + '.$(MSBuildProjectName)');
  if varName = 'verinfo-version-code' then
      begin
        if Project.Application.Version = '' then exit('0');
        exit(TVersion(Project.Application.Version).Normalized);
      end;

  if varName = 'verinfo-version-name' then
      begin
        if Project.Application.Version = '' then exit(TVersion('0.0').Normalized);
        exit(TVersion(Project.Application.Version).Normalized);
      end;
  if varName = 'verinfo-company-name' then exit(Project.Application.CompanyName);
  if varName = 'verinfo-copyright' then exit(Project.Application.Copyright);
  if varName = 'verinfo-program-id' then exit('com.' + Project.Application.Id);

  if varName  = 'verinfo-major-ver' then exit(IntToStr(TVersion(Project.Application.Version).Major));
  if varName  = 'verinfo-minor-ver' then exit(IntToStr(TVersion(Project.Application.Version).Minor));
  if varName  = 'verinfo-release' then exit(IntToStr(TVersion(Project.Application.Version).Release));
  if varName  = 'verinfo-build' then exit(IntToStr(TVersion(Project.Application.Version).Build));


  raise Exception.Create('Unknown variable: ' + varName);

end;

procedure ReplaceData(const IDEName: TIDEName; const Template: TBytes; const IsXML: boolean; const Project: TProjectDefinition; const Package: TPackage; const OutFileName: string);
begin
  var Data := TEncoding.UTF8.GetString(Template);
  var ReplacedData := ParseString(Data, function(varName: string): string
    begin
      var IsEscaped: boolean;
      Result := ReplaceVariable(varName, IDEName, Project, Package, IsEscaped);
      if not IsEscaped then
      begin
        if IsXML then Result := XmlEscape(Result) else Result := DelphiEscape(Result);
      end;
    end);

  TDirectory_CreateDirectory(TPath.GetDirectoryName(OutFileName));
  var Encoding := TUTF8Encoding.Create(False);
  try
    TFile.WriteAllText(OutFileName, ReplacedData, Encoding);
  finally
    Encoding.Free;
  end;
end;


procedure ProcessTemplates(const IDEName: TIDEName; const Project: TProjectDefinition; const Package: TPackage; const BaseFileName: string);
begin
  var PackageTemplates := TResourceStream.Create(HInstance, 'PackageTemplates', RT_RCDATA);
  try
    var Zip := TZipFile.Create;
    try
      Zip.Open(PackageTemplates, TZipMode.zmRead);
      var Template: TBytes;
      Zip.Read(DelphiSuffixes[IDEName] + '/Package.dpk', Template);
      ReplaceData(IDEName, Template, false, Project, Package, BaseFileName + '.dpk');
      Zip.Read(DelphiSuffixes[IDEName] + '/Package.dproj', Template);
      ReplaceData(IDEName, Template, true, Project, Package, BaseFileName + '.dproj');
    finally
      Zip.Free;
    end;

  finally
    PackageTemplates.Free;
  end;

end;

procedure CreatePackages(const Projects: TProjectList);
begin
  for var Project in Projects.All do
  begin
    for var Package in Project.Packages do
    begin
      if Package.FileMasks.Empty then continue;
      const IDEName = TIDEName.delphi11;
      ProcessTemplates(IDEName, Project, Package, TPath.Combine(Project.RootFolder, 'packages', DelphiSuffixes[IDEName], Package.Name));
    end;
  end;
end;
end.

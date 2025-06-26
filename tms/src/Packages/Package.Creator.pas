unit Package.Creator;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, Deget.IDETypes, IOUtils, UTmsBuildSystemUtils, Package.Definition;

const
  NewLine = #13#10;

procedure CreatePackages(const IDENames: TArray<TIDEName>);
implementation
uses SysUtils, Classes, Zip, System.Types, Util.Replacer, Deget.Filer.DprojFile;

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
  Result := s + ' in ' + s + '.pas';
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

  if Matches(TPath.GetExtension(s), ['.dcp', '.pas', '.dcr']) then exit('        <DCCReference Include="' + s + '"/>');
  if SameText(TPath.GetExtension(s), '.rc') then exit(Format(
  '''
        <RcCompile Include="%s">
            <Form>%s</Form>
        </RcCompile>
  ''', [s, TPath.GetFileNameWithoutExtension(s) + '.res']));
  Result := '<None Include="' + s + '"/>';
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
  Platforms := Platforms * PlatformsInDelphi[IDEName];

  Result := IntToStr(PlatformsToInteger(Platforms));

end;

function HasPlatform(const Platform: TPlatform; const Platforms: TPlatformSet): string;
begin
  if Platform in Platforms then exit('True');
  exit('False');
end;

procedure ReplaceData(const IDEName: TIDEName; const Template: TBytes; const Package: TPackageDefinition; const OutFileName: string);
begin
  var Data := TEncoding.UTF8.GetString(Template);
  var ReplacedData := ParseString(Data, function(varName: string): string
    begin
      if varName = 'package-name' then exit(Package.Name);
      if varName = 'lib-suffix' then exit(GetLibSuffix(IDEName));
      if varName = 'requires' then exit(GetDpkSection(Package.Requires, function(s: string): string begin Result := TPath.GetFileNameWithoutExtension(s); end));

      if varName = 'contains' then exit(GetDpkSection(Package.Files, function(s: string): string begin Result := GetDpkContain(s); end));
      if varName = 'guid' then exit(GuidToStringN(TGuid.NewGuid));
      if varName = 'framework-type' then exit(Package.FrameworkType);
      if varName = 'targeted-platforms' then exit(TargetedPlatforms(IDEName, Package.Platforms));
      if varName = 'cpp-output' then exit(GetCppOutput(Package.SupportsCpp));
      if varName = 'dcc-references' then exit(GetDccReferences([Package.Requires, Package.Files]));
      if varName = 'plat-android' then exit(HasPlatform(TPlatform.android32, Package.Platforms));
      if varName = 'plat-android64' then exit(HasPlatform(TPlatform.android64, Package.Platforms));
      if varName = 'plat-iosdevice64' then exit(HasPlatform(TPlatform.iosdevice64, Package.Platforms));
      if varName = 'plat-iossimarm64' then exit(HasPlatform(TPlatform.iossimulator64arm, Package.Platforms));
      if varName = 'plat-linux64' then exit(HasPlatform(TPlatform.linux64, Package.Platforms));
      if varName = 'plat-osx64' then exit(HasPlatform(TPlatform.macos64intel, Package.Platforms));
      if varName = 'plat-osxarm64' then exit(HasPlatform(TPlatform.macos64arm, Package.Platforms));
      if varName = 'plat-win32' then exit(HasPlatform(TPlatform.win32intel, Package.Platforms));
      if varName = 'plat-win64' then exit(HasPlatform(TPlatform.win64intel, Package.Platforms));
      if varName = 'plat-Win64x' then exit(HasPlatform(TPlatform.win64Xintel, Package.Platforms));
      raise Exception.Create('Unknown variable: ' + varName);
    end);

  var Encoding := TUTF8Encoding.Create(False);
  try
    TFile.WriteAllText(OutFileName, ReplacedData, TEncoding.UTF8);
  finally
    Encoding.Free;
  end;
end;


procedure ProcessTemplates(const IDEName: TIDEName; const Package: TPackageDefinition; const BaseFileName: string);
begin
  var PackageTemplates := TResourceStream.Create(HInstance, 'PackageTemplates', RT_RCDATA);
  try
    var Zip := TZipFile.Create;
    try
      Zip.Open(PackageTemplates, TZipMode.zmRead);
      var Template: TBytes;
      Zip.Read(DelphiSuffixes[IDEName] + '/Package.dpk', Template);
      ReplaceData(IDEName, Template, Package, BaseFileName + '.dpk');
      Zip.Read(DelphiSuffixes[IDEName] + '/Package.dproj', Template);
      ReplaceData(IDEName, Template, Package, BaseFileName + '.dproj');

    finally
      Zip.Free;
    end;

  finally
    PackageTemplates.Free;
  end;

end;

procedure CreatePackages(const IDENames: TArray<TIDEName>);
begin
end;
end.

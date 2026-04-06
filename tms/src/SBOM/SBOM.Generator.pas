unit SBOM.Generator;
{$i ../../../common/src/tmscommon.inc}

interface
uses UProjectDefinition;

procedure GenSBOMFromProject(const AProject: TProjectDefinition; const AOutputFile: string);

implementation
uses
  System.SysUtils, System.IOUtils, System.JSON, System.DateUtils;

const
  SBomFormat   = 'CycloneDX';
  SSpecVersion = '1.5';
  SEcosystem   = 'smartsetup';

function BuildPurl(const AId, AVersion: string): string;
begin
  Result := 'pkg:' + SEcosystem + '/' + StringReplace(AId, ' ', '%20', [rfReplaceAll]);
  if AVersion <> '' then
    Result := Result + '@' + AVersion;
end;

function BuildLicenseJson(const ALicense: string): TJSONArray;
begin
  Result := TJSONArray.Create(
    TJSONObject.Create.AddPair('license',
      TJSONObject.Create.AddPair('name', ALicense)));
end;

procedure GenSBOMFromProject(const AProject: TProjectDefinition; const AOutputFile: string);
begin
  var App := AProject.Application;
  var ProjPurl := BuildPurl(App.Id, App.Version);

  var Root := TJSONObject.Create;
  try
    Root.AddPair('bomFormat', SBomFormat);
    Root.AddPair('specVersion', SSpecVersion);
    Root.AddPair('version', TJSONNumber.Create(1));

    var G: TGUID;
    CreateGUID(G);
    Root.AddPair('serialNumber',
      'urn:uuid:' + LowerCase(Copy(GUIDToString(G), 2, 36)));

    // --- metadata ---
    var Meta := TJSONObject.Create;
    Root.AddPair('metadata', Meta);

    Meta.AddPair('timestamp',
      FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"',
        TTimeZone.Local.ToUniversalTime(Now), TFormatSettings.Invariant));

    var Tools := TJSONArray.Create;
    Meta.AddPair('tools', Tools);
    Tools.AddElement(
      TJSONObject.Create
        .AddPair('vendor', 'TMS Software')
        .AddPair('name', 'TMS Smart Setup'));

    // metadata.component
    var ProjComp := TJSONObject.Create;
    Meta.AddPair('component', ProjComp);

    ProjComp.AddPair('type', 'library');
    ProjComp.AddPair('name', App.Id);
    if App.Version <> '' then
      ProjComp.AddPair('version', App.Version);
    if App.Description <> '' then
      ProjComp.AddPair('description', App.Description);
    if App.Copyright <> '' then
      ProjComp.AddPair('copyright', App.Copyright);
    if App.CompanyName <> '' then
      ProjComp.AddPair('publisher', App.CompanyName);

    ProjComp.AddPair('purl', ProjPurl);

    // external references from application metadata
    var ExtRefs: TJSONArray := nil;
    if App.Docs <> '' then
    begin
      if ExtRefs = nil then ExtRefs := TJSONArray.Create;
      ExtRefs.AddElement(
        TJSONObject.Create
          .AddPair('type', 'documentation')
          .AddPair('url', App.Docs));
    end;
    if App.Url <> '' then
    begin
      if ExtRefs = nil then ExtRefs := TJSONArray.Create;
      ExtRefs.AddElement(
        TJSONObject.Create
          .AddPair('type', 'website')
          .AddPair('url', App.Url));
    end;
    if ExtRefs <> nil then
      ProjComp.AddPair('externalReferences', ExtRefs);

    // --- components (dependencies) ---
    var CompsArr := TJSONArray.Create;
    Root.AddPair('components', CompsArr);

    for var Dep in AProject.Dependencies do
    begin
      var Obj := TJSONObject.Create;
      Obj.AddPair('type', 'library');
      Obj.AddPair('name', Dep.Id);
      Obj.AddPair('purl', BuildPurl(Dep.Id, ''));
      if Dep.Description <> '' then
        Obj.AddPair('description', Dep.Description);
      CompsArr.AddElement(Obj);
    end;

    // --- dependencies ---
    var Deps := TJSONArray.Create;
    Root.AddPair('dependencies', Deps);

    var ProjDep := TJSONObject.Create;
    Deps.AddElement(ProjDep);
    ProjDep.AddPair('ref', ProjPurl);

    var DepOn := TJSONArray.Create;
    ProjDep.AddPair('dependsOn', DepOn);
    for var Dep in AProject.Dependencies do
      DepOn.AddElement(TJSONString.Create(BuildPurl(Dep.Id, '')));

    TFile.WriteAllText(AOutputFile, Root.Format(2), TEncoding.UTF8);
  finally
    Root.Free;
  end;
end;

end.


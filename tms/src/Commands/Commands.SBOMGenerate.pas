unit Commands.SBOMGenerate;

interface
uses
 Classes, StrUtils, System.Generics.Collections, System.SysUtils, UCommandLine;

procedure RegisterSBOMGenerateCommand;

implementation
uses
  Commands.CommonOptions, Commands.Logging, Commands.GlobalConfig, Status.Manager, Deget.CoreTypes,
  Deget.Version, UProjectLoader, UProjectList, SBOM.Generator, IOUtils, UMultiLogger, Masks, JSON,
  UProjectDefinition, UJsonPrinter;

var
  ProductIds: TArray<string>;
  Force: boolean;

function Matches(const ProductId: string): boolean;
begin
  for var Mask in ProductIds do if MatchesMask(ProductId, Mask) then exit(true);
  Result := false;
end;

procedure AddJson(const Root: TJSONObject; const Proj: TProjectDefinition; const CdxFile: string; const Generated: boolean);
begin
  if Root = nil then exit;
  var Item := TJSONObject.Create;
  Root.AddPair(Proj.Application.Id, Item);
  Item.AddPair('filename', CdxFile);
  Item.AddPair('generated', Generated);
end;

procedure SBOMGenerate;
begin
  var Projs := TProjectList.Create;
  try
    var JsonRoot: TJSONObject := nil;
    try
      if Logger.JsonMode then JsonRoot := TJSONObject.Create;

      TProjectLoader.LoadProjects(Config.GetAllRootFolders, Projs);
      Projs.ResolveDependencies;

      for var Proj in Projs.All do
      begin
        if not Matches(Proj.Application.Id) then continue;

        var ProductFolder := Proj.RootFolder;
        if ProductFolder = '' then Continue;
        var CdxFile := TPath.Combine(ProductFolder, Proj.Application.Id + '.cdx.json');
        if (Force or not TFile.Exists(CdxFile)) then
        begin
          Logger.Info('Generating SBOM for "' + Proj.Application.Id + '"...');
          try
            GenSBOMFromProject(Proj, CdxFile);
            Logger.Info('SBOM created: "' + CdxFile + '"');
            AddJson(JsonRoot, Proj, CdxFile, true);
          except
            on E: Exception do
              Logger.Error('Failed to generate SBOM for "' + Proj.Application.Id + '": ' + E.Message);
          end;
        end else
        begin
          Logger.Trace('Skipped generation SBOM for ' + Proj.Application.Id + '. SBOM already exists.');
          AddJson(JsonRoot, Proj, CdxFile, false);
        end;
      end;
      if JsonRoot <> nil then OutputJson(JsonRoot);
    finally
      JsonRoot.Free;
    end;
  finally
    Projs.Free;
  end;
end;

procedure RunSBOMGenerateCommand;
begin
  InitFolderBasedCommand(true);
  SBOMGenerate;
end;


procedure RegisterSBOMGenerateCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('sbom-generate', '', 'Generates a CycloneDX SBOM (.cdx.json) for selected products. By default, it won''t override existing sbom files.',
    'More information: https://doc.tmssoftware.com/smartsetup/reference/tms-sbom-generate.html',
    'sbom-generate <product-ids> [-force] [-json]');

  cmd.Examples.Add('sbom-generate *');
  cmd.Examples.Add('sbom-generate tms.flexcel.vcl tms.biz.aurelius');

  var option := cmd.RegisterUnNamedOption<string>('The ids of the products for which to generate sboms', 'product-ids',
    procedure(const Value: string)
    begin
      ProductIds := ProductIds + SplitString(Value, ',');
    end);
    option.Required := True;
    option.AllowMultiple := True;

  option := cmd.RegisterOption<Boolean>('force', '', 'Rewrite existing SBOM files',
    procedure(const Value: boolean)
    begin
      Force := Value;
    end);
  option.HasValue := False;

  RegisterJsonOption(cmd);


  AddCommand(cmd.Name, CommandGroups.Status, RunSBOMGenerateCommand);
end;
end.

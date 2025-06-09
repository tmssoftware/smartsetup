unit UDefines;
{$i ../../tmssetup.inc}

interface
uses UProjectDefinition, UConfigDefinition, UConfigKeys, Generics.Defaults, Generics.Collections;

  function GetDefinesFilename(const Project: TProjectDefinition): string;
  procedure CreateDefinesInclude(const Config: TConfigDefinition; const Project: TProjectDefinition);

implementation
uses SysUtils, IOUtils, Classes, UMultiLogger, UTmsBuildSystemUtils;

function GetDefinesFilename(const Project: TProjectDefinition): string;
begin
  Result := CombinePath(Project.RootFolder, Project.DefinesFilename);

  if not SameText(TPath.GetExtension(Result), '.inc') then
  begin
    raise Exception.Create('The defines filename must have a ".inc" extension. The name "' + Result + ' is not valid.');
  end;
end;

procedure CreateDefinesInclude(const Config: TConfigDefinition; const Project: TProjectDefinition);
begin
  if not Config.ModifySources(Project.Application.Id) then exit;

  if String.IsNullOrWhiteSpace(Project.DefinesFilename) then exit;

  var sw := TStreamWriter.Create(GetDefinesFileName(Project), false, TEncoding.UTF8);
  try
    sw.WriteLine('// This file was generated automatically by TMS Smart Setup.');
    sw.WriteLine('// Do not modify it, as it will be overwritten every time you rebuild.');
    sw.WriteLine('// To change the values below, modify tms.config.yaml and re-run tms build.');
    var Defines := Config.GetAllDefines(Project.Application.Id);

    for var def in Defines do
    begin
      sw.WriteLine('{$DEFINE ' + def + '}');
    end;

    // Just some check to see if the defines that are done exactly for one product are allowed.
    var ProductOnlyDefines := Config.GetDefinesOnlyForProject(Project.Application.Id);
    for var def in ProductOnlyDefines do
    begin
      if not Project.Defines.ContainsKey(def) then
      begin
        Logger.Info('Note: The define "' + def + '" is not in the list of allowed defines for ' + Project.Application.NameAndVersion
           + '. It will probably be ignored. The list of possible defines for ' +  Project.Application.Name + ' is ['
           + Project.ListDefines
           + '].') ;
      end;
    end;
  finally
    sw.Free;
  end;
end;

end.

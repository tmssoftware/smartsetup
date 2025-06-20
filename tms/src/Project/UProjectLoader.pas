unit UProjectLoader;
{$i ../../tmssetup.inc}

interface
uses UProjectDefinition, BBYaml, UProjectLoaderStateMachine, IOUtils, Generics.Collections, UProjectList;
type

  TProjectLoader = class
  public const
    TMSBuildDefinitionFile = 'tmsbuild.yaml';
  private
    class procedure LoadIntoProject(const Filename: string; const Project: TProjectDefinition; const aStopAt: string; const aIgnoreOtherFiles: boolean);
  public
    class procedure LoadProjects(const Roots: TArray<string>; const Projects: TProjectList; const aStopAt: string = ''; const aIgnoreOtherFiles: boolean = false);
    class function LoadProjectDefinition(const RootFolder: string; const aStopAt: string = ''; const aIgnoreOtherFiles: boolean = false): TProjectDefinition;
    class function HasProjectDefinition(const RootFolder: string): Boolean;
    class procedure LoadDataIntoProject(const Filename, Data: string; const Project: TProjectDefinition; const aStopAt: string; const aIgnoreOtherFiles: boolean);
  end;

implementation
uses Classes, SysUtils, UMultiLogger, UTmsBuildSystemUtils, ULogger;

{ TProjectLoader }

class function TProjectLoader.HasProjectDefinition(const RootFolder: string): Boolean;
begin
  Result := TFile.Exists(TPath.Combine(RootFolder, TMSBuildDefinitionFile));
end;

class procedure TProjectLoader.LoadDataIntoProject(const Filename, Data: string;
  const Project: TProjectDefinition; const aStopAt: string;
  const aIgnoreOtherFiles: boolean);
var
  MainSection: TMainSectionDef;
begin
  MainSection := TMainSectionDef.Create(Project);
  try
    var Reader := TStringReader.Create(Data);
    try
      TBBYamlReader.ProcessStream(Reader, Filename, MainSection, aStopAt, aIgnoreOtherFiles);
    finally
      Reader.Free;
    end;
  finally
    MainSection.Free;
  end;
end;

class procedure TProjectLoader.LoadIntoProject(const Filename: string; const Project: TProjectDefinition; const aStopAt: string; const aIgnoreOtherFiles: boolean);
var
  MainSection: TMainSectionDef;
begin
  MainSection := TMainSectionDef.Create(Project);
  try
    TBBYamlReader.ProcessFile(Filename, MainSection, aStopAt, aIgnoreOtherFiles);
  finally
    MainSection.Free;
  end;
end;

class function TProjectLoader.LoadProjectDefinition(const RootFolder: string; const aStopAt: string; const aIgnoreOtherFiles: boolean): TProjectDefinition;
begin
  var FileName := TPath.Combine(RootFolder, TMSBuildDefinitionFile);
  Result := TProjectDefinition.Create(FileName);
  try
    TProjectLoader.LoadIntoProject(FileName, Result, aStopAt, aIgnoreOtherFiles);
  except
    Result.Free;
    raise;
  end;
end;

class procedure TProjectLoader.LoadProjects(const Roots: TArray<string>;
  const Projects: TProjectList; const aStopAt: string; const aIgnoreOtherFiles: boolean);
begin
  Logger.StartSection(TMessageType.Load, 'Project loading');
  try
    for var Filepath in Roots do
    begin
      if (Filepath = '') or not TDirectory.Exists(Filepath) then continue;

      var files := TList<string>.Create;
      try
        FindProjects(Filepath, TMSBuildDefinitionFile, files, false);
        for var i := 0 to files.Count - 1 do
        begin
          Projects.Add(TProjectDefinition.Create(files[i]));
          Logger.Trace('Loading project ' + files[i]);
          LoadIntoProject(files[i], Projects.LastUnresolved, aStopAt, aIgnoreOtherFiles);
          Logger.Trace('Loaded project ' + Projects.LastUnresolved.Application.Id);
        end;
      finally
        files.Free;
      end;
    end;
  except
    Logger.FinishSection(TMessageType.Load, true);
    raise;
  end;
  Logger.FinishSection(TMessageType.Load, false);
end;

end.

unit UProjectLoader;
{$i ../../tmssetup.inc}

interface
uses UProjectDefinition, BBYaml, BBCmd, UProjectLoaderStateMachine, IOUtils, Generics.Collections, UProjectList;
type

  TProjectLoader = class
  public const
    TMSBuildDefinitionFile = 'tmsbuild.yaml';
  private
    class procedure LoadIntoProject(const Filename: string; const Project: TProjectDefinition; const aStopAt: string; const aIgnoreOtherFiles: boolean; const CmdParameters: TArray<string>);
  public
    class procedure LoadProjects(const Roots: TArray<string>; const Projects: TProjectList; const aStopAt: string = ''; const aIgnoreOtherFiles: boolean = false);
    class function LoadProjectDefinition(const RootFolder: string; const aStopAt: string = ''; const aIgnoreOtherFiles: boolean = false): TProjectDefinition;
    class function LoadProjectDefinitionFromFile(const FileName: string; const aStopAt: string = ''; const aIgnoreOtherFiles: boolean = false; const CmdParameters: TArray<string> = nil): TProjectDefinition;
    class function GetProjectDefinition(const RootFolder: string): string;
    class procedure LoadDataIntoProject(const Filename, Data: string; const Project: TProjectDefinition; const aStopAt: string; const aIgnoreOtherFiles: boolean);
  end;

implementation
uses Classes, SysUtils, UMultiLogger, UTmsBuildSystemUtils, ULogger;

{ TProjectLoader }
function GetEmptySubfolder(const Path: string): string;
begin
  Result := '';
  if not TDirectory.Exists(Path) then exit;

  var SubFolders := TDirectory.GetDirectories(Path, '*', TSearchOption.soTopDirectoryOnly);
  for var SubFolder in SubFolders do
  begin
    if Subfolder.StartsWith('.') or SubFolder.StartsWith('__') then continue; //skip folders that svn, vs, delphi or whatever could have added.
    if Result <> '' then exit(''); //If there is more than one valid folder, then this is not supported. The subfolder must be unique.
    Result := SubFolder;
  end;
end;

class function TProjectLoader.GetProjectDefinition(const RootFolder: string): string;
begin
  // tmsbuild.yaml might not be at the very root. For VCS products, we put the product, including tmsbuild.yaml
  // inside a folder, so other files like tmsfetch.info.txt don't pollute the repo.
  Result := RootFolder;
  while true do
  begin
    if not TDirectory.Exists(Result) then exit('');
    if TFile.Exists(TPath.Combine(Result, TMSBuildDefinitionFile)) then exit;
    var SubFolder := GetEmptySubfolder(Result);
    if SubFolder = '' then exit('');
    Result := TPath.Combine(Result, SubFolder);
  end;
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

class procedure TProjectLoader.LoadIntoProject(const Filename: string; const Project: TProjectDefinition; const aStopAt: string; const aIgnoreOtherFiles: boolean; const CmdParameters: TArray<string>);
var
  MainSection: TMainSectionDef;
begin
  MainSection := TMainSectionDef.Create(Project);
  try
    MainSection.CreatedBy := 'Main: ' + Filename;
    TBBYamlReader.ProcessFile(Filename, MainSection, aStopAt, aIgnoreOtherFiles);

    MainSection.CreatedBy := 'Command line';
    TBBCmdReader.ProcessCommandLine(CmdParameters, MainSection, ':', false);
  finally
    MainSection.Free;
  end;
end;

class function TProjectLoader.LoadProjectDefinition(const RootFolder: string; const aStopAt: string; const aIgnoreOtherFiles: boolean): TProjectDefinition;
begin
  Result := LoadProjectDefinitionFromFile(TPath.Combine(RootFolder, TMSBuildDefinitionFile), aStopAt, aIgnoreOtherFiles);
end;

class function TProjectLoader.LoadProjectDefinitionFromFile(const FileName: string; const aStopAt: string; const aIgnoreOtherFiles: boolean; const CmdParameters: TArray<string>): TProjectDefinition;
begin
  Result := TProjectDefinition.Create(FileName);
  try
    TProjectLoader.LoadIntoProject(FileName, Result, aStopAt, aIgnoreOtherFiles, CmdParameters);
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
          LoadIntoProject(files[i], Projects.LastUnresolved, aStopAt, aIgnoreOtherFiles, nil);
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

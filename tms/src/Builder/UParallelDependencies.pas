unit UParallelDependencies;
{$i ../../tmssetup.inc}

interface
uses UBuildInfo, UParallelProjectInfo, Deget.CoreTypes;

procedure ResolveDependencies(const BuildInfo: TBuildInfo; const ParallelProjects: TParallelProjectInfoList);

implementation
uses Generics.Collections, UProjectDefinition, UCoreTypes;

procedure ResolveDependencies(const BuildInfo: TBuildInfo; const ParallelProjects: TParallelProjectInfoList);
begin
  for var ProjectInfo in BuildInfo.ProjectsToBuild do
  begin
    for var ide in ProjectInfo.IDEsBuildInfo do
    begin
      for var plat in ide.PlatformsBuildInfo do
      begin
        ParallelProjects.Add(ProjectInfo.ProjectId, IDEId[ide.Name], PlatformId[plat.Name], TParallelProjectInfo.Create);
      end;
    end;
  end;

  for var ProjectInfo in BuildInfo.ProjectsToBuild do
  begin
    var Duplicated := TDictionary<string, boolean>.Create;
    try
      var MainProjectId := ProjectInfo.ProjectId;
      Duplicated.Add(MainProjectId, true);
      BuildInfo.ProjectList.LoopDependencies(
        procedure (pd: TProjectDefinition)
        begin
          if Duplicated.ContainsKey(pd.Application.Id) then exit;
          Duplicated.Add(pd.Application.Id, true);

          var PInfo := BuildInfo.ProjectToProjectBuildInfo[pd.Application.Id];
          for var ide in PInfo.IDEsBuildInfo do
          begin
            for var plat in ide.PlatformsBuildInfo do
            begin
              ParallelProjects.AddDep(MainProjectId, pd.Application.Id, IDEId[ide.Name], PlatformId[plat.Name]);
            end;
          end;

        end
        , ProjectInfo.Project, nil);

    finally
      Duplicated.Free;
    end;
  end;
end;
end.

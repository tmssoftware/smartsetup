unit UBuildSummary;
{$i ../../tmssetup.inc}

interface
uses UBuildInfo, Deget.CoreTypes;

procedure LogBuildSummary(const BuildInfo: TBuildInfo);
procedure LogSkipped(const BuildInfo: TBuildInfo);

implementation
uses ULogger, UMultiLogger, UInstaller, UProjectBuildInfo, SysUtils,
     Generics.Collections, UUninstallInfo, UProjectInstallerConstants;

procedure LogSkippedGeneric(const BuildInfo: TBuildInfo; const NoteTypes: TNoteTypeSet; const Header: string; var HasSDKs: boolean);
begin
  var Duplicates := TDictionary<string, boolean>.Create;
  try
    var Consolidated := TList<TBuildNotes.TBuildNotesData>.Create;
    try
      for var BuildProject in BuildInfo.ProjectsToBuild do
      begin
        BuildProject.Notes.PrepareLog(Duplicates, Consolidated, NoteTypes);
        if BuildProject.Notes.HasSDK then HasSDKs := true;
      end;

      if Consolidated.Count > 0 then
      begin
        Logger.Info('');
        Logger.Info(Header);
      end;
      TBuildNotes.Log(procedure(s: string) begin Logger.Info(s); end, Consolidated);
    finally
      Consolidated.Free;
    end;
  finally
    Duplicates.Free;
  end;

end;

procedure LogSkipped(const BuildInfo: TBuildInfo);
begin
  Logger.StartSection(TMessageType.Skipped, 'Skipped');
  try
    var HasSDKs := false;
    LogSkippedGeneric(BuildInfo, [TNoteType.SkippedIDE..TNoteType.MissingSDK], '--- Skipped IDEs ---', HasSDKs);
    LogSkippedGeneric(BuildInfo, [TNoteType.SkippedProduct], '--- Skipped products in platforms ---', HasSDKs);
    if (HasSDKs) then
    begin
      Logger.Info('');
      Logger.Info('Some platforms were skipped because the SDKs for them are not available.');
      Logger.Info('To compile for those platforms: Open Rad Studio, create a new Multi-Device application,');
      Logger.Info('set it to the platform you want, and deploy it. This will download the SDKs for the platform.');
    end;
  finally
    Logger.FinishSection(TMessageType.Skipped, false);
  end;
end;

procedure DumpUninstalledIde(const proj: TProjectBuildInfo; const ide: TIDEName; const DumpIdeToo: boolean);
begin
  if proj.PlatformsToUninstall.Get(ide).Count > 0 then
  begin
    if DumpIdeToo then Logger.Info('    - ' + TInstallerFactory.GetInstaller(ide).DisplayName);

    for var platu in proj.PlatformsToUninstall.Get(ide) do
    begin
      Logger.Info('      - ' + PlatformId[platu.Platform] + ' -> UNINSTALLED.');
    end;
  end;
end;

procedure DumpUninstalled(const proj: TProjectBuildInfo);
begin
  var IdesFullyUninstalled: TIDENameSet := [];
  for var ideu in proj.IDEsToUninstall do
  begin
    Logger.Info('    - ' + TInstallerFactory.GetInstaller(ideu.IDE).DisplayName + ' -> UNINSTALLED.');
    Include(IdesFullyUninstalled, ideu.IDE);
  end;

  for var ide := Low(TIDEName) to High(TIDEName) do
  begin
    if ide in IdesFullyUninstalled then continue;
    DumpUninstalledIde(proj, ide, true);
  end;
end;

function MixedResults(const AllFailed: boolean): string;
begin
  if (AllFailed) then exit(' -> FAILED.');

  Result := ''
end;

procedure LogBuildSummary(const BuildInfo: TBuildInfo);
begin
  Logger.StartSection(TMessageType.Summary, 'Build Summary');
  Logger.Info('');
  Logger.Info('=== Build Summary ===');
  for var skipped in BuildInfo.ProjectsSkippedByConfig do
    if not BuildInfo.IsProjectToBuild(skipped.Application.Id) and not BuildInfo.IsProjectToUninstall(skipped.Application.id) then
      Logger.Info('  - ' + skipped.Application.NameAndVersion + ' -> IGNORED.'); //by configuration.

  var SkippedBecauseNotModified := false;

  for var proj in BuildInfo.ProjectsToBuild do
  begin
    if (proj.IDEsBuildInfo.Count = 0) and (not proj.HasSomethingToUninstall) then
    begin
      if Proj.Skipped.Empty then
      begin
        Logger.Info('  - ' + proj.Project.Application.NameAndVersion + ' -> SKIPPED.'); //nothing to build.
      end else
      begin
        Logger.Info('  - ' + proj.Project.Application.NameAndVersion + ' -> NOT MODIFIED.'); //not modified.
        SkippedBecauseNotModified := true;
      end;

    end;
  end;

  for var uninst in BuildInfo.ProjectsToUninstall do
  begin
    var ProjectName := uninst.Value.ReadStr(InstallerConstants.ProjectNameJsonId, uninst.ProjectId);

    Logger.Info('  - ' + ProjectName + ' -> UNINSTALLED.');
  end;

  for var proj in BuildInfo.ProjectsToBuild do
  begin
    if (proj.IDEsBuildInfo.Count = 0) and (proj.HasSomethingToUninstall) then
    begin
      Logger.Info('  - ' + proj.Project.Application.NameAndVersion); //nothing to build.
      DumpUninstalled(proj);
    end;
  end;

  for var proj in BuildInfo.ProjectsToBuild do
  begin
    if (proj.IDEsBuildInfo.Count > 0) and proj.AllOk then
    begin
      Logger.Info('  - ' + proj.Project.Application.NameAndVersion + ' -> OK.');
      DumpUninstalled(proj);
    end;
  end;

  for var proj in BuildInfo.ProjectsToBuild do
  begin
    if (proj.IDEsBuildInfo.Count > 0) and not proj.AllOk then
    begin
      Logger.Info('  - ' + proj.Project.Application.NameAndVersion + MixedResults(proj.AllFailed));
      for var ide in proj.IDEsBuildInfo do
      begin
        if ide.AllOk then
        begin
          Logger.Info('    - ' + TInstallerFactory.GetInstaller(ide.Name).DisplayName + ' -> OK.');
          DumpUninstalledIde(proj, ide.Name, false);
        end;
      end;
      for var ide in proj.IDEsBuildInfo do
      begin
        if not ide.AllOk then
        begin
          Logger.Info('    - ' + TInstallerFactory.GetInstaller(ide.Name).DisplayName + MixedResults(ide.AllFailed));
          DumpUninstalledIde(proj, ide.Name, false);

          for var plat in ide.PlatformsBuildInfo do
          begin
            if plat.Ok then
            Logger.Info('      - ' + PlatformId[plat.Name] + ' -> OK.');
          end;
          for var plat in ide.PlatformsBuildInfo do
          begin
            if not plat.Ok then
            Logger.Info('      - ' + PlatformId[plat.Name] + ' -> FAILED.');
          end;
        end;
      end;
    end;
  end;

  if SkippedBecauseNotModified then
  begin
    Logger.Info('');
    Logger.Info('To force rebuild libraries skipped because they were not modified, you can use "tms build -full".');
  end;

  Logger.FinishSection(TMessageType.Summary);

end;

end.

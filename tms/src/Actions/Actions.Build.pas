unit Actions.Build;

interface

procedure ExecuteBuildAction(FullBuild: Boolean; OnlyUnregister: Boolean = False); overload;
procedure ExecuteBuildAction(ProductIdsAndVersions: TArray<string>; FullBuild: Boolean; OnlyUnregister: Boolean = False); overload;
procedure RemoveFromWindowsPathIfNoProducts; overload;

implementation

uses
  System.SysUtils, System.Diagnostics, System.Threading, System.IOUtils, ULogger, UMultiLogger, UFileHasher, UInsomnia,
  UConfigKeys, UProjectAnalyzer, Commands.GlobalConfig, Commands.Termination, UProjectList, UProjectLoader,
  UAppTerminated, UCheckForOldVersions, UProjectUninstaller, UProjectBuilderInterface, UConfigDefinition,
  UParallelProjectBuilder, UWindowsPath, UEnvironmentPath, Fetching.ProductVersion,
  {$IFDEF MSWINDOWS}
  Windows, Messages,
  {$ENDIF}

  Package.Creator, UBuildSummary, Megafolders.Manager;


procedure RemoveFromWindowsPathIfNoProducts(const ProjectCount: integer); overload;
begin
{$IFDEF MSWINDOWS}
    if ProjectCount = 0 then
    begin
      var LinkedFolder := Config.Folders.BplFolder;
      var RemovedPaths := RemoveFromWindowsPathWithChildren(LinkedFolder);
      for var Removed in RemovedPaths do Logger.Info('Removed "' + Removed + '" from the Windows Path');
      RemoveFromEnviromnentOverrides;
    end;
{$ENDIF}
end;

procedure RemoveFromWindowsPathIfNoProducts; overload;
begin
  var Projects := TProjectList.Create;
  try
    TProjectLoader.LoadProjects(Config.GetAllRootFolders, Projects, 'root:minimum required tmsbuild version', true);
    RemoveFromWindowsPathIfNoProducts(Projects.All.Count);
  finally
    Projects.Free;
  end;
end;

function GetProjectBuilder(const Config: TConfigDefinition; const FileHasher: TFileHasher): IProjectBuilder;
begin
  Result := TParallelProjectBuilder.Create(Config, FileHasher);
end;

procedure ExecuteBuildAction(ProductIdsAndVersions: TArray<string>; FullBuild: Boolean; OnlyUnregister: Boolean = False); overload;
begin
  var ProductIds := ParseVersions(ProductIdsAndVersions);
  if Length(ProductIds) > 0 then
  begin
    //Do not clear Excluded, only included.
    //Config.ClearExcludedComponents;
    Config.ClearIncludedComponents;
    for var ProductId in ProductIds do
    begin
      Config.AddIncludedComponent(ProductId.IdMask, '');
    end;
  end;
  ExecuteBuildAction(FullBuild, OnlyUnregister);
end;


// This 2 methods are in UWinMessages.pas
// But we don't want deps between smartsetup and tmsrefresh, so we will duplicate them.
// UWinMessages.pas must have all files in its folder to be distributed with smarsetup,
// And we don't want smartsetup to link to files in a tmsrefresh folder.
procedure PostWinMessage(const WindowID: string; const MessageId: NativeUInt);
begin
{$IFDEF MSWINDOWS}
  var WinHandle: HWND := 0;
  while true do
  begin
    WinHandle := FindWindowEx(HWND_MESSAGE, WinHandle, 'STATIC', PWideChar(WindowID));
    if (WinHandle = 0) then break;

    PostMessage(WinHandle,  MessageId, 0, 0);
  end;
{$ENDIF}
end;

function RegisterWinMessage(const MessageIdStr: string): NativeUInt;
begin
{$IFDEF MSWINDOWS}
  Result := RegisterWindowMessage(PWideChar(MessageIdStr));
{$ELSE}
  Result := NativeUINT(-1);
{$ENDIF}
end;

procedure NotifyTmsRefresh;
// Those 2 constants are defined in UTmsRefreshConsts.pas
// But we don't want deps between smartsetup and tmsrefresh, so we will duplicate them.
const
  tmsRefreshWindowID = 'tms.tmsrefresh.actions';
  tmsRefreshMessageRefresh = 'tms.tmsrefresh.refreshpackages';
begin
 var RefreshMessageId := RegisterWinMessage(tmsRefreshMessageRefresh);
  PostWinMessage(tmsRefreshWindowID, RefreshMessageId);
end;

procedure ExecuteBuildAction(FullBuild: Boolean; OnlyUnregister: Boolean = False);
begin
  EnableCtrlCTermination;

  var StopWatch := TStopwatch.Create;
  StopWatch.Start;

  var Projs := TProjectList.Create;
  try
    Logger.Info('Starting build...');
    if TFile.Exists(ConfigFileName) then
      Logger.Info('Configuration file: ' + ConfigFileName)
    else
      Logger.Info('No configuration file found');
    Logger.Info('');

    var FileHasher := TFileHasher.Create(Config);
    try
      if Config.PreventSleep then
      begin
        if not SleepIsAllowedByPolicy then Logger.Trace('Smart setup is configured to prevent sleep, but the Group Policies of this machine don''t seem to allow disabling sleep by apps.'+
         ' For this to work you need to set "Allow applications to prevent automatic sleep (plugged in)" to true.');

        PreventSleep;
      end;

      if FullBuild then
      begin
        var AllIncluded: boolean;
        FileHasher.CleanHashes(false, AllIncluded); //even in a dry-run. So we can rebuild everything
        if AllIncluded then TMegafolderManager.RemoveAll(Config.Folders.DcuMegafolder); // do not remove if not building everything.
      end else
      begin
        TMegafolderManager.RemoveUnused(Config.Folders.DcuMegafolder, Config.DcuMegafolders);
        if TMegafolderManager.ChangedMegafolders(Config.DcuMegafolders, Config.Folders.DcuMegafolder) then
        begin
          TMegafolderManager.RemoveAll(Config.Folders.DcuMegafolder);
          //In this case, we will rebuild everything. See https://github.com/tmssoftware/tms-smartsetup/discussions/193#discussioncomment-13632149
          var AllIncluded: boolean;
          FileHasher.CleanHashes(true, AllIncluded);
        end;
      end;

      // if we are unregistering the products, the easiest way is to "simulate" the deletion of all projects
      // This is done by simply not loading the list of existing projects
      if not OnlyUnregister then
        TProjectLoader.LoadProjects(Config.GetAllRootFolders, Projs);

      Projs.ResolveDependencies;

      RemoveFromWindowsPathIfNoProducts(Projs.All.Count);

      CreatePackages(Projs);
      var ProjectAnalyzer := TProjectAnalyzer.Create(Config, Projs, FileHasher);
      try
        Logger.StartSection(TMessageType.Analyze, 'Analyzing');
        try
          Logger.Info('Analyzing products...');
          ProjectAnalyzer.Analyze(Projs.Resolved);
          Logger.Info('Ok');
          CheckAppTerminated;
          ProjectAnalyzer.AnalyzeUnusedPackages;
          CheckOlderVersions(ProjectAnalyzer.BuildInfo.ProjectsToBuild);
        except
          Logger.FinishSection(TMessageType.Analyze, true);
          raise;
        end;
        Logger.FinishSection(TMessageType.Analyze, false);

        // do the build process
        begin
          var ProjectUninstaller := TProjectUninstaller.Create(Config);
          try
            CheckAppTerminated;
            ProjectUninstaller.Uninstall(ProjectAnalyzer.BuildInfo);
          finally
            ProjectUninstaller.Free;
          end;

          var ProjectBuilder := GetProjectBuilder(Config, FileHasher);
          CheckAppTerminated;
          ProjectBuilder.Build(ProjectAnalyzer.BuildInfo);
        end;

        NotifyTmsRefresh;

        LogBuildSummary(ProjectAnalyzer.BuildInfo);
        if Config.ErrorIfSkipped then
        begin
          if not ProjectAnalyzer.BuildInfo.AllOkAndNoSkipped then
          begin
            ExitCode := 3;
            Logger.Info('');
            Logger.Error('* There were errors building; or some IDEs or Platforms where skipped, and "error if skipped" is set in the configuration.');
            Logger.Info('');
          end;
        end
        else
        begin
          if not ProjectAnalyzer.BuildInfo.AllOk then ExitCode := 3;
        end;

      finally
        ProjectAnalyzer.Free;
      end;
    finally
      FileHasher.Free;
    end;
  finally
    Projs.Free;
  end;

  StopWatch.Stop;

  Logger.Info('');
  Logger.Info('Build finished. Elapsed time: ' + StopWatch.Elapsed.ToString);
end;

end.

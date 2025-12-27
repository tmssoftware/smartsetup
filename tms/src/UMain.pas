unit UMain;

interface

uses
  Commands.CommonOptions,
  Commands.Install,
  Commands.Uninstall,
  Commands.Update,
  Commands.List,
  Commands.ListRemote,
  Commands.VersionsRemote,
  Commands.Credentials,
  Commands.Config,
  Commands.ConfigWrite,
  Commands.ConfigRead,
  Commands.ServerEnable,
  Commands.ServerList,
  Commands.ServerAdd,
  Commands.ServerRemove,
  Commands.Fetch,
  Commands.Build,
  Commands.SelfUpdate,
  Commands.Info,
  Commands.Doctor,
  Commands.LogView,
  Commands.Uncompress,
  Commands.Pin,
  Commands.Snapshot,
  Commands.Restore;

procedure Run;

implementation

uses
  System.SysUtils, System.IOUtils, UCommandLine, Commands.Logging, Commands.GlobalConfig, UConfigDefinition,
  UMultiLogger, UConsoleLogger, ULogger, UTmsBuildSystemUtils, Deget.CoreTypes,
  UConfigKeys, Diagnostics, UDelayedErrors, Fetching.InfoFile;

procedure Cleanup;
begin
  try
    //We could recursively remove everything in the temp folder, but it feels safer to explicitly name what to remove.
    //Also, ParallelFolder shouldn't be cleaned, we might be in the middle of a partial build.
    //Also if we changed the logic to allow it to be cleaned, it should be cleaned only if RemoveParallelFolders in the YAML is off.
    //The real issue with ParallelFolder is that it doesn't belong to temp in that it is not always 100% ok to delete it, as for example LockedFileFolders it.
    //It doesn't really belong in temp, but the reason we created this temp folder was so we could put it there. See https://github.com/tmssoftware/tms-smartsetup/issues/97
    TryToDeleteAllFilesInFolderIgnoringLocked(Config.Folders.LockedFilesFolder);
    TryToDeleteAllFilesInFolderIgnoringLocked(Config.Folders.ProductsTempFolder);
    TryToDeleteAllFilesInFolderIgnoringLocked(Config.Folders.CompileTempFolder);
    TryToDeleteAllFilesInFolderIgnoringLocked(Config.Folders.VCSTempFolder, true, true);
    TryToDeleteAllFilesInFolderIgnoringLocked(Config.Folders.ZipFileTempFolder);

    // self-update
    TryToDeleteAllFilesInFolderIgnoringLocked(TPath.Combine(TPath.GetDirectoryName(paramstr(0)), '.locked'), false, true, '*' + TempExtension);
  except on ex: Exception do
    //just ignore it.
    Logger.Trace('Error cleaning up lock folder. ' + ex.Message);
  end;
end;

var
  NewSmartSetupVersion: string = '';
  ConsoleLogger: TConsoleLogger;

procedure Start;
begin
  // Register common command-line stuff (help command, for example)
  RegisterCommonOptions;

  // Register global options (valid for all commands)
  RegisterGlobalOptions;

  // Register actual commands
  RegisterInstallCommand;
  RegisterUpdateCommand;
  RegisterFetchCommand;
  RegisterBuildCommand;
  RegisterUninstallCommand;
  RegisterPinCommand;
  RegisterUnpinCommand;

  RegisterListCommand;
  RegisterListRemoteCommand;
  RegisterVersionsRemoteCommand;
  RegisterInfoCommand;

  RegisterSnapshotCommand;
  RegisterRestoreCommand;

  RegisterCredentialsCommand;
  RegisterConfigCommand;
  RegisterConfigWriteCommand;
  RegisterConfigReadCommand;

  RegisterServerEnableCommand;
  RegisterServerListCommand;
  RegisterServerAddCommand;
  RegisterServerRemoveCommand;

  RegisterDoctorCommand;

  RegisterVersionCommand;
  RegisterSelfUpdateCommand;
  RegisterLogViewCommand;
  RegisterUncompressCommand;

  // Execute
  ParseCommandLine(procedure
    begin
      if IsValidTMSSetupFolder then
      begin
        CleanUp;
        ConsoleLogger.DisplayOptions := ConsoleLogDisplayOptions;
        try
          if not VerbositySetByCommandLine then Logger.Verbosity := Config.Verbosity(GlobalProductId, Logger.Verbosity);
        except on ex: Exception do
          // We clear the error so "tms config" can work even if the config file is wrong.
          Logger.Trace('Error setting the Verbosity: ' + ex.Message);
        end;

        NewSmartSetupVersion := NewSmartSetupAvailable; //This shouldn't use the log at all, but in case that some change ends up referencing the logger, we call it after it is created.
      end;
    end);
end;

procedure SaveAutoSnapshots;
begin
  try
    if not TFetchInfoFile.ProductsWereModified then exit;
    for var Snapshot in Config.FullAutoSnapshotFileNames do
    begin
      var FileName := Snapshot.Trim;
      if FileName = ''  then continue;
      TakeSnapshot(FileName);
    end;
  except on ex: Exception do
    raise Exception.Create('Error creating snapshot: ' + ex.message);
  end;
end;

procedure LogDelayedErrors;
begin
  if (DelayedErrors = nil) or (DelayedErrors.Count = 0) then exit;
  Logger.Error('');
  for var Error in DelayedErrors do
  begin
    Logger.Error('Error: ' + Error);
  end;
  ExitCode := 1;
end;

procedure Run;
begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  try
    ConsoleLogger := TConsoleLogger.Create(ConsoleLogDisplayOptions, 'TMS Smart Setup'); //Here ConsoleLogDisplayOptions is not yet initialized as we haven't parsed the command line yet :(
    Logger := TMultiLogger.Create([ConsoleLogger]);
    try
      Logger.Verbosity := TVerbosity.info;
      try
        Start;
        SaveAutoSnapshots;
        LogDelayedErrors;
      except
        on E: Exception do
        begin
          // If anything bad happened, set the command-line exit code to -1 to indicate execution failed.
          if ExitCode = 0 then
            ExitCode := 1;

          // Do not output error message for abort exceptions.
          if not (E is EAbort) then
            Logger.Error('Error: ' + E.Message);
        end;
      end;
    finally
      Logger.Free;
    end;


    FinishLogging;
  finally
    AlertAboutNewVersions(NewSmartSetupVersion);
    AlertAboutDiskSpace;
  end;


{$IFDEF DEBUG}
{$IFDEF MSWINDOWS}
{$WARNINGS OFF}
  if (DebugHook <> 0) then ReadLn;
{$WARNINGS ON}
{$ENDIF}
{$ENDIF}
end;

end.

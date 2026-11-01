unit Commands.CleanUp;

interface
procedure Cleanup;

implementation
uses Classes, SysUtils, UTmsBuildSystemUtils, Commands.GlobalConfig, IOUtils, UMultiLogger;

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
    Logger.Trace('Error cleaning up temp folders. ' + ex.Message);
  end;
end;


end.

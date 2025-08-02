unit FileSystem.CleanLockedFolder;
interface
uses SysUtils, Classes, StrUtils;

procedure RegisterCleanLockedFolderCommand;

implementation
uses VSoft.CommandLine.Options, UTmsBuildSystemUtils, IOUtils, UCommandLine;

procedure CleanLockedFolderCommand;
begin
  var RootFolder := GetEnvironmentVariable('TMS_TEST_ROOT_DIR');
  if RootFolder.Trim = '' then raise Exception.Create('The environment variable TMS_TEST_ROOT_DIR is empty.');
  RootFolder := TPath.GetFullPath(RootFolder);
  var LockedFolder := TPath.GetFullPath(TPath.Combine(RootFolder, 'tmp', 'locked'));

  TryToDeleteAllFilesInFolderIgnoringLocked(LockedFolder);
end;


procedure RegisterCleanLockedFolderCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('clean-locked', '', 'Deletes the locked folder.',
    '',
    '');

  AddCommand(cmd.Name, 'FileSystem', CleanLockedFolderCommand);
end;


end.


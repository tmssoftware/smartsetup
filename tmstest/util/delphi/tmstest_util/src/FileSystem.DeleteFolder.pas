unit FileSystem.DeleteFolder;

interface
uses SysUtils, Classes, StrUtils;

procedure RegisterDeleteFolderCommand;

implementation
uses VSoft.CommandLine.Options, UTmsBuildSystemUtils, IOUtils, UCommandLine;

var TargetFolder: string;
var KeepRootFolder: boolean;

procedure DeleteFolderCommand;
begin
  var RootFolder := GetEnvironmentVariable('TMS_TEST_ROOT_DIR');
  if RootFolder.Trim = '' then raise Exception.Create('The environment variable TMS_TEST_ROOT_DIR is empty.');
  RootFolder := TPath.GetFullPath(RootFolder);
  var LockedFolder := TPath.GetFullPath(TPath.Combine(RootFolder, 'tmp', 'locked'));
  var TmpRunFolder := TPath.GetFullPath(TPath.Combine(RootFolder, 'tmp-run'));

  if not StartsText(TmpRunFolder, TargetFolder) then raise Exception.Create('Folder to delete: "' + TargetFolder + '" is outside the root folder: "' + TmpRunFolder + '"');

  WriteLn('Removing folder: "', TargetFolder, '" and moving locked files to "', LockedFolder, '"');
  DeleteFolderMovingToLocked(LockedFolder, TargetFolder, true, KeepRootFolder);
end;


procedure RegisterDeleteFolderCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('delete-folder', '', 'Deletes the folder, or moves to locked folder if not possible.',
    '',
    '');

  var option := cmd.RegisterOption<string>(
    'folder', '', 'The folder to delete',
    procedure(const Value: string)
    begin
      TargetFolder := TPath.GetFullPath(Value);
    end);
  option.Required := True;
  option.AllowMultiple := False;

  option := cmd.RegisterOption<boolean>(
    'keep-root-folder', '', 'If true, the root folder itself won''t be deleted. This is useful if you have a locked file in there.',
    procedure(const Value: boolean)
    begin
      KeepRootFolder := Value;
    end);
    option.HasValue := false;

  AddCommand(cmd.Name, 'FileSystem', DeleteFolderCommand);
end;


end.

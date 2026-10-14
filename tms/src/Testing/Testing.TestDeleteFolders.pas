unit Testing.TestDeleteFolders;

interface
{$IFDEF DEBUG}
uses Classes, SysUtils;
var
  TestDeleteFolderMode: string;
  TestDeleteFolderFolder: string;

procedure RunTestDeleteFolders;
{$ENDIF}


implementation
{$IFDEF DEBUG}
uses IOUtils,
{$IFDEF MSWINDOWS}
      Windows, //avoid a warning about TDirectory.GetCurrentDirectory not inilined.
{$ENDIF}
     Commands.Logging, Removal.FolderDeleter, UTmsBuildSystemUtils;
procedure RunTestDeleteFolders;
begin
  InitFolderBasedCommand;
  if TestDeleteFolderFolder.Trim = '' then raise Exception.Create('Folder must be not empty.');
  CheckFolderInside(TestDeleteFolderFolder, [TDirectory.GetCurrentDirectory]);


  if SameText(TestDeleteFolderMode,'folder-deleter') then
  begin
    var Deleter := TFolderDeleter.Create;
    try
      Deleter.AddFolder(TestDeleteFolderFolder);
      Deleter.DeleteAll;
    finally
      Deleter.Free;
    end;
    exit;
  end;

  if SameText(TestDeleteFolderMode, 'delete-folder-moving-to-locked') then
  begin
    DeleteFolderMovingToLocked(TPath.Combine(TDirectory.GetCurrentDirectory, 'locked'), TestDeleteFolderFolder, true, false);
    exit;
  end;

  if SameText(TestDeleteFolderMode, 'delete-folder-ignore-locked') then
  begin
    TryToDeleteAllFilesInFolderIgnoringLocked(TestDeleteFolderFolder, true, true, '*');
    exit;
  end;

  raise Exception.Create('Unknown delete mode: ' + TestDeleteFolderMode);

end;
{$ENDIF}


end.

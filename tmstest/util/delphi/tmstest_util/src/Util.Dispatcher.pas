unit Util.Dispatcher;

interface
procedure Dispatch;

implementation
uses UCommandLine,
  FileSystem.DeleteFolder,
  FileSystem.CleanLockedFolder;

procedure Dispatch;
begin
  RegisterDeleteFolderCommand;
  RegisterCleanLockedFolderCommand;
  ParseCommandLine;

end;
end.

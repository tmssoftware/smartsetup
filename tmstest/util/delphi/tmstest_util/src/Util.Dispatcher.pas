unit Util.Dispatcher;

interface
procedure Dispatch;

implementation
uses UCommandLine,
  FileSystem.DeleteFolder,
  FileSystem.CleanLockedFolder,
  Convert.SummaryToJson;

procedure Dispatch;
begin
  RegisterDeleteFolderCommand;
  RegisterCleanLockedFolderCommand;
  RegisterConvertSummaryToJsonCommand;
  ParseCommandLine;

end;
end.

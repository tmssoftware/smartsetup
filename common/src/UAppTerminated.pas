unit UAppTerminated;
{$i tmscommon.inc}

interface
  procedure TerminateApp;
  procedure CheckAppTerminated;
  function AppIsTerminated: boolean;
implementation
uses Threading, SysUtils;

var
  [volatile] AppTerminated: boolean;

procedure TerminateApp;
begin
  AppTerminated := true;
end;

procedure CheckAppTerminated;
begin
  if AppTerminated then raise Exception.Create('Operation was canceled by the user.');

  if TTask.CurrentTask <> nil then TTask.CurrentTask.CheckCanceled;
end;

function AppIsTerminated: boolean;
begin
  Result := AppTerminated;
end;

end.

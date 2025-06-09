unit Commands.Termination;

interface

uses
{$IFDEF MSWINDOWS}
   Windows,
{$ENDIF}
  UAppTerminated, UMultiLogger;

procedure EnableCtrlCTermination;

implementation

{$IFDEF MSWINDOWS}
function CtrlCHandler( dwCtrlType: DWORD ): BOOL; stdcall;
begin
  if (dwCtrlType = CTRL_C_EVENT) then
  begin
    if AppIsTerminated then
    begin
      Logger.Info('');
      Logger.Info(' * Ctrl-C pressed twice,forcing termination... *');
      Logger.Info('');
      exit(false);
    end;
    Logger.Info('');
    Logger.Info(' * Ctrl-C pressed, exiting. Please wait while we clean up... *');
    Logger.Info(' * Press Ctrl-C again to force terminate.');
    Logger.Info('');
    TerminateApp;
    exit(true);
  end;

  Result := false;
end;
{$ENDIF}

procedure EnableCtrlCTermination;
begin
{$IFDEF MSWINDOWS}
  SetConsoleCtrlHandler(@CtrlCHandler, True);
{$ENDIF}
end;

end.

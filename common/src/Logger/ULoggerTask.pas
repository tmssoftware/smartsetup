unit ULoggerTask;

interface
uses Classes, SysUtils, Threading, UMultiLogger;

function RunTask(const Func: TProc): ITask;

implementation

function RunTask(const Func: TProc): ITask;
begin
  var ConnectionPoint := Logger.CreateConnectionPoint;

  Result := TTask.Create(
  procedure
  begin
    var State := Logger.Push;
    try
      Logger.ConnectTo(ConnectionPoint);
      Func;
    finally
      Logger.Pop(State);
    end;
  end);
  Result.Start;
end;

end.

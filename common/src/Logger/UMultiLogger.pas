unit UMultiLogger;
{$i ../tmscommon.inc}

interface
uses ULogger, SysUtils;

type
  TMultiLogger = class
  var
    FVerbosity: TVerbosity;
    Loggers: Array of TLogger;
  public
    constructor Create(const aLoggers: array of TLogger);
    destructor Destroy; override;

    procedure AddLogger(const aLogger: TLogger);

    procedure Error(const Message: string);
    procedure Info(const Message: string);
    procedure Trace(const Message: string);
    procedure Write(const verb: TVerbosity; const s: string);
    procedure Message(const MessageKind: TLogMessageKind; const Message: string; const Process: boolean = false);
    procedure Progress(const Message: string; const Progress: TProductProgressInfo);

    //Use the 2 methods below to group operations in the log. You can for example put all the build actions in a group.
    //MessageType: Type of message you are grouping. Except for summary and basicinfo, they all behave the same.
    //MessageLabel: Caption for the group.
    //IsError: If true, the group will be marked as an error. You specify the error
    //         at the end of the group, since normally at the start you won't know if
    //         there will be an error. The log system will take care of propagating it.
    procedure StartSection(const MessageType: TMessageType; const MessageLabel: string);
    procedure FinishSection(const MessageType: TMessageType; const IsError: boolean = false);

    //The 2 methods below are used to connect a thread with the main thread,
    //And produce a "Natural" log that looks the same as if all actions were done
    //single-threaded. Call CreateConnectionPoint in the main thread, and then ConnectTo
    //as the first thing when starting the child thread. The output will go to the place where
    //you marked it with CreateConnectionPoint.
    function CreateConnectionPoint: TConnectionPoint;
    procedure ConnectTo(const ConnectionPoint: TConnectionPoint);
    //Save and restore the state of the logger. Used to restore state in multithread scenarios.
    function Push: TArray<TObject>;
    procedure Pop(const State: TArray<TObject>);


    property Verbosity: TVerbosity read FVerbosity write FVerbosity;

    procedure ResetPercentAction;
    procedure SetPercentAction(const func: TFunc<integer>);

    procedure StartSpinner;
    procedure StopSpinner;
  end;

var
  Logger: TMultiLogger;

implementation

{ TMultiLogger }

constructor TMultiLogger.Create(const aLoggers: array of TLogger);
begin
  SetLength(Loggers, Length(aLoggers));
  for var i := Low(Loggers) to High(Loggers) do
  begin
    Loggers[i] := aLoggers[i];
  end;
end;

function TMultiLogger.CreateConnectionPoint: TConnectionPoint;
begin
  Result := TConnectionPoint.Create;
  for var i := Low(Loggers) to High(Loggers) do
  begin
    try
      Loggers[i].CreateConnectionPoint(Result);
    except
      //nothing, we can't recover or even write the message. Maybe other logger will.
    end;
  end;
end;

procedure TMultiLogger.ConnectTo(const ConnectionPoint: TConnectionPoint);
begin
  for var i := Low(Loggers) to High(Loggers) do
  begin
    try
      Loggers[i].ConnectTo(ConnectionPoint);
    except
      //nothing, we can't recover or even write the message. Maybe other logger will.
    end;
  end;

end;

destructor TMultiLogger.Destroy;
begin
  for var i := Low(Loggers) to High(Loggers) do
  begin
    Loggers[i].Free;
  end;
  inherited;
end;


procedure TMultiLogger.AddLogger(const aLogger: TLogger);
begin
  SetLength(Loggers, Length(Loggers) + 1);
  Loggers[Length(Loggers) - 1] := aLogger;
end;

procedure TMultiLogger.StartSection(const MessageType: TMessageType;
  const MessageLabel: string);
begin
  for var i := Low(Loggers) to High(Loggers) do
  begin
    try
      Loggers[i].StartSection(MessageType, MessageLabel);
    except
      //nothing, we can't recover or even write the message. Maybe other logger will.
    end;
  end;
end;

procedure TMultiLogger.StartSpinner;
begin
  for var i := Low(Loggers) to High(Loggers) do
  begin
    try
      Loggers[i].StartSpinner;
    except
      //nothing, we can't recover or even write the message. Maybe other logger will.
    end;
  end;
end;

procedure TMultiLogger.StopSpinner;
begin
  for var i := Low(Loggers) to High(Loggers) do
  begin
    try
      Loggers[i].StopSpinner;
    except
      //nothing, we can't recover or even write the message. Maybe other logger will.
    end;
  end;
end;

procedure TMultiLogger.FinishSection(const MessageType: TMessageType; const IsError: boolean = false);
begin
  for var i := Low(Loggers) to High(Loggers) do
  begin
    try
      Loggers[i].FinishSection(MessageType, IsError);
    except
      //nothing, we can't recover or even write the message. Maybe other logger will.
    end;
  end;

end;


procedure TMultiLogger.Error(const Message: string);
begin
  for var i := Low(Loggers) to High(Loggers) do
  begin
    if (not Loggers[i].IgnoresVerbosity) and (Verbosity > TVerbosity.error) then continue;
    try
      Loggers[i].Error(Loggers[i].ProcessMsg(Message));
    except
      //nothing, we can't recover or even write the message. Maybe other logger will.
    end;
  end;
end;

procedure TMultiLogger.Info(const Message: string);
begin
  if Loggers = nil then exit;

  for var i := Low(Loggers) to High(Loggers) do
  begin
    if (not Loggers[i].IgnoresVerbosity) and (Verbosity > TVerbosity.info) then continue;
    try
      Loggers[i].Info(Loggers[i].ProcessMsg(Message));
    except
      //nothing, we can't recover or even write the message. Maybe other logger will.
    end;
  end;
end;


procedure TMultiLogger.Progress(const Message: string; const Progress: TProductProgressInfo);
begin
  if Loggers = nil then exit;

  for var i := Low(Loggers) to High(Loggers) do
  begin
    if (not Loggers[i].IgnoresVerbosity) and (Verbosity > TVerbosity.info) then continue;
    try
      Loggers[i].Progress(Message, Progress);
    except
      //nothing, we can't recover or even write the message. Maybe other logger will.
    end;
  end;
end;


function TMultiLogger.Push: TArray<TObject>;
begin
  Result := nil;
  SetLength(Result, Length(Loggers));
  for var i := Low(Loggers) to High(Loggers) do
  begin
    try
      Result[i] := Loggers[i].Push;
    except
      //nothing, we can't recover or even write the message. Maybe other logger will.
    end;
  end;
end;

procedure TMultiLogger.Pop(const State: TArray<TObject>);
begin
  for var i := Low(Loggers) to High(Loggers) do
  begin
    try
      Loggers[i].Pop(State[i]);
    except
      //nothing, we can't recover or even write the message. Maybe other logger will.
    end;
  end;
end;


procedure TMultiLogger.Trace(const Message: string);
begin
  for var i := Low(Loggers) to High(Loggers) do
  begin
    if (not Loggers[i].IgnoresVerbosity) and (Verbosity > TVerbosity.trace) then continue;
    try
      Loggers[i].Trace(Loggers[i].ProcessMsg(Message));
    except
      //nothing, we can't recover or even write the message. Maybe other logger will.
    end;
  end;
end;



procedure TMultiLogger.Message(const MessageKind: TLogMessageKind; const Message: string; const Process: boolean = false);
begin
  for var i := Low(Loggers) to High(Loggers) do
  begin
    try
      var ActualMsg := Message;
      if Process then ActualMsg := Loggers[i].ProcessMsg(Message);
      Loggers[i].Message(MessageKind, ActualMsg);
    except
      //nothing, we can't recover or even write the message. Maybe other logger will.
    end;
  end;
end;

procedure TMultiLogger.ResetPercentAction;
begin
  for var logger in Loggers do logger.ResetPercentAction;
end;

procedure TMultiLogger.SetPercentAction(const func: TFunc<integer>);
begin
  for var logger in Loggers do logger.SetPercentAction(func);
end;

procedure TMultiLogger.Write(const verb: TVerbosity; const s: string);
begin
  case verb of
    TVerbosity.trace: Trace(s);
    TVerbosity.info: Info(s);
    TVerbosity.error: Error(s);
  end;
end;

end.

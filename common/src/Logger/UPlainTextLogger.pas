unit UPlainTextLogger;
{$i ../tmscommon.inc}

interface
uses ULogger, UFileLogger, Generics.Collections;

type
  TPlainTextLogger = class(TFileLogger)
  private
    Loggers: TObjectList<TLogMessageList>;

    class threadvar
      PTLogger: TLogMessageList;

    const
    MessageVerbosities: Array[TVerbosity] of string = ('[TRACE]', '[INFO ]', '[ERROR]');

  protected
    function Logger: TLogMessageList; override;
    procedure CheckLogger; override;

    procedure SaveLog; override;
  public
    procedure StartSection(const MessageType: TMessageType; const MessageLabel: string); override;
    procedure FinishSection(const MessageType: TMessageType; const IsError: boolean); override;

    constructor Create(const aFileName: string);
    destructor Destroy; override;

  end;

implementation
uses Classes, SysUtils;

{ TPlainTextLogger }

function TPlainTextLogger.Logger: TLogMessageList;
begin
  Result := PTLogger;
end;

procedure TPlainTextLogger.CheckLogger;
begin
  if Logger = nil then
  begin
    MonitorEnter(Loggers);
    try
      PTLogger := TLogMessageList.Create;
      Loggers.Add(Logger);
    finally
      MonitorExit(Loggers);
    end;
  end;
end;



procedure TPlainTextLogger.StartSection(const MessageType: TMessageType;
  const MessageLabel: string);
begin
end;

constructor TPlainTextLogger.Create(const aFileName: string);
begin
  inherited Create(aFileName);
  Loggers := TObjectList<TLogMessageList>.Create;
end;

destructor TPlainTextLogger.Destroy;
begin
  SaveLog;
  Loggers.Free;

  inherited;
end;

procedure TPlainTextLogger.FinishSection(const MessageType: TMessageType;
  const IsError: boolean);
begin
end;


procedure TPlainTextLogger.SaveLog;
begin
  var Writer := TStreamWriter.Create(FileName, false, TEncoding.UTF8);
  try
    var thread := 0;
    for var log in Loggers do
    begin
      if thread > 0 then
      begin
        Writer.WriteLine;
        Writer.WriteLine('---- Thread ' + IntToStr(thread));
        Writer.WriteLine;
      end;
      inc(thread);
      for var msg in log do
      begin
        var MsgText := Format('%s [%s] %s', [MessageVerbosities[msg.MessageVerbosity], FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', msg.Time), msg.Message]);
        if msg.NewLine then Writer.WriteLine(MsgText)
        else Writer.Write(MsgText);
      end;
    end;
  finally
    Writer.Free;
  end;

end;

end.

unit UFileLogger;
{$i ../tmscommon.inc}

interface
uses ULogger, Classes, SysUtils, Generics.Collections, SyncObjs;

type
  TSectionType =
  (
    Close = -1,
    None = 0,
    Open = 1
  );

  TLogMessage = class;

  TLogMessageList = TObjectList<TLogMessage>;

  TLogMessage = class
  private
    FMessage: string;
    FTime: TDateTime;
    FMessageVerbosity: TVerbosity;
    FMessageType: TMessageType;
    FLogMessageKind: TLogMessageKind;
    FSectionType: TSectionType;
    FSectionHasErrors: boolean;
    FChildren: TLogMessageList;
  public
    property Message: string read FMessage;
    property MessageVerbosity: TVerbosity read FMessageVerbosity;
    property Time: TDateTime read FTime;
    property MessageType: TMessageType read FMessageType;
    property LogMessageKind: TLogMessageKind read FLogMessageKind;
    property SectionType: TSectionType read FSectionType;
    property SectionHasErrors: boolean read FSectionHasErrors write FSectionHasErrors;

    property Children: TLogMessageList read FChildren;

    constructor CreateJoint;
    constructor Create(const aMessage: string; const aMessageVerbosity: TVerbosity; const aLogMessageKind: TLogMessageKind = TLogMessageKind.Text);
    constructor CreateStartSection(const aMessageType: TMessageType; const aCaption: string);
    constructor CreateEndSection(const aMessageType: TMessageType; const IsError: boolean);
    destructor Destroy; override;
  end;

  TFileLogger = class(TLogger)
  protected
    FileName: string;
    procedure SaveLog; virtual; abstract;

    procedure CheckLogger; virtual; abstract;
    function Logger: TLogMessageList; virtual; abstract;

  public
    constructor Create(const aFileName: string);
    destructor Destroy; override;
    function ProcessMsg(const s: string): string; override;
    procedure ResetPercentAction; override;
    procedure SetPercentAction(const func: TFunc<integer>); override;
    function IgnoresVerbosity: boolean; override;

    procedure Error(const Message: string); override;
    procedure Info(const Message: string); override;
    procedure Trace(const Message: string); override;
    procedure Message(const MessageKind: TLogMessageKind; const Message: string); override;
  end;

implementation


{ TFileLogger }
constructor TFileLogger.Create(const aFileName: string);
begin
  FileName := aFileName;
end;

destructor TFileLogger.Destroy;
begin
  inherited;
end;

function TFileLogger.IgnoresVerbosity: boolean;
begin
  Result := true;
end;

function TFileLogger.ProcessMsg(const s: string): string;
begin
  Result := s;
end;

procedure TFileLogger.ResetPercentAction;
begin
  //nothing.
end;


procedure TFileLogger.SetPercentAction(const func: TFunc<integer>);
begin
  //nothing.
end;

procedure TFileLogger.Trace(const Message: string);
begin
  CheckLogger;
  if (Logger <> nil) then Logger.Add(TLogMessage.Create(Message, TVerbosity.trace));
end;

procedure TFileLogger.Info(const Message: string);
begin
  CheckLogger;
  if (Logger <> nil) then Logger.Add(TLogMessage.Create(Message, TVerbosity.info));
end;

procedure TFileLogger.Message(const MessageKind: TLogMessageKind;
  const Message: string);
begin
  CheckLogger;
  if (Logger <> nil) then Logger.Add(TLogMessage.Create(Message, TVerbosity.info, MessageKind));
end;

procedure TFileLogger.Error(const Message: string);
begin
  CheckLogger;
  if (Logger <> nil) then Logger.Add(TLogMessage.Create(Message, TVerbosity.error));
end;

{ TLogMessage }

constructor TLogMessage.Create(const aMessage: string; const aMessageVerbosity: TVerbosity; const aLogMessageKind: TLogMessageKind = TLogMessageKind.Text);
begin
  FMessage := aMessage;
  FMessageVerbosity := aMessageVerbosity;
  FLogMessageKind := aLogMessageKind;
  FTime := Now;
end;

constructor TLogMessage.CreateJoint;
begin
  FChildren := TLogMessageList.Create;
end;

constructor TLogMessage.CreateStartSection(const aMessageType: TMessageType;
  const aCaption: string);
begin
  FMessageType := aMessageType;
  FMessage := aCaption;
  FTime := now;
  FSectionType := TSectionType.Open;
end;

constructor TLogMessage.CreateEndSection(const aMessageType: TMessageType; const IsError: boolean);
begin
  FMessageType := aMessageType;
  FTime := now;
  SectionHasErrors := IsError;
  FSectionType := TSectionType.Close;
end;

destructor TLogMessage.Destroy;
begin
  FChildren.Free;
  inherited;
end;

end.

unit UMemoLogger;

interface
uses ULogger, Classes, SysUtils, Generics.Defaults, SyncObjs, StdCtrls;

type
  TMemoLogger = class(TLogger)
  strict private
    FMemo: TMemo;
    procedure LogToMemo(const S: string);
  public
    MessageFormat: string;
    constructor Create(AMemo: TMemo);
    function ProcessMsg(const s: string): string; override;
    procedure ResetPercentAction; override;
    procedure SetPercentAction(const func: TFunc<integer>); override;
    function IgnoresVerbosity: boolean; override;

    procedure Error(const s: string); override;
    procedure Info(const s: string); override;
    procedure Trace(const s: string); override;
    procedure Message(const MessageKind: TLogMessageKind; const Message: string); override;

  end;

implementation

{ TMemoLogger }

constructor TMemoLogger.Create(AMemo: TMemo);
begin
  MessageFormat := '[%1:s] %0:s';
  FMemo := AMemo;
end;

function TMemoLogger.IgnoresVerbosity: boolean;
begin
  Result := False;
end;

function TMemoLogger.ProcessMsg(const s: string): string;
begin
  Result := Format(MessageFormat, [s, FormatDateTime('hh:nn:ss', Now)]);
end;

procedure TMemoLogger.ResetPercentAction;
begin
end;

procedure TMemoLogger.SetPercentAction(const func: TFunc<integer>);
begin
end;

procedure TMemoLogger.Trace(const s: string);
begin
  LogToMemo(s);
end;

procedure TMemoLogger.Info(const s: string);
begin
  LogToMemo(s);
end;

procedure TMemoLogger.LogToMemo(const S: string);
begin
  if Assigned(FMemo) then
    TThread.Queue(nil, procedure
      begin
        FMemo.Lines.Add(S);
      end);
end;

procedure TMemoLogger.Message(const MessageKind: TLogMessageKind;
  const Message: string);
begin
  inherited;
  LogToMemo(Message);
end;

procedure TMemoLogger.Error(const s: string);
begin
  LogToMemo(s);
end;

end.

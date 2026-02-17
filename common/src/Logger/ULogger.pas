unit ULogger;
{$i ../tmscommon.inc}

interface
uses SysUtils;
type
  TVerbosity = (trace, info, error);
  TLogMessageKind = (Text, Caption, Question, Comment, Conclusion);

type
  TMessageType = (
      None,
      BasicInfo,
      Summary,
      Command,
      Build,
      Fetch,
      Update,
      Register,
      Unregister,
      Check,
      VCSFetch,
      VCSRegister,
      Load,
      Analyze,
      Skipped,
      Megafolder,
      SetupIDE,
      Configuration
  );

  TConnectionPoint = record
    Guid: TGuid;
    class function Create: TConnectionPoint; static;
  end;

  TProductProgressInfo = record
    ProductId: string;
    ProcessedPackages: integer;
    TotalPackages: integer;

    constructor Create(const aProductId: string);
    procedure SetTotalPackages(const aTotalPackages: integer);
    procedure DoAtomicIncrement;
    function Finished: boolean;
    function ToString: string;

  end;

  TLogger = class
  private
  public
    procedure Error(const Message: string); virtual; abstract;
    procedure Info(const Message: string); virtual; abstract;
    procedure Trace(const Message: string); virtual; abstract;
    procedure Message(const MessageKind: TLogMessageKind; const Message: string; const NewLine: boolean); virtual; abstract;
    procedure Progress(const Message: string; const Progress: TProductProgressInfo); virtual;


    procedure StartSection(const MessageType: TMessageType; const MessageLabel: string); virtual; abstract;
    procedure FinishSection(const MessageType: TMessageType; const IsError: boolean); virtual; abstract;

    procedure CreateConnectionPoint(const ConnectionPoint: TConnectionPoint); virtual;
    procedure ConnectTo(const ConnectionPoint: TConnectionPoint); virtual;
    function Push: TObject; virtual;
    procedure Pop(const State: TObject); virtual;


    function ProcessMsg(const s: string): string; virtual; abstract;
    procedure ResetPercentAction; virtual; abstract;
    procedure SetPercentAction(const func: TFunc<integer>); virtual; abstract;

    function IgnoresVerbosity: boolean; virtual; abstract;

    procedure StartSpinner; virtual;
    procedure StopSpinner; virtual;
  end;


implementation


{ TLogger }

procedure TLogger.CreateConnectionPoint(
  const ConnectionPoint: TConnectionPoint);
begin

end;


procedure TLogger.Progress(const Message: string;
  const Progress: TProductProgressInfo);
begin
  Info(ProcessMsg(Message)); // loggers except the console will ignore the progress
end;

function TLogger.Push: TObject;
begin
  Result := nil;
end;

procedure TLogger.Pop(const State: TObject);
begin

end;

procedure TLogger.StartSpinner;
begin
  //by default, nothing
end;

procedure TLogger.StopSpinner;
begin

end;

procedure TLogger.ConnectTo(const ConnectionPoint: TConnectionPoint);
begin
  //nothing
end;

{ TConnectionPoint }

class function TConnectionPoint.Create: TConnectionPoint;
begin
  Result.Guid := TGuid.NewGuid;
end;

{ TProductProgressInfo }

procedure TProductProgressInfo.DoAtomicIncrement;
begin
  AtomicIncrement(ProcessedPackages);
end;

function TProductProgressInfo.Finished: boolean;
begin
  var Succeeded := false;
  AtomicCmpExchange(ProcessedPackages, ProcessedPackages, TotalPackages, Succeeded);
  Result := Succeeded;
end;

constructor TProductProgressInfo.Create(const aProductId: string);
begin
  ProductId := aProductId;
end;

procedure TProductProgressInfo.SetTotalPackages(const aTotalPackages: integer);
begin
  TotalPackages := aTotalPackages;
end;

function TProductProgressInfo.ToString: string;
begin
  Result := '';
  if ProductId = '' then exit;
  
  var ProductPercent := 100;
  if (TotalPackages > 0) and (ProcessedPackages < TotalPackages) then
  begin
   ProductPercent :=  (100 * ProcessedPackages) div TotalPackages;
  end;
  Result := Result + '[' + ProductId + ': '+ IntToStr(ProductPercent).PadLeft(3) + '%]';
end;

end.

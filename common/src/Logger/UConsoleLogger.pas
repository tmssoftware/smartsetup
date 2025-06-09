unit UConsoleLogger;
{$i ../tmscommon.inc}

interface
uses ULogger, Classes, SysUtils, Generics.Defaults, SyncObjs, UConsoleUtil;

type
  TConsoleLogDisplayOptions = (
     UseANSI,
     ProductProgress
  );

  TConsoleLogDisplayOptionSet = set of TConsoleLogDisplayOptions;
const
  LogDisplayOptionsDefaut = [TConsoleLogDisplayOptions.UseANSI];
  LogDisplayOptionsMachineReadable: TConsoleLogDisplayOptionSet = [TConsoleLogDisplayOptions.ProductProgress];

type
  TConsoleLogger = class(TLogger)
  private
    class var
      OutputLock: TCriticalSection;
    class var
      Console: TConsoleUtil;
  private
    ProgressBar: TConsoleProgressType;
    FDisplayOptions: TConsoleLogDisplayOptionSet;
    AppTitle: string;
    Spinning: boolean;

    function GetPercentString(const Percent: integer): string;
    procedure SetDisplayOptions(const AValue: TConsoleLogDisplayOptionSet);
    function GetColor(const MessageKind: TLogMessageKind): TConsoleColors;
  public
    GetPercent: TFunc<integer>;
    MessageFormat: string;

    constructor Create(const ADisplayOptions: TConsoleLogDisplayOptionSet; const AAppTitle: string = '');
    destructor Destroy; override;
    class constructor Create;
    class destructor Destroy;
    function ProcessMsg(const s: string): string; override;
    procedure ResetPercentAction; override;
    procedure SetPercentAction(const func: TFunc<integer>); override;
    function IgnoresVerbosity: boolean; override;

    procedure Error(const Message: string); override;
    procedure Info(const Message: string); override;
    procedure Trace(const Message: string); override;
    procedure Message(const MessageKind: TLogMessageKind; const Message: string); override;
    procedure Progress(const Message: string; const Progress: TProductProgressInfo); override;
    procedure StartSection(const MessageType: TMessageType; const MessageLabel: string); override;
    procedure FinishSection(const MessageType: TMessageType; const IsError: boolean); override;

    property DisplayOptions: TConsoleLogDisplayOptionSet read FDisplayOptions write SetDisplayOptions;

    procedure StartSpinner; override;
    procedure StopSpinner; override;

  end;

implementation

{ TConsoleLogger }

constructor TConsoleLogger.Create(const ADisplayOptions: TConsoleLogDisplayOptionSet; const AAppTitle: string);
begin
  inherited Create;
  DisplayOptions := ADisplayOptions;
  AppTitle := AAppTitle;
  ProgressBar := TConsoleProgressType.NormalBar;
  MessageFormat := '[%1:s%2:s] %0:s';

  // we don't call ResetPercentFunction here to not call a virtual method in a constructor.
  GetPercent := function: integer
  begin
    Result := -1;
  end;
end;

destructor TConsoleLogger.Destroy;
begin
  inherited;
end;

class constructor TConsoleLogger.Create;
begin
  OutputLock := TCriticalSection.Create;
  Console := TConsoleUtil.Create;
end;

class destructor TConsoleLogger.Destroy;
begin
  Console.Free;
  OutputLock.Free;
end;

function TConsoleLogger.IgnoresVerbosity: boolean;
begin
  Result := false;
end;

function TConsoleLogger.GetPercentString(const Percent: integer): string;
begin
  if (Percent < 0) then Exit('');
  Result := ' - ' + IntToStr(Percent).PadLeft(3) + '%';

end;

function TConsoleLogger.ProcessMsg(const s: string): string;
begin
  var Percent := GetPercent;
  var ConsoleProgress: string;
  var ConsoleTitle := '';
  if (Percent >= 0) then
  begin
    if not Spinning then ConsoleProgress := Console.SetProgress(ProgressBar, Percent);
    if AppTitle <> '' then ConsoleTitle := Console.SetTitle(IntToStr(Percent) + '% - ' + AppTitle);
  end
    else
  begin
    if not Spinning then ConsoleProgress := Console.SetProgress(TConsoleProgressType.Default, 0);
    ConsoleTitle := Console.SetTitle(AppTitle);
  end;

  Result := ConsoleTitle + ConsoleProgress + Format(MessageFormat, [s, FormatDateTime('hh:nn:ss', Now) , GetPercentString(Percent)]);
end;

procedure TConsoleLogger.ResetPercentAction;
begin
  GetPercent := function: integer
  begin
    Result := -1;
  end;
  if not Spinning then Console.SetProgress(TConsoleProgressType.Default, 0);
end;

procedure TConsoleLogger.SetDisplayOptions(
  const AValue: TConsoleLogDisplayOptionSet);
begin
  FDisplayOptions := AValue;
  Console.Enabled :=  TConsoleLogDisplayOptions.UseANSI in AValue;
end;

procedure TConsoleLogger.SetPercentAction(const func: TFunc<integer>);
begin
  GetPercent := func;
end;

procedure TConsoleLogger.StartSection(
  const MessageType: TMessageType; const MessageLabel: string);
begin
 //ignore them.
end;

procedure TConsoleLogger.StartSpinner;
begin
  inherited;
  Spinning := true;
  OutputLock.Enter;
  try
    Write(Output, Console.SetProgress(TConsoleProgressType.Spin, 0));
  finally
    OutputLock.Leave;
  end;
end;

procedure TConsoleLogger.StopSpinner;
begin
  inherited;
  Spinning := false;
  OutputLock.Enter;
  try
    Write(Output, Console.SetProgress(TConsoleProgressType.Default, 0));
  finally
    OutputLock.Leave;
  end;
end;

procedure TConsoleLogger.FinishSection(
  const MessageType: TMessageType; const IsError: boolean);
begin
 //ignore them.
end;

procedure TConsoleLogger.Trace(const Message: string);
begin
  OutputLock.Enter;
  try
    WriteLn(Output, Message);
  finally
    OutputLock.Leave;
  end;
end;

procedure TConsoleLogger.Info(const Message: string);
begin
  OutputLock.Enter;
  try
    WriteLn(Output, Message);
  finally
    OutputLock.Leave;
  end;
end;

function TConsoleLogger.GetColor(const MessageKind: TLogMessageKind): TConsoleColors;
begin
  Result := TConsoleColors.None;
  case MessageKind of
    TLogMessageKind.Text: exit(TConsoleColors.None);
    TLogMessageKind.Caption: exit(TConsoleColors.BrightBlue);
    TLogMessageKind.Question: exit(TConsoleColors.BrightMagenta);
    TLogMessageKind.Comment: exit(TConsoleColors.Cyan);
    TLogMessageKind.Conclusion: exit(TConsoleColors.BrightGreen);
  end;
end;

procedure TConsoleLogger.Message(const MessageKind: TLogMessageKind;
  const Message: string);
begin
  OutputLock.Enter;
  try
    WriteLn(Output, Console.SetColor(GetColor(MessageKind)) + Message + Console.SetColor(TConsoleColors.None));
  finally
    OutputLock.Leave;
  end;
end;

procedure TConsoleLogger.Progress(const Message: string;
  const Progress: TProductProgressInfo);
begin
  if TConsoleLogDisplayOptions.ProductProgress in DisplayOptions
    then Info(ProcessMsg(Progress.ToString + ' ' + Message))
    else Info(ProcessMsg(Message));
end;


procedure TConsoleLogger.Error(const Message: string);
begin
  OutputLock.Enter;
  try
    ProgressBar := TConsoleProgressType.ErrorBar;
    WriteLn(Output, Console.SetColor(TConsoleColors.BrightRed) + Message + Console.SetColor(TConsoleColors.None));
  finally
    OutputLock.Leave;
  end;
end;

end.

unit Util.Animator;

interface
uses Classes, SysUtils, VCL.ExtCtrls;
type
  TAnimator = class
  private
    NestedLevel: integer;
    Timer: TTimer;
    ProgressIndex: integer;
    Text: string;
    FinalText: string;
    DoubleBlock: boolean;
    SetText: TProc<string>;
    procedure Tick(Sender: TObject);
  public
    procedure Start;
    procedure Stop;

    constructor Create(const aDelayMs: integer; const aDoubleBlock: boolean; const aText, aFinalText: string; const aSetText: TProc<string>);
    destructor Destroy; override;
  end;

implementation

constructor TAnimator.Create(const aDelayMs: integer; const aDoubleBlock: boolean; const aText, aFinalText: string; const aSetText: TProc<string>);
begin
  inherited Create;
  DoubleBlock := aDoubleBlock;
  Text := aText;
  FinalText := aFinalText;
  SetText := aSetText;
  Timer := TTimer.Create(nil);
  Timer.Enabled := false;
  Timer.Interval := aDelayMs;
  Timer.OnTimer := Tick;

end;

destructor TAnimator.Destroy;
begin
  Timer.Enabled := false;
  Timer.Free;
  inherited;
end;

procedure TAnimator.Start;
begin
  TThread.Queue(nil, procedure
    begin
      Inc(NestedLevel);
      if (NestedLevel = 1) then SetText(Text + ' ');
      Timer.Enabled := NestedLevel > 0;
    end);
end;

procedure TAnimator.Tick(Sender: TObject);
const
  Blocks: Array[0.. 9] of string = ('⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏');
begin
  var FirstBlock := '';
  if DoubleBlock then FirstBlock := Blocks[(Length(Blocks) * 2 + 1 - ProgressIndex) mod Length(Blocks)];

  SetText(FirstBlock + Blocks[ProgressIndex] + ' ' + Text + ' ');
  if ProgressIndex < Length(Blocks) - 1 then Inc(ProgressIndex) else ProgressIndex := 0;
end;

procedure TAnimator.Stop;
begin
  TThread.Queue(nil, procedure
    begin
      if (NestedLevel > 0) then Dec(NestedLevel);
      Timer.Enabled := NestedLevel > 0;
      if (NestedLevel = 0) then SetText(FinalText);
    end);
end;


end.

unit Util.Animator;

interface
uses Classes, SysUtils, VCL.ExtCtrls;
type
  TAnimator = class
  strict private
    NestedLevel: integer;
    Timer: TTimer;
  protected
    ProgressIndex: integer;
    procedure Tick(Sender: TObject); virtual; abstract;
    procedure DoStart; virtual; abstract;
    procedure DoStop; virtual; abstract;
  public
    procedure Start;
    procedure Stop;

    constructor Create(const aDelayMs: integer);
    destructor Destroy; override;
  end;

  TTextAnimator = class(TAnimator)
  private
    Text: string;
    FinalText: string;
    DoubleBlock: boolean;

    SetText: TProc<string>;
  protected
    procedure Tick(Sender: TObject); override;
    procedure DoStart; override;
    procedure DoStop; override;
  public
    constructor Create(const aDelayMs: integer; const aDoubleBlock: boolean; const aText, aFinalText: string; const aSetText: TProc<string>);
  end;

  TImageAnimator = class(TAnimator)
  private
    ImageCategory: string;
    FinalImage: string;
    SetImage: TProc<string>;
    HasImage: TFunc<string, boolean>;
    CachedLastImage: string; //avoid looking up for it all the time.This runs every some ms.

    function GetImageName: string;
  protected
    procedure Tick(Sender: TObject); override;
    procedure DoStart; override;
    procedure DoStop; override;
  public
    constructor Create(const aDelayMs: integer; const aImageCategory, aFinalImage: string; const aSetImage: TProc<string>; const aHasImage: TFunc<string, boolean>);
  end;

implementation

constructor TAnimator.Create(const aDelayMs: integer);
begin
  inherited Create;
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
  TThread.ForceQueue(nil, procedure
    begin
      Inc(NestedLevel);
      if (NestedLevel = 1) then
      begin
        ProgressIndex := 0;
        DoStart;
      end;
      Timer.Enabled := NestedLevel > 0;
    end);
end;

procedure TAnimator.Stop;
begin
  TThread.ForceQueue(nil, procedure
    begin
      if (NestedLevel > 0) then Dec(NestedLevel);
      Timer.Enabled := NestedLevel > 0;
      if (NestedLevel = 0) then DoStop;
    end);
end;


{ TTextAnimator }

constructor TTextAnimator.Create(const aDelayMs: integer;
  const aDoubleBlock: boolean; const aText, aFinalText: string;
  const aSetText: TProc<string>);
begin
  inherited Create(aDelayMs);
  DoubleBlock := aDoubleBlock;
  Text := aText;
  FinalText := aFinalText;
  SetText := aSetText;
end;

procedure TTextAnimator.DoStart;
begin
  SetText(Text + ' ');
end;

procedure TTextAnimator.DoStop;
begin
  SetText(FinalText);
end;

procedure TTextAnimator.Tick(Sender: TObject);
const
  Blocks: Array[0.. 9] of string = ('⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏');
begin
  var FirstBlock := '';
  if DoubleBlock then FirstBlock := Blocks[(Length(Blocks) * 2 + 1 - ProgressIndex) mod Length(Blocks)];

  SetText(FirstBlock + Blocks[ProgressIndex] + ' ' + Text + ' ');
  if ProgressIndex < Length(Blocks) - 1 then Inc(ProgressIndex) else ProgressIndex := 0;
end;


{ TImageAnimator }

constructor TImageAnimator.Create(const aDelayMs: integer; const aImageCategory,
  aFinalImage: string; const aSetImage: TProc<string>;
  const aHasImage: TFunc<string, boolean>);
begin
  inherited Create(aDelayMs);
  ImageCategory := aImageCategory;
  FinalImage := aFinalImage;
  SetImage := aSetImage;
  HasImage := aHasImage;
end;

procedure TImageAnimator.DoStart;
begin

end;

procedure TImageAnimator.DoStop;
begin
  SetImage(FinalImage);
end;

function TImageAnimator.GetImageName: string;
begin
  Result := ImageCategory + '\' + ImageCategory + '-' + IntToStr(ProgressIndex).PadLeft(2, '0');
end;

procedure TImageAnimator.Tick(Sender: TObject);
begin
  Inc(ProgressIndex);
  var ImageName := GetImageName;
  var Wrap: boolean;
  if CachedLastImage <> '' then
  begin
    Wrap := (CachedLastImage = ImageName);
  end else
  begin
    Wrap := not HasImage(ImageName);
    if Wrap then CachedLastImage := ImageName;
  end;
  if Wrap then
  begin
    ProgressIndex := 1;
    ImageName := GetImageName;
  end;

  SetImage(ImageName);
end;

end.

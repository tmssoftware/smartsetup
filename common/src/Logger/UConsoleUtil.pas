unit UConsoleUtil;

interface
uses Classes,
{$IFDEF MSWINDOWS}
Windows,
{$ENDIF}
SysUtils, SyncObjs;

type
  TConsoleProgressType = (
    Default = 0,
    NormalBar = 1,
    ErrorBar = 2,
    Spin = 3,
    WarningBar = 4
  );

  TConsoleColors = (
    None = 0,
    Red = 31,
    Green = 32,
    Yellow = 33,
    Blue = 34,
    Magenta = 35,
    Cyan = 36,
    BrightRed = 91,
    BrightGreen = 92,
    BrightYellow = 93,
    BrightBlue = 94,
    BrightMagenta = 95,
    BrightCyan = 96,
    BrightRedBkg = 101

  );

  TConsoleUtil = class
  private
    FSupportsANSI: boolean;
    FEnabled: boolean;
  public
    property SupportsANSI: boolean read FSupportsANSI;
    property Enabled: boolean read FEnabled write FEnabled;

    constructor Create;
    destructor Destroy; override;

    function SetProgress(const BarType: TConsoleProgressType; const Value: integer): string; //value between 0 and 100
    function SetColor(const Color: TConsoleColors): string;
    function SetTitle(const Title: string): string;
  end;

implementation

{ TConsoleUtil }

constructor TConsoleUtil.Create;
begin
  Enabled := true;
{$IFDEF MSWINDOWS}
  FSupportsANSI := false;
  var handle := GetStdHandle(STD_OUTPUT_HANDLE);
  if handle = INVALID_HANDLE_VALUE then exit;

  var previousConsoleMode: DWORD;
  if not GetConsoleMode(handle, previousConsoleMode) then exit;
  if not SetConsoleMode(handle, previousConsoleMode or ENABLE_VIRTUAL_TERMINAL_PROCESSING) then exit;
  FSupportsANSI := true;
{$ELSE}
  FSupportsANSI := true; //we assume most linux/mac support ansi escape codes?
{$ENDIF}
end;

destructor TConsoleUtil.Destroy;
begin
  SetProgress(TConsoleProgressType.Default, 0);
  inherited;
end;

function TConsoleUtil.SetColor(const Color: TConsoleColors): string;
begin
  if not Enabled or not SupportsANSI then exit ('');
  Result := #$1b'[' + IntToStr(Integer(Color)) + 'm';
end;

function TConsoleUtil.SetProgress(const BarType: TConsoleProgressType; const Value: integer): string;
begin
  if not Enabled or not SupportsANSI then exit ('');
  Result := #$1b']9;4;' + IntToStr(Integer(BarType)) + ';' + IntToStr(Value) + #$07;
end;

function TConsoleUtil.SetTitle(const Title: string): string;
begin
  if not Enabled or not SupportsANSI then exit ('');
  Result := #$1b']2;' + Title + #$1B#$5C;
end;

end.

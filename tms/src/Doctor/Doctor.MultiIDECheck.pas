unit Doctor.MultiIDECheck;

interface

{$IFDEF MSWINDOWS}

uses Doctor.Check, SysUtils, Classes, Deget.CoreTypes, Deget.IDEInfo;

type
  TSingleIDECheck = class(TCheck)
  protected
    IDEInfo: IDelphiIDEInfo;
  public
    constructor Create(const AIDEInfo: IDelphiIDEInfo); virtual;
  end;

  ClassOfTSingleIDECheck = class of TSingleIDECheck;

  TMultiIDECheck = class(TCheck)
  protected
    Checks: Array[TIDEName] of TCheck;

  public
    constructor Create(const AlternateRegistryKey: string; const CheckClass: ClassOfTSingleIDECheck);
    destructor Destroy; override;

    function Name: string; override;
    function Description: string; override;
    procedure Check; override;

    function FixesCount: integer; override;
    function FixesApplied: integer; override;
    function GetFix(const index: integer): TFix; override;

    procedure Fix(const UndoInfo: TUndoInfo); override;
    function IsSlow: boolean; override;

  end;
{$ENDIF}

implementation
{$IFDEF MSWINDOWS}
uses Deget.DelphiInfo;

{ TMultiIDECheck }

constructor TMultiIDECheck.Create(const AlternateRegistryKey: string; const CheckClass: ClassOfTSingleIDECheck);
begin
  inherited Create;
  for var IDEName := Low(TIDEName) to High(TIDEName) do Checks[IDEName] := CheckClass.Create(TDelphiIDEInfo.Create(IDEName, AlternateRegistryKey));
end;

function TMultiIDECheck.Name: string;
begin
  for var IDEName := High(TIDEName) downto Low(TIDEName) do
  begin
    var CheckName := Checks[IDEName].Name;
    if CheckName <> '' then exit(CheckName);
  end;
  raise Exception.Create('No name in the check.');
end;

function TMultiIDECheck.Description: string;
begin
  for var IDEName := High(TIDEName) downto Low(TIDEName) do
  begin
    var CheckDescription := Checks[IDEName].Description;
    if CheckDescription <> '' then exit(CheckDescription);
  end;
  raise Exception.Create('No description in the check.');
end;

destructor TMultiIDECheck.Destroy;
begin
  for var IDEName := Low(TIDEName) to High(TIDEName) do Checks[IDEName].Free;
  inherited;
end;

procedure TMultiIDECheck.Check;
begin
  for var IDEName := Low(TIDEName) to High(TIDEName) do Checks[IDEName].Check;
end;

procedure TMultiIDECheck.Fix(const UndoInfo: TUndoInfo);
begin
  for var IDEName := Low(TIDEName) to High(TIDEName) do Checks[IDEName].Fix(UndoInfo);
end;

function TMultiIDECheck.FixesApplied: integer;
begin
  Result := 0;
  for var IDEName := Low(TIDEName) to High(TIDEName) do Inc(Result, Checks[IDEName].FixesApplied);
end;

function TMultiIDECheck.FixesCount: integer;
begin
  Result := 0;
  for var IDEName := Low(TIDEName) to High(TIDEName) do Inc(Result, Checks[IDEName].FixesCount);
end;

function TMultiIDECheck.GetFix(const index: integer): TFix;
begin
  var Start := 0;
  for var IDEName := Low(TIDEName) to High(TIDEName) do
  begin
    var Fixes := Checks[IDEName].FixesCount;
    if index < Start + Fixes then exit(Checks[IDEName].GetFix(Index - Start));

    Inc(Start, Fixes);
  end;
  raise Exception.Create('Fix outside of bounds.');
end;

function TMultiIDECheck.IsSlow: boolean;
begin
  for var IDEName := Low(TIDEName) to High(TIDEName) do
  begin
    if Checks[IDEName].IsSlow then exit(True);
  end;
  Result := false;
end;

{ TSingleIDECheck }

constructor TSingleIDECheck.Create(const AIDEInfo: IDelphiIDEInfo);
begin
  inherited Create;
  IDEInfo := AIDEInfo;
end;

{$ENDIF}
end.

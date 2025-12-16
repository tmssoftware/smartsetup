unit Doctor.Check;

interface
uses Classes, SysUtils, Generics.Collections, System.JSON;
const
  DoctorUndoExtension = '.undo.json';

type

TUndoInfo = class
private
  FName: string;
  FValue: TJSONObject;
public
  constructor Create(const AName: string);
  destructor Destroy; override;

  property Name: string read FName;
  property Value: TJSONObject read FValue;

end;

TFixType = (YesNo, Numeric);
TFix = class
private
  FFixType: TFixType;
  FMessage: string;
  FAction: string;
  FApply: integer; //0 means do not apply. 1 or more mean apply.
  FFix: TProc;
  FNumericQuestions: TArray<string>;  //Applies only if FixType is numeric.
public
  property FixType: TFixType read FFixType;
  property Message: string read FMessage;
  property Action: string read FAction;
  property Apply: integer read FApply write FApply;
  property Fix: TProc read FFix;
  property NumericQuestions: TArray<string> read FNumericQuestions;

  constructor Create(const aFixType: TFixType; const aMessage, aAction: string; const aFix: TProc; const aNumericQuestions: TArray<string> = nil);
end;

TCheck = class
protected
  Fixes: TObjectList<TFix>;
  FFixApplied: boolean;
  FFixesApplied: integer;
public
  constructor Create;
  destructor Destroy; override;

  function Name: string; virtual; abstract;
  function Description: string; virtual; abstract;
  //Slow checks shouldn't be run by tms build.
  function IsSlow: boolean; virtual; abstract;
  procedure Check; virtual; abstract;
  function FixesCount: integer; virtual;
  function FixesApplied: integer; virtual;
  function GetFix(const index: integer): TFix; virtual;

  procedure Fix(const UndoInfo: TUndoInfo); virtual;
  //procedure Undo(const UndoInfo: TUndoInfo); virtual; abstract;

  property FixApplied: boolean read FFixApplied;

end;

implementation
{ TCheck }

constructor TCheck.Create;
begin
  inherited;
  Fixes := TObjectList<TFix>.Create;
end;

destructor TCheck.Destroy;
begin
  Fixes.Free;
  inherited;
end;

procedure TCheck.Fix;
begin
  if FixApplied then raise Exception.Create('The fix for "' + Name + '" has already been applied.');

  for var f in Fixes do
  begin
    if (f.Apply > 0) and (Assigned(f.Fix)) then
    begin
      f.Fix();
      Inc(FFixesApplied);
    end;
  end;
  FFixApplied := true;
end;

function TCheck.FixesApplied: integer;
begin
  Result := FFixesApplied;
end;

function TCheck.FixesCount: integer;
begin
  Result := Fixes.Count;
end;

function TCheck.GetFix(const index: integer): TFix;
begin
  Result := Fixes[index];
end;

{ TFix }

constructor TFix.Create(const aFixType: TFixType; const aMessage, aAction: string; const aFix: TProc; const aNumericQuestions: TArray<string> = nil);
begin
  FFixType := aFixType;
  FMessage := aMessage;
  FAction := aAction;
  FApply := 0;
  FFix := aFix;
  FNumericQuestions := aNumericQuestions;
end;

{ TUndoInfo }

constructor TUndoInfo.Create(const AName: string);
begin
  FName := AName;
  FValue := TJSONObject.Create;
end;

destructor TUndoInfo.Destroy;
begin
  FValue.Free;
  inherited;
end;

end.

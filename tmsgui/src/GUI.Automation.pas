unit GUI.Automation;
{$IFDEF DEBUG}
(*
  Special words:
  self as component name -> refers to the active form.
  // or # mark a comment. Note: it should be a full line comment.
  ~ means contain. !~ means not contains. For example memo.text ~success means memo.text contains "success"
  = <> < > <= >= compare as integers if both sides are integers, else as case-insensitive text.
  $name in the right side of a comparison is replaced by the value of variable $name.
  $env:name means an environment variable
  Commands:
  assert <component.text> [=/<>/</>/<=/>=/~/!~value] | assert self.name = form1 -> crashes the app if the form name is not "form1"
  log <component.value>  | log memo1.text -> logs the value of memo1.text
  wait <ms>  | wait 5000 -> waits for 5 seconds
  wait <component.property> [=/<>/</>/<=/>=/~/!~value] -> wait button1.enabled -> waits until button is enabled
  click <button> | click button2 -> clicks button2  Can also click an action
  call <component.method> [argument] | call listview1.SelectAll -> Selects all items
    call self.ChangeWorkingFolder c:	emp -> calls a method with a single string parameter. The argument is the rest of the line.
  set-text <edit> text |  set-text memo1 hello today  -> sets the memo value to "hello today"
  set <component.property> text |  set cb.checked true  -> sets the checkbox to checked.
  var <$name> <component.property> | var $count self.ProductCount -> stores the current value of self.ProductCount in $count
*)

interface
uses SysUtils, Classes, Vcl.Forms, Vcl.Controls, Vcl.StdCtrls,
     Vcl.AppEvnts, Generics.Defaults, Generics.Collections, TypInfo, RTTI;

type
  TAutomationActionType = (
    Assert,
    Log,
    Wait,
    Click,
    Call,
    SetText,
    &Set,
    &Var
  );

type
  TVarOperation = (
    None,
    Add,
    Subtract
  );


  TAutomationData = record
    Action: TAutomationActionType;
    Target: string;
    Data: string;
    SkipLog: boolean;

    constructor Create(const aAction: TAutomationActionType; const aTarget, aData: string);
  end;

  TGUIAutomator = class
  private
    const DefaultLogFile = 'tmsgui.automation.log';
    class var Instance: TGUIAutomator;
  private
    ApplicationEvents: TApplicationEvents;
    Actions: TList<TAutomationData>;
    ActionCounter: integer;
    MaxExecutionDate: TDateTime;
    SleepingUntilTime: TDateTime;
    SleepingUntilCondition: TAutomationData;
    FLogFile: string;
    Variables: TDictionary<string, string>;
    SkipLog: boolean;

    procedure MainLoop(Sender: TObject; var Done: Boolean);
    procedure RunAction(const Action: TAutomationData);

    class constructor Create;
    class destructor Destroy;
    procedure Load(const aScriptFile, aLogFile: string; const TimeOutInMs: integer);
    procedure ParseActions(const aActions: TArray<string>);

    procedure Log(const Message: string); overload;
    function Redact(const Message: string): string;

    function GetComponent<T: TComponent>(const ComponentAndOrProperty, ComponentType: string; const MustBeType: boolean = true): T;
    procedure DoAssert(const Action: TAutomationData);
    procedure DoLog(const Action: TAutomationData);
    procedure DoWait(const Action: TAutomationData);
    procedure DoClick(const Action: TAutomationData);
    procedure DoCall(const Action: TAutomationData);
    procedure DoSetText(const Action: TAutomationData);
    procedure DoSet(const Action: TAutomationData);
    procedure DoVar(const Action: TAutomationData);
    function GetName(const Action: TAutomationData): string;
    function WaitingForCondition: boolean;
    function ConditionIsTrue(const Action: TAutomationData): boolean;
    function GetObj(const ComponentAndOrProperty: string): string;
    function GetProp(const ComponentAndProperty: string): string;

    function LogFile: string;
    function IsAvailable(const Control: TControl; const RedactedMsg: string): boolean;
    function Compare(const a, b: string): integer;
    function ReplaceVar(const s: string): string;
    function GetPropValue(const ComponentAndProperty: string): TValue;
    procedure SplitVar(const s: string; out VarName: string;
      out Operation: TVarOperation; out VarExtra: string);
    function DoMath(const Value: string; const Operation: TVarOperation;
      const Extra: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    class procedure LoadActions(const aScriptFile, aLogFile: string; const TimeOutInMs: integer = 5000 * 60); static;
    class function IsAutomating: boolean; static;
  end;

implementation
uses Windows, IOUtils, DateUtils, Vcl.Buttons, Vcl.ActnList, Messages;

function GetDynamicPropValue(const aComponent: TObject; const aPropertyName: string): TValue;
begin
  var RttiContext := TRttiContext.Create;
  try
    var RttiType := RttiContext.GetType(aComponent.ClassType);

    var RttiProperty := RttiType.GetProperty(aPropertyName);
    if Assigned(RttiProperty)
       then exit(RttiProperty.GetValue(aComponent));

    var RttiField := RttiType.GetField(aPropertyName);
    if Assigned(RttiField)
       then exit(RttiField.GetValue(aComponent));

    var RttiMethod := RttiType.GetMethod(aPropertyName);
    if Assigned(RttiMethod)
       then exit(RttiMethod.Invoke(aComponent, []));

    raise Exception.CreateFmt('Property "%s" not found.', [aPropertyName]);
  finally
    RttiContext.Free;
  end;
end;

procedure RTTISetValue(aComponent: TObject; const DataMember: TRttiDataMember;  const aPropertyName, Value: string);
begin
  case DataMember.DataType.TypeKind of
    tkInteger     : DataMember.SetValue(aComponent, Value.ToInteger);
    tkChar        :
      begin
        if Value.Length <> 1 then raise Exception.Create('Property ' + aPropertyName + ' is of type char. Its value must be a single character.');
        DataMember.SetValue(aComponent, Value.Chars[0]);
      end;
    //tkEnumeration includes boolean. GetEnumValue accepts "true" and "false" too.
    tkEnumeration :
      begin
        var Ordinal := GetEnumValue(DataMember.DataType.Handle, Value);
        if Ordinal < 0 then raise Exception.Create('"' + Value + '" is not a valid value for property ' + aPropertyName + '.');
        DataMember.SetValue(aComponent, TValue.FromOrdinal(DataMember.DataType.Handle, Ordinal));
      end;

    tkFloat       : DataMember.SetValue(aComponent, Value.ToSingle);
    tkString      : DataMember.SetValue(aComponent, Value);
    //tkSet         = System.TTypeKind.tkSet;
    //tkClass       = System.TTypeKind.tkClass;
    //tkMethod      = System.TTypeKind.tkMethod;
    //tkWChar       = System.TTypeKind.tkWChar;
    //tkLString     = System.TTypeKind.tkLString;
    //tkWString     = System.TTypeKind.tkWString;
    tkVariant     : DataMember.SetValue(aComponent, Value);
    //tkArray       = System.TTypeKind.tkArray;
    //tkRecord      = System.TTypeKind.tkRecord;
    //tkInterface   = System.TTypeKind.tkInterface;
    tkInt64       : DataMember.SetValue(aComponent, Value.ToInt64);
    //tkDynArray    = System.TTypeKind.tkDynArray;
    tkUString     : DataMember.SetValue(aComponent, Value);
    //tkClassRef    = System.TTypeKind.tkClassRef;
    //tkPointer     = System.TTypeKind.tkPointer;
    //tkProcedure   = System.TTypeKind.tkProcedure;
    else raise Exception.Create('Property ' + aPropertyName + ' has an unsupported type.');
  end;

end;

procedure SetDynamicPropValue(const aComponent: TObject; const aPropertyName, Value: string);
begin
  var RttiContext := TRttiContext.Create;
  try
    var RttiType := RttiContext.GetType(aComponent.ClassType);

    var RttiProperty := RttiType.GetProperty(aPropertyName);
    if Assigned(RttiProperty) then
    begin
      RTTISetValue(aComponent, RttiProperty, aPropertyName, Value);
      exit;
    end;

    var RttiField := RttiType.GetField(aPropertyName);
    if Assigned(RttiField) then
    begin
      RTTISetValue(aComponent, RttiField, aPropertyName, Value);
      exit;
    end;

    raise Exception.CreateFmt('Property "%s" not found.', [aPropertyName]);
  finally
    RttiContext.Free;
  end;
end;

procedure CallDynamicMethod(const aComponent: TObject; const aMethodName, Argument: string);
begin
  var RttiContext := TRttiContext.Create;
  try
    var RttiType := RttiContext.GetType(aComponent.ClassType);

    var RttiMethod := RttiType.GetMethod(aMethodName);
    if Assigned(RttiMethod) then
    begin
      var Params := RttiMethod.GetParameters;
      case Length(Params) of
        0:
          begin
            if Argument <> '' then raise Exception.CreateFmt('Method "%s" has no parameters, but it was called with the argument "%s".', [aMethodName, Argument]);
            RttiMethod.Invoke(aComponent, []);
          end;
        1:
          begin
            if not (Params[0].ParamType.TypeKind in [tkUString, tkString, tkLString, tkWString]) then raise Exception.CreateFmt('Method "%s" has a parameter that is not a string.', [aMethodName]);
            RttiMethod.Invoke(aComponent, [Argument]);
          end;
        else raise Exception.CreateFmt('Method "%s" has more than one parameter. Only methods with zero or one string parameter can be called.', [aMethodName]);
      end;
      exit;
    end;

    raise Exception.CreateFmt('Method "%s" not found.', [aMethodName]);
  finally
    RttiContext.Free;
  end;
end;


{ TGUIAutomator }

constructor TGUIAutomator.Create;
begin
  Actions := TList<TAutomationData>.Create;
  Variables := TDictionary<string, string>.Create(TIStringComparer.Ordinal);
  ApplicationEvents := TApplicationEvents.Create(nil);
end;

destructor TGUIAutomator.Destroy;
begin
  ApplicationEvents.Free;
  Actions.Free;
  Variables.Free;
  inherited;
end;

function TGUIAutomator.LogFile: string;
begin
  if FLogFile.Trim <> '' then exit(TPath.GetFullPath(FLogFile));
  exit(TPath.GetFullPath(DefaultLogFile));
end;


procedure TGUIAutomator.Load(const aScriptFile, aLogFile: string; const TimeOutInMs: integer);
begin
  MaxExecutionDate := TDateTime.NowUTC.IncMilliSecond(TimeOutInMs);
  FLogFile := aLogFile;
  Actions.Clear;
  Variables.Clear;

  TDirectory.CreateDirectory(TPath.GetDirectoryName(LogFile));
  SysUtils.DeleteFile(LogFile);
  Log('Starting Automation');

  ActionCounter := 0;
  try
    ParseActions(TFile.ReadAllLines(aScriptFile));
  except on ex: Exception do
    begin
      //This runs from FormCreate. If we let the exception escape, the GUI shows an error dialog and the test hangs.
      Log('ERROR: Cannot load automation script "' + aScriptFile + '": ' + ex.Message);
      ExitProcess(10);
    end;
  end;

  ApplicationEvents.OnIdle := MainLoop;
end;

procedure TGUIAutomator.ParseActions(const aActions: TArray<string>);
begin
  for var Entry in aActions do
  begin
    var EntryTrimmed := Entry.Trim;
    if (EntryTrimmed = '') or (EntryTrimmed.StartsWith('#')) or (EntryTrimmed.StartsWith('//')) then continue;
    var IdxEndCommand := EntryTrimmed.IndexOf(' ');
    if IdxEndCommand <= 0 then raise Exception.Create('Invalid line: "' + EntryTrimmed + '". It must have a command.' );
    var Command := EntryTrimmed.Substring(0, IdxEndCommand);

    var Rest := EntryTrimmed.Substring(IdxEndCommand + 1).Trim;
    var IdxEndTarget := Rest.IndexOfAny([' ', '=', '<', '>', '~', '!']);
    if IdxEndTarget <= 0 then IdxEndTarget := Rest.Length;
    var Target := Rest.Substring(0, IdxEndTarget).Trim;
    if Target = '' then raise Exception.Create('Invalid line: "' + EntryTrimmed + '". It must have a command and a target.' );

    var Data := Rest.Substring(IdxEndTarget).Trim;

    var Action := TAutomationData.Create(TAutomationActionType.Log, Target, Data);

    if Command.StartsWith('*') then
    begin
      Action.SkipLog := true;
      Command := Command.Substring(1).Trim;
    end;
    if SameText(Command, 'assert') then Action.Action := TAutomationActionType.Assert
    else if SameText(Command, 'log') then Action.Action := TAutomationActionType.Log
    else if SameText(Command, 'wait') then Action.Action := TAutomationActionType.Wait
    else if SameText(Command, 'click') then Action.Action := TAutomationActionType.Click
    else if SameText(Command, 'call') then Action.Action := TAutomationActionType.Call
    else if SameText(Command, 'set-text') then Action.Action := TAutomationActionType.SetText
    else if SameText(Command, 'set') then Action.Action := TAutomationActionType.Set
    else if SameText(Command, 'var') then Action.Action := TAutomationActionType.Var
    else raise Exception.Create('Unknown action: "' + Command + '". In Line "' + EntryTrimmed + '".');

    Actions.Add(Action);
  end;
end;

class procedure TGUIAutomator.LoadActions(const aScriptFile, aLogFile: string; const TimeOutInMs: integer = 5000 * 60);
begin
  Instance.Load(aScriptFile, aLogFile, TimeOutInMs);
end;

class constructor TGUIAutomator.Create;
begin
  Instance := TGUIAutomator.Create;
end;

class destructor TGUIAutomator.Destroy;
begin
  Instance.Free;
end;

procedure TGUIAutomator.Log(const Message: string);
begin
  TFile.AppendAllText(LogFile, Message + #13#10, TEncoding.UTF8);
end;

function TGUIAutomator.GetName(const Action: TAutomationData): string;
begin
  Result := '"' + Screen.ActiveForm.Name +'.' + Action.Target + '"';
end;

function TGUIAutomator.IsAvailable(const Control: TControl; const RedactedMsg: string): boolean;
begin
  if not Control.Enabled then begin Log(RedactedMsg + ' because it is disabled'); exit(false); end;
  if not Control.Visible then begin Log(RedactedMsg + ' because it is not visible'); exit(false); end;
  Result := true;
end;

function TGUIAutomator.GetObj(const ComponentAndOrProperty: string): string;
begin
  var Idx := ComponentAndOrProperty.IndexOf('.');
  if Idx < 0 then exit(ComponentAndOrProperty);
  exit(ComponentAndOrProperty.Substring(0, Idx).Trim);
end;

function TGUIAutomator.GetProp(const ComponentAndProperty: string): string;
begin
  var Idx := ComponentAndProperty.IndexOf('.');
  if Idx < 0 then raise Exception.Create('Invalid property value for "' + ComponentAndProperty + '". It must be in the form <component.property>');

  Result := ComponentAndProperty.Substring(Idx + 1).Trim;
  if Result = '' then raise Exception.Create('Invalid property value for "' + ComponentAndProperty + '". It must be in the form <component.property>');
end;

function TGUIAutomator.GetComponent<T>(const ComponentAndOrProperty, ComponentType: string; const MustBeType: boolean = true): T;
begin
  var ComponentName := GetObj(ComponentAndOrProperty);
  var Component: TComponent;
  if SameText(ComponentName, 'self') then Component := Screen.ActiveForm
    else Component := Screen.ActiveForm.FindComponent(ComponentName);

  if not Assigned(Component) then raise Exception.Create('Cannot find ' + ComponentType + ': "' + ComponentName + '"');
  if not (Component is T)
    then if not MustBeType then exit(nil)
         else raise Exception.Create('Component: "' + ComponentName + '" is not a ' + ComponentType +'. It is a ' + Component.ClassName);
  Result := T(Component);
end;

procedure TGUIAutomator.MainLoop(Sender: TObject; var Done: Boolean);
begin
  if TDateTime.NowUTC > MaxExecutionDate then
  begin
    Log('ERROR: Running time exceeded the maximum allowed.');
    ExitProcess(10);
    exit;
  end;

  //done = true refreshes the actions between calls, but hangs if zero messages are arriving to the window (no mouse moving, etc)
  //this is because TApplication.Idle has a line: if Done then WaitMessage;
  //If we set it false, then TApplication.Idle skips refreshing TActions (It doesn't call DoActionIdle)
  Done := true;
  if (Screen.ActiveForm <> nil) then PostMessage(Screen.ActiveForm.Handle, WM_NULL, 0, 0); //sends a message so the loop doesn't keep waiting for a message.


  if (Screen.ActiveForm = nil) or (TDateTime.NowUTC < SleepingUntilTime) or WaitingForCondition then
  begin
    exit;
  end;
  if ActionCounter >= Actions.Count then
  begin
    Log('Completed successfully');
    Application.Terminate;
    exit;
  end;

  //SkipLog is saved and restored because this is reentrant: a nested action inside a ShowModal would change it for the outer one.
  var SavedSkipLog := SkipLog;
  try
    try
      Inc(ActionCounter); //inc first, this is reentrant in showmodal.
      SkipLog := Actions[ActionCounter - 1].SkipLog;
      RunAction(Actions[ActionCounter - 1]);
    except on ex: Exception do
      begin
        Log('ERROR: ' + Redact(ex.Message));
        ExitProcess(10);
      end;
    end;
  finally
    SkipLog := SavedSkipLog;
  end;

end;

function TGUIAutomator.Compare(const a, b: string): integer;
begin
  var aInt, bInt: Int64;
  if TryStrToInt64(a, aInt) and TryStrToInt64(b, bInt) then exit(TComparer<Int64>.Default.Compare(aInt, bInt));
  Result := CompareText(a, b);
end;

procedure TGUIAutomator.RunAction(const Action: TAutomationData);
begin
  case Action.Action of
    TAutomationActionType.Assert: DoAssert(Action);
    TAutomationActionType.Log: DoLog(Action);
    TAutomationActionType.Wait: DoWait(Action);
    TAutomationActionType.Click: DoClick(Action);
    TAutomationActionType.Call: DoCall(Action);
    TAutomationActionType.SetText: DoSetText(Action);
    TAutomationActionType.Set: DoSet(Action);
    TAutomationActionType.Var: DoVar(Action);
    else raise Exception.Create('Unknown action.');
  end;
end;

class function TGUIAutomator.IsAutomating: boolean;
begin
  Result := Assigned(Instance.ApplicationEvents.OnIdle);
end;

procedure TGUIAutomator.SplitVar(const s: string; out VarName: string; out Operation: TVarOperation; out VarExtra: string);
begin
  VarName := s;
  Operation := TVarOperation.None;
  VarExtra := '';

  var Idx := s.IndexOfAny(['+', '-']);
  if Idx < 0 then exit;

  VarName := s.Substring(Idx);
  case s.Chars[Idx] of
    '+': Operation := TVarOperation.Add;
    '-': Operation := TVarOperation.Subtract;
    else raise Exception.Create('Internal error: Operation not supported ' + s.Chars[Idx]);
  end;

  VarName := s.Substring(0, Idx).Trim;
  VarExtra := s.Substring(Idx + 1).Trim;
end;

function TGUIAutomator.DoMath(const Value: string; const Operation: TVarOperation; const Extra: string): string;
begin
  if Operation = TVarOperation.None then exit(Value);

  var v64: Int64;
  if not TryStrToInt64(Value, v64) then raise Exception.Create('To do operation with variables, the value must be integer: ' + Value);
  var x64: Int64;
  if not TryStrToInt64(Extra, x64) then raise Exception.Create('To do operation with variables, the extra part must be integer: ' + Extra);

  case Operation of
    Add: exit((v64 + x64).ToString);
    Subtract: exit((v64 - x64).ToString);
    else raise Exception.Create('Internal error. Unknown operation.');
  end;
end;

//SysUtils.GetEnvironmentVariable returns '' both for a missing variable and for an empty one. We need to tell them apart.
function TryGetEnvironmentVariable(const Name: string; out Value: string): boolean;
begin
  Value := '';
  SetLastError(0);
  var Len := Windows.GetEnvironmentVariable(PChar(Name), nil, 0); //Len includes the null terminator.
  if Len = 0 then exit(GetLastError <> ERROR_ENVVAR_NOT_FOUND);

  SetLength(Value, Len);
  Len := Windows.GetEnvironmentVariable(PChar(Name), PChar(Value), Len); //Len doesn't include the null terminator now.
  SetLength(Value, Len);
  Result := true;
end;

function TGUIAutomator.Redact(const Message: string): string;
begin
  if SkipLog then exit('****') else exit(Message);

end;

function TGUIAutomator.ReplaceVar(const s: string): string;
const
  EnvId = '$env:';
begin
  Result := s.Trim;
  if not Result.StartsWith('$') then exit;
  var VarName, VarExtra: string;
  var VarOperation: TVarOperation;
  SplitVar(Result, VarName, VarOperation, VarExtra);

  if VarName.StartsWith(EnvId, true) then
  begin
    var EnvName := VarName.Substring(EnvId.Length).Trim;
    var EnvValue: string;
    if not TryGetEnvironmentVariable(EnvName, EnvValue) then raise Exception.Create('Environment variable "' + EnvName + '" is not defined.');
    exit(DoMath(EnvValue, VarOperation, VarExtra));
  end;

  if SameText(VarName, '$nowUTC') then
  begin
    exit(DoMath(TDateTime.NowUTC.ToMilliseconds.ToString, VarOperation, VarExtra));
  end;

  var VarValue: string;
  if not Variables.TryGetValue(VarName, VarValue) then raise Exception.Create('Variable "' + Result + '" is not defined.');
  Result := DoMath(VarValue.Trim, VarOperation, VarExtra);
end;

function TGUIAutomator.GetPropValue(const ComponentAndProperty: string): TValue;
begin
  var VarValue: string;
  if ComponentAndProperty.StartsWith('$')
    then exit(ReplaceVar(ComponentAndProperty));

  var Component := GetComponent<TComponent>(ComponentAndProperty, 'component');
  Result := GetDynamicPropValue(Component, GetProp(ComponentAndProperty));
end;

function TGUIAutomator.ConditionIsTrue(const Action: TAutomationData): boolean;
begin
  if Action.Target = '' then exit(true);

  var Value := GetPropValue(Action.Target);

  if Action.Data = '' then
  begin
    if Value.IsType<Boolean> then exit(Value.AsType<Boolean>);
   raise Exception.Create('Property ' + GetName(Action) + ' should be a boolean, or you should specify the expected property value');
   end;

  if Action.Data.StartsWith('=') then exit(Compare(Value.ToString, ReplaceVar(Action.Data.Substring(1))) = 0);
  if Action.Data.StartsWith('<>') then exit(Compare(Value.ToString, ReplaceVar(Action.Data.Substring(2))) <> 0);
  if Action.Data.StartsWith('<=') then exit(Compare(Value.ToString, ReplaceVar(Action.Data.Substring(2))) <= 0);
  if Action.Data.StartsWith('>=') then exit(Compare(Value.ToString, ReplaceVar(Action.Data.Substring(2))) >= 0);
  if Action.Data.StartsWith('<') then exit(Compare(Value.ToString, ReplaceVar(Action.Data.Substring(1))) < 0);
  if Action.Data.StartsWith('>') then exit(Compare(Value.ToString, ReplaceVar(Action.Data.Substring(1))) > 0);
  if Action.Data.StartsWith('~') then exit(Value.ToString.ToLowerInvariant.Contains(ReplaceVar(Action.Data.Substring(1)).ToLowerInvariant));
  if Action.Data.StartsWith('!~') then exit(not Value.ToString.ToLowerInvariant.Contains(ReplaceVar(Action.Data.Substring(2)).ToLowerInvariant));
  raise Exception.Create('Comparison must start with "=", "<>", "<", ">", "<=", ">=", "~" or "!~": "' + Action.Data + '"');
end;

function TGUIAutomator.WaitingForCondition: boolean;
begin
  try
    Result := not ConditionIsTrue(SleepingUntilCondition);
    if not Result then SleepingUntilCondition.Target := '';
  except
    on ex: Exception do
    begin
      Log('ERROR: ' + Redact(ex.Message));
      ExitProcess(10);
      exit(true);
    end;
  end;
end;

procedure TGUIAutomator.DoAssert(const Action: TAutomationData);
begin
  var Assertion := Action.Target + ' ' + Action.Data;
  if not ConditionIsTrue(Action) then raise Exception.Create('assertion "' + Redact(Assertion) + '" is false');
  Log('assert: "' + Redact(Assertion) + '" is true');
end;

procedure TGUIAutomator.DoClick(const Action: TAutomationData);
begin
  var Button := GetComponent<TCustomButton>(Action.Target, 'button', false);
  if Button <> nil then
  begin
    if IsAvailable(Button, 'click: cannot click button ' + GetName(Action)) then
    begin
      Log('click: ' + GetName(Action));
      Button.Click;
    end;
    exit;
  end;

  var SpeedButton := GetComponent<TCustomSpeedButton>(Action.Target, 'speed button', false);
  if SpeedButton <> nil then
  begin
    if IsAvailable(SpeedButton, 'click: cannot click speed button ' + GetName(Action)) then
    begin
      Log('click: ' + GetName(Action));
      SpeedButton.Click;
    end;
    exit;
  end;

  var VCLAction := GetComponent<TCustomAction>(Action.Target, 'button, speedbutton or action');
  if VCLAction <> nil then
  begin
    if VCLAction.Enabled and VCLAction.Visible then
    begin
      Log('click: ' + GetName(Action));
      if not VCLAction.Execute then raise Exception.Create('click: cannot execute action ' + GetName(Action));
    end
    else Log('click: cannot click action ' + GetName(Action) + ' because it is disabled or not visible.');
  end;

end;

procedure TGUIAutomator.DoCall(const Action: TAutomationData);
begin
  var Control := GetComponent<TControl>(Action.Target, 'control');
  if IsAvailable(Control, 'call: cannot call action in component ' + GetName(Action)) then
  begin
    if Action.Data = '' then Log('call: ' + GetName(Action))
    else Log('call: ' + GetName(Action) + ' with "' + Redact(Action.Data) + '"');
    CallDynamicMethod(Control, GetProp(Action.Target), Action.Data);
  end;
end;

procedure TGUIAutomator.DoLog(const Action: TAutomationData);
begin
  var Value := GetPropValue(Action.Target);

  Log('log: property ' + GetName(Action) + ' has the value ' + Redact(Value.ToString.QuotedString));
end;

procedure TGUIAutomator.DoWait(const Action: TAutomationData);
begin
  var WaitTime: integer;
  if TryStrToInt(Action.Target, WaitTime) then
  begin
    SleepingUntilTime := TDateTime.NowUTC.IncMilliSecond(WaitTime);
    Log('wait: ' + Redact(Action.Target) + ' milliseconds');
    exit;
  end;

  SleepingUntilCondition := Action;
  Log('wait: condition ' + Action.Target + ' ' + Redact(Action.Data));
end;



procedure TGUIAutomator.DoSetText(const Action: TAutomationData);
begin
  var Edit := GetComponent<TCustomEdit>(Action.Target, 'edit box/memo');

  if IsAvailable(Edit, 'set-text: Cannot set text of ' + GetName(Action) + ' to "' + Redact(Action.Data)) then
  begin
    var Value := ReplaceVar(Action.Data);
    Edit.Text := Value;
    Log('set-text: ' + GetName(Action) + ' to "' + Redact(Value) +'"');
  end;
end;

procedure TGUIAutomator.DoSet(const Action: TAutomationData);
begin
  var Control := GetComponent<TControl>(Action.Target, 'control');

  if IsAvailable(Control, 'set: Cannot set property ' + GetName(Action) + ' to "' + Redact(Action.Data)) then
  begin
    SetDynamicPropValue(Control, GetProp(Action.Target), Action.Data);
    Log('set: ' + GetName(Action) + ' to "' + Redact(Action.Data) +'"');
  end;
end;

procedure TGUIAutomator.DoVar(const Action: TAutomationData);
begin
  if (not Action.Target.StartsWith('$')) or (Action.Target.Length < 2) then raise Exception.Create('Invalid variable name: "' + Action.Target + '". Variable names must start with "$".');

  var VarValue := GetPropValue(Action.Data).ToString;
  Variables.AddOrSetValue(Action.Target, VarValue);
  Log('var: "' + Action.Target + '" = "' + Redact(VarValue) + '"');
end;

{ TAutomationData }

constructor TAutomationData.Create(const aAction: TAutomationActionType;
  const aTarget, aData: string);
begin
  Action := aAction;
  Target := aTarget;
  Data := aData;
end;

{$ELSE}
interface
implementation
{$ENDIF}
end.

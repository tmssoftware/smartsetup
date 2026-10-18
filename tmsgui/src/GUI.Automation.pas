unit GUI.Automation;
{$IFDEF DEBUG}
(*
  Special words:
  self as component name -> refers to the active form.
  // or # mark a comment. Note: it should be a full line comment.
  ~ means contain. !~ means not contains. For example memo.text ~success means memo.text contains "success"
  Commands:
  assert <component.text> [=/<>/~/!~value] | assert self.name = form1 -> crashes the app if the form name is not "form1"
  log <component.value>  | log memo1.text -> logs the value of memo1.text
  wait <ms>  | wait 5000 -> waits for 5 seconds
  wait <component.property> [=/<>/~/!~value] -> wait button1.enabled -> waits until button is enabled
  click <button> | click button2 -> clicks button2
  call <component.method> | call listview1.SelectAll -> Selects all items
  set-text <edit> text |  set-text memo1 hello today  -> sets the memo value to "hello today"
  set <component.property> text |  set cb.checked true  -> sets the checkbox to checked.
*)

interface
uses SysUtils, Classes, Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.AppEvnts, Generics.Collections;

type
  TAutomationActionType = (
    Assert,
    Log,
    Wait,
    Click,
    Call,
    SetText,
    &Set
  );

  TAutomationData = record
    Action: TAutomationActionType;
    Target: string;
    Data: string;

    constructor Create(const aAction: TAutomationActionType; const aTarget, aData: string);
  end;

  TGUIAutomator = class
  private
    const LogFile = 'tmsgui.automation.log';
    class var Instance: TGUIAutomator;
  private
    ApplicationEvents: TApplicationEvents;
    Actions: TList<TAutomationData>;
    ActionCounter: integer;
    MaxExecutionDate: TDateTime;
    SleepingUntilTime: TDateTime;
    SleepingUntilCondition: TAutomationData;
    procedure MainLoop(Sender: TObject; var Done: Boolean);
    procedure RunAction(const Action: TAutomationData);

    class constructor Create;
    class destructor Destroy;
    procedure Load(const aFileName: string; const TimeOutInMs: integer);
    procedure ParseActions(const aActions: TArray<string>);

    procedure Log(const Message: string);

    function GetComponent<T: TComponent>(const ComponentAndOrProperty, ComponentType: string): T;
    procedure DoAssert(const Action: TAutomationData);
    procedure DoLog(const Action: TAutomationData);
    procedure DoWait(const Action: TAutomationData);
    procedure DoClick(const Action: TAutomationData);
    procedure DoCall(const Action: TAutomationData);
    procedure DoSetText(const Action: TAutomationData);
    procedure DoSet(const Action: TAutomationData);
    function GetName(const Action: TAutomationData): string;
    function WaitingForCondition: boolean;
    function ConditionIsTrue(const Action: TAutomationData): boolean;
    function GetObj(const ComponentAndOrProperty: string): string;
    function GetProp(const ComponentAndProperty: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    class procedure LoadActions(const aFileName: string; const TimeOutInMs: integer = 5000 * 60); static;

  end;

implementation
uses Windows, IOUtils, TypInfo, RTTI, DateUtils;

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

procedure CallDynamicMethod(const aComponent: TObject; const aMethodName: string);
begin
  var RttiContext := TRttiContext.Create;
  try
    var RttiType := RttiContext.GetType(aComponent.ClassType);

    var RttiMethod := RttiType.GetMethod(aMethodName);
    if Assigned(RttiMethod) then
    begin
      RttiMethod.Invoke(aComponent, []);
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

  ApplicationEvents := TApplicationEvents.Create(nil);
end;

destructor TGUIAutomator.Destroy;
begin
  ApplicationEvents.Free;
  Actions.Free;
  inherited;
end;

procedure TGUIAutomator.Load(const aFileName: string; const TimeOutInMs: integer);
begin
  MaxExecutionDate := TDateTime.NowUTC.IncMilliSecond(TimeOutInMs);
  Actions.Clear;

  DeleteFile(LogFile);
  Log('Starting Automation');

  ActionCounter := 0;
  try
    ParseActions(TFile.ReadAllLines(aFileName));
  except on ex: Exception do
    begin
      //This runs from FormCreate. If we let the exception escape, the GUI shows an error dialog and the test hangs.
      Log('ERROR: Cannot load automation script "' + aFileName + '": ' + ex.Message);
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
    var IdxEndTarget := Rest.IndexOfAny([' ', '=', '<']);
    if IdxEndTarget <= 0 then IdxEndTarget := Rest.Length;
    var Target := Rest.Substring(0, IdxEndTarget).Trim;
    if Target = '' then raise Exception.Create('Invalid line: "' + EntryTrimmed + '". It must have a command and a target.' );

    var Data := Rest.Substring(IdxEndTarget).Trim;

    var Action := TAutomationData.Create(TAutomationActionType.Log, Target, Data);

    if SameText(Command, 'assert') then Action.Action := TAutomationActionType.Assert
    else if SameText(Command, 'log') then Action.Action := TAutomationActionType.Log
    else if SameText(Command, 'wait') then Action.Action := TAutomationActionType.Wait
    else if SameText(Command, 'click') then Action.Action := TAutomationActionType.Click
    else if SameText(Command, 'call') then Action.Action := TAutomationActionType.Call
    else if SameText(Command, 'set-text') then Action.Action := TAutomationActionType.SetText
    else if SameText(Command, 'set') then Action.Action := TAutomationActionType.Set
    else raise Exception.Create('Unknown action: "' + Command + '". In Line "' + EntryTrimmed + '".');

    Actions.Add(Action);
  end;
end;

class procedure TGUIAutomator.LoadActions(const aFileName: string; const TimeOutInMs: integer = 5000 * 60);
begin
  Instance.Load(aFileName, TimeOutInMs);
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
  TFile.AppendAllText(LogFile, Message + #10, TEncoding.UTF8);
end;

function TGUIAutomator.GetName(const Action: TAutomationData): string;
begin
  Result := '"' + Screen.ActiveForm.Name +'.' + Action.Target + '"';
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

function TGUIAutomator.GetComponent<T>(const ComponentAndOrProperty, ComponentType: string): T;
begin
  var ComponentName := GetObj(ComponentAndOrProperty);
  var Component: TComponent;
  if SameText(ComponentName, 'self') then Component := Screen.ActiveForm
    else Component := Screen.ActiveForm.FindComponent(ComponentName);

  if not Assigned(Component) then raise Exception.Create('Cannot find ' + ComponentType + ': "' + ComponentName + '"');
  if not (Component is T) then raise Exception.Create('Component: "' + ComponentName + '" is not a ' + ComponentType +'. It is a ' + Component.ClassName);
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

  Done := false;
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

  try
    Inc(ActionCounter); //inc first, this is reentrant in showmodal.
    RunAction(Actions[ActionCounter - 1]);
  except on ex: Exception do
    begin
      Log('ERROR: ' + ex.Message);
      ExitProcess(10);
    end;
  end;

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
    else raise Exception.Create('Unknown action.');
  end;
end;

function TGUIAutomator.ConditionIsTrue(const Action: TAutomationData): boolean;
begin
  if Action.Target = '' then exit(true);

  var Component := GetComponent<TComponent>(Action.Target, 'component');
  var Value := GetDynamicPropValue(Component, GetProp(Action.Target));

  if Action.Data = '' then
  begin
    if Value.IsType<Boolean> then exit(Value.AsType<Boolean>);
   raise Exception.Create('Property ' + GetName(Action) + ' should be a boolean, or you should specify the expected property value');
   end;

  if Action.Data.StartsWith('=') then exit(SameText(Value.ToString, Action.Data.Substring(1).Trim));
  if Action.Data.StartsWith('<>') then exit(not SameText(Value.ToString, Action.Data.Substring(2).Trim));
  if Action.Data.StartsWith('~') then exit(Value.ToString.ToLowerInvariant.Contains(Action.Data.Substring(1).Trim.ToLowerInvariant));
  if Action.Data.StartsWith('!~') then exit(not Value.ToString.ToLowerInvariant.Contains(Action.Data.Substring(2).Trim.ToLowerInvariant));
  raise Exception.Create('Comparison must start with "=", "<>", "~" or "!~": "' + Action.Data + '"');
end;

function TGUIAutomator.WaitingForCondition: boolean;
begin
  try
    Result := not ConditionIsTrue(SleepingUntilCondition);
    if not Result then SleepingUntilCondition.Target := '';
  except
    on ex: Exception do
    begin
      Log('ERROR: ' + ex.Message);
      ExitProcess(10);
      exit(true);
    end;
  end;
end;

procedure TGUIAutomator.DoAssert(const Action: TAutomationData);
begin
  var Assertion := Action.Target + ' ' + Action.Data;
  if not ConditionIsTrue(Action) then raise Exception.Create('assertion "' + Assertion + '" is false');
  Log('assert: "' + Assertion + '" is true');
end;

procedure TGUIAutomator.DoClick(const Action: TAutomationData);
begin
  var Button := GetComponent<TButton>(Action.Target, 'button');
  if Button.Enabled then
  begin
    Log('click: ' + GetName(Action));
    Button.Click;
  end else
  begin
    Log('click: cannot click button ' + GetName(Action) + ' because it is disabled');
  end;
end;

procedure TGUIAutomator.DoCall(const Action: TAutomationData);
begin
  var Control := GetComponent<TControl>(Action.Target, 'control');
  if Control.Enabled then
  begin
    Log('call: ' + GetName(Action));
    CallDynamicMethod(Control, GetProp(Action.Target));
  end else
  begin
    Log('call: cannot call action in component ' + GetName(Action) + ' because it is disabled');
  end;
end;

procedure TGUIAutomator.DoLog(const Action: TAutomationData);
begin
  var Component := GetComponent<TComponent>(Action.Target, 'component');
  var Value := GetDynamicPropValue(Component, GetProp(Action.Target));

  Log('log: property ' + GetName(Action) + ' has the value ' + Value.ToString.QuotedString);
end;

procedure TGUIAutomator.DoWait(const Action: TAutomationData);
begin
  var WaitTime: integer;
  if TryStrToInt(Action.Target, WaitTime) then
  begin
    SleepingUntilTime := TDateTime.NowUTC.IncMilliSecond(WaitTime);
    Log('wait: ' + Action.Target + ' milliseconds');
    exit;
  end;

  SleepingUntilCondition := Action;
  Log('wait: condition ' + Action.Target + Action.Data);
end;



procedure TGUIAutomator.DoSetText(const Action: TAutomationData);
begin
  var Edit := GetComponent<TCustomEdit>(Action.Target, 'edit box/memo');

  if Edit.Enabled then
  begin
    Edit.Text := Action.Data;
    Log('set-text: ' + GetName(Action) + ' to "' + Action.Data +'"');
  end else
  begin
    Log('set-text: Cannot set text of ' + GetName(Action) + ' to "' + Action.Data +'" because the control is disabled.');
  end;
end;

procedure TGUIAutomator.DoSet(const Action: TAutomationData);
begin
  var Control := GetComponent<TControl>(Action.Target, 'control');

  if Control.Enabled then
  begin
    SetDynamicPropValue(Control, GetProp(Action.Target), Action.Data);
    Log('set: ' + GetName(Action) + ' to "' + Action.Data +'"');
  end else
  begin
    Log('set: Cannot set property ' + GetName(Action) + ' to "' + Action.Data +'" because the control is disabled.');
  end;
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

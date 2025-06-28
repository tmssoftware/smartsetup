unit BBClasses;
{$i ../tmscommon.inc}

interface
uses Generics.Collections;

type
TErrorInfo = class
private
  FIgnoreOtherFiles: boolean;
public
  property IgnoreOtherFiles: boolean read FIgnoreOtherFiles;
  constructor Create(const aIgnoreOtherFiles: boolean);

  // function ToString: string; virtual; abstract;
end;

TSection = class;

TSectionDictionary = class (TObjectDictionary<string, TSection>)
  Constructor Create;
end;

TAction = reference to procedure(Value: string; ErrorInfo: TErrorInfo);
TActionNameValue = reference to procedure(Name, Value: string; ErrorInfo: TErrorInfo);
TChildSectionAction = reference to function(Name: string; ErrorInfo: TErrorInfo): TSection;

TListOfActions = TDictionary<string, TAction>;

TSectionValueTypes = (Values, NoValues, Both);

TSection = class
private
  function ListSectionsAndActions: string;
  class function GetActions(const Act: TListOfActions): string;

public
  ChildSections: TSectionDictionary;
  ChildSectionAction: TChildSectionAction; //if defined, ChildSections is not used.
  Parent: TSection;

  function FullPath: string;
  function GotoChild(const Line: string; const ErrorInfo: TErrorInfo; const KeepValues: boolean = false): TSection;
  function GotoParent: TSection;
  class function RemoveDoubleSpaces(const s: string): string;

  function GetBool(const s: string; const ErrorInfo: TErrorInfo): boolean;
  function GetBoolEx(const s: string; const ErrorInfo: TErrorInfo): boolean;
  function GetInt(const s: string; const ErrorInfo: TErrorInfo): integer;

public
  SectionValueTypes: TSectionValueTypes; //Only needed to set in data sections.
  Actions: TListOfActions;
  ArrayMainAction: TActionNameValue;
  ContainsArrays: Boolean; //Only needs to be set it if not setting ArrayMainAction.
  ClearArrayValues: TAction;  //allows to clear an array before adding new values.
  Duplicated: TDictionary<string, boolean>; // keep it nil to allow duplicated values.
  ArrayActions: TListOfActions;

  procedure ThrowInvalidTag(const Name: string; const ErrorInfo: TErrorInfo);
  class procedure GetArray(const s: string; const ArrActions: TListOfActions; const CallAction: TAction; const ErrorInfo: TErrorInfo);

  function VarPrefix: string; virtual;
  function ExtraInfo: string; virtual;

public
  constructor Create(const aParent: TSection);
  destructor Destroy; override;

  class function SectionNameStatic: string; virtual; abstract;
  function SectionName: string; virtual;
  function FullSectionName: string;
end;

implementation
uses Classes, SysUtils;

{ TSectionDictionary }

constructor TSectionDictionary.Create;
begin
  inherited Create([doOwnsValues]);
end;

{ TSection }

constructor TSection.Create(const aParent: TSection);
begin
  Parent := aParent;
  ChildSections := TSectionDictionary.Create;
end;

destructor TSection.Destroy;
begin
  ChildSections.Free;
  Actions.Free;
  ArrayActions.Free;
  Duplicated.Free;
  inherited;
end;

function TSection.ExtraInfo: string;
begin
  Result := '';
end;

function TSection.FullPath: string;
var
  p: TSection;
begin
  Result := SectionName;
  P := Parent;

  while P <> nil do
  begin
    Result := P.SectionName + '->';
    P := P.Parent;
  end;

end;

function TSection.FullSectionName: string;
begin
  if Parent <> nil then exit(Parent.SectionName + ':' + SectionName);
  Result := SectionName;
end;

class function TSection.GetActions(const Act: TListOfActions): string;
var
  Sep: string;
begin
  if (Act = nil) then exit('');
  Result := '';
  Sep := '';
  for var d in Act.Keys do
  begin
    Result := Result + Sep + d;
    Sep := ', ';
  end;
end;


function TSection.ListSectionsAndActions: string;
var
  Sep: string;
begin
  Result := '';
  Sep := '';
  if ChildSections <> nil then
  begin
    for var v in ChildSections.Values do
    begin
      Result := Result + sep + '"' + v.SectionName +'"';
      Sep := ', ';
    end;
  end;

  if Actions <> nil then
  begin
    for var v in Actions.Keys do
    begin
      Result := Result + sep + '"' + v +'"';
      Sep := ', ';
    end;
  end;

end;

class function TSection.RemoveDoubleSpaces(const s: string): string;
begin
  if s = '' then exit('');
  Result := '';
  SetLength(Result, Length(s));
  Result[1] := s[1];
  var iResult := 1;
  for var ist := 2 to Length(s) do
  begin
    if (s[ist] = ' ') and (s[ist - 1] = ' ') then
    begin
      //skip
    end else
    begin
      inc (iResult);
      Result[iResult] := s[ist];
    end;

  end;
  SetLength(Result, iResult);
end;

function TSection.SectionName: string;
begin
  Result := SectionNameStatic;
end;

procedure TSection.ThrowInvalidTag(const Name: string;
  const ErrorInfo: TErrorInfo);
begin
  raise Exception.Create('Invalid tag "' + Name + '" for section "' + FullSectionName +
  '". It must be one of [' + ListSectionsAndActions + ']. ' + ErrorInfo.ToString);
end;

function TSection.VarPrefix: string;
begin
  Result := '';
end;

class procedure TSection.GetArray(const s: string; const ArrActions: TListOfActions; const CallAction: TAction; const ErrorInfo: TErrorInfo);
var
  Act: TAction;
begin
  if not s.StartsWith('[') then raise Exception.Create('"' + s + '" is not a valid array. It must be between square brackets, like [value1, value2]. ' + ErrorInfo.ToString);
  var varr := s.Substring(1, s.Length - 2).Split([',']);

  for var v0 in varr do
  begin
    var v := v0.Trim;
    if ArrActions <> nil then
    begin
      if not ArrActions.TryGetValue(v, Act) then raise Exception.Create('The value "' + v +'" in the array ' + s + ' is not a valid value. It must be one of [' + GetActions(ArrActions) + ']. ' + ErrorInfo.ToString );
      Act(v, ErrorInfo);
    end else CallAction(v, ErrorInfo);
  end;
end;

function TSection.GetBool(const s: string;
  const ErrorInfo: TErrorInfo): boolean;
var
  s1: string;
begin
 s1 := AnsiLowerCase(s);
 if (s1 = 'true') or (s1='1') or (s1 = 'on') or (s1 = 'yes') then exit(true);
 if (s1 = 'false') or (s1='0') or (s1 = 'off') or (s1 = 'no') then exit(false);

 raise Exception.Create('"' + s + '" is not a valid boolean value. It must be true, 1, on, yes, false, 0, off or no. ' + ErrorInfo.ToString);

end;

function TSection.GetBoolEx(const s: string;
  const ErrorInfo: TErrorInfo): boolean;
begin
  if s = '' then exit(true);
  exit(GetBool(s, ErrorInfo));
end;

function TSection.GetInt(const s: string;
  const ErrorInfo: TErrorInfo): integer;
begin
 if not TryStrToInt(s, Result) then
   raise Exception.Create('"' + s + '" is not a valid integer value. ' + ErrorInfo.ToString);

end;

function TSection.GotoChild(const Line: string; const ErrorInfo: TErrorInfo; const KeepValues: boolean): TSection;
begin
  if Assigned(ChildSectionAction) then
  begin
    var ChildAction := ChildSectionAction(Line, ErrorInfo);
    if ChildAction <> nil then
    begin
      if Assigned(ChildAction.ClearArrayValues) then ChildAction.ClearArrayValues(Line, ErrorInfo);
      exit(ChildAction);
    end;

  end;
  if not ChildSections.TryGetValue(Line, Result) then
  begin
    raise Exception.Create('"' + Line +
      '" is an invalid child section for "' + FullSectionName + '". It must be one of: ['
      + ListSectionsAndActions + ']. '+ ErrorInfo.ToString);
  end;

  if not KeepValues and Assigned(Result) and Assigned(Result.ClearArrayValues) then Result.ClearArrayValues(Line, ErrorInfo);
end;

function TSection.GotoParent: TSection;
begin
  Result := Parent;
end;

{ TErrorInfo }

constructor TErrorInfo.Create(const aIgnoreOtherFiles: boolean);
begin
  FIgnoreOtherFiles := aIgnoreOtherFiles;
end;

end.

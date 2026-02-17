unit BBClasses;
{$i ../tmscommon.inc}

interface
uses Classes, SysUtils, Generics.Collections, BBArrays, BBStrings;

const
  SectionAddPrefix = 'add ';
  SectionReplacePrefix = 'replace ';

  function TArrayOverrideBehavior_FromString(const value: string): TArrayOverrideBehavior;
  function TArrayOverrideBehavior_ToStringPrefix(const value: TArrayOverrideBehavior): string;

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

TSectionDictionary = class
private
  FData: TObjectDictionary<string, TSection>;
public
  constructor Create;
  destructor Destroy; override;
  function Values: TEnumerable<TSection>;
  function TryGetValue(const name: string; out Section: TSection; const ErrorInfo: TErrorInfo; const KeepValues: boolean): boolean;
  procedure Add(const aKey: string; const aValue: TSection);
  function Count: integer;
end;

TAction = reference to procedure(Value: string; ErrorInfo: TErrorInfo);
TActionNameValue = reference to procedure(Name, Value: string; ErrorInfo: TErrorInfo);
TChildSectionAction = reference to function(Name: string; ErrorInfo: TErrorInfo; const KeepValues: boolean): TSection;

TListOfActions = TDictionary<string, TAction>;

TSectionValueTypes = (Values, NoValues, Both);

TSection = class
private
  FParent: TSection;
  FCreatedBy: string;
  FChildSections: TSectionDictionary;

  function ListSectionsAndActions: string;
  class function GetActions(const Act: TListOfActions): string;

public
  ChildSectionAction: TChildSectionAction; //if defined, ChildSections is not used.

  function FullPath: string;
  function GotoChild(const Line: string; const ErrorInfo: TErrorInfo; const KeepValues: boolean = false): TSection;
  function GotoParent: TSection;
  class function RemoveDoubleSpaces(const s: string): string;

  function GetBool(const s: string; const ErrorInfo: TErrorInfo): boolean;
  function GetBoolEx(const s: string; const ErrorInfo: TErrorInfo): boolean;
  function GetInt(const s: string; const ErrorInfo: TErrorInfo): integer;

  property CreatedBy: string read FCreatedBy write FCreatedBy;

public
  SectionValueTypes: TSectionValueTypes; //Only needed to set in data sections.
  Actions: TListOfActions;
  ArrayMainAction: TActionNameValue;
  ContainsArrays: Boolean; //Only needs to be set it if not setting ArrayMainAction.
  ArraysCanBeKeys: boolean; //For backwards compat. A key can't be repeated in yaml, but we allowed it sometimes. When set to true, we will allow both "- value:" and "value:" values. This property is *only* for wrong existing data. Don't use it for new data.

  ClearArrayValues: TProc;  //allows to clear an array before adding new values.

  Duplicated: TDictionary<string, boolean>; // keep it nil to allow duplicated values.
  ArrayActions: TListOfActions;

  procedure ThrowInvalidTag(const Name: string; const ErrorInfo: TErrorInfo);
  class procedure GetFlowArray(const s: string; const ArrActions: TListOfActions; const CallAction: TAction; const ErrorInfo: TErrorInfo);

  function ExtraInfo: string; virtual;

  procedure LoadedState(const State: TArrayOverrideBehavior); virtual;

  property Parent: TSection read FParent;
  property ChildSections: TSectionDictionary read FChildSections;
  function Root: TSection;

public
  constructor Create(const aParent: TSection);
  destructor Destroy; override;

  class function SectionNameStatic: string; virtual; abstract;
  function SectionName: string; virtual;
  function FullSectionName: string;
end;

implementation

{ TSectionDictionary }
constructor TSectionDictionary.Create;
begin
  FData := TObjectDictionary<string, TSection>.Create([doOwnsValues]);
end;

destructor TSectionDictionary.Destroy;
begin
  FData.Free;
  inherited;
end;

function TSectionDictionary.Count: integer;
begin
  Result := FData.Count;
end;

procedure TSectionDictionary.Add(const aKey: string; const aValue: TSection);
begin
  FData.Add (aKey, aValue);
end;



function TSectionDictionary.TryGetValue(const name: string;
  out Section: TSection; const ErrorInfo: TErrorInfo; const KeepValues: boolean): boolean;
begin
  if FData.TryGetValue(name, Section) then
  begin
    if not KeepValues and Assigned(Section.ClearArrayValues) then Section.ClearArrayValues();
    Section.LoadedState(TArrayOverrideBehavior.None);
    exit(true);
  end;

  if name.StartsWith(SectionAddPrefix, true) then
  begin
    if FData.TryGetValue(name.Substring(SectionAddPrefix.Length), Section) then
    begin
      Section.LoadedState(TArrayOverrideBehavior.Add);
      exit(Assigned(Section.ClearArrayValues));
    end;
  end;

  if name.StartsWith(SectionReplacePrefix, true) then
  begin
    if FData.TryGetValue(name.Substring(SectionReplacePrefix.Length), Section) then
    begin
      if not KeepValues and Assigned(Section.ClearArrayValues) then Section.ClearArrayValues();
      Section.LoadedState(TArrayOverrideBehavior.Replace);
      exit(Assigned(Section.ClearArrayValues));
    end;
  end;

  Result := false;
end;

function TSectionDictionary.Values: TEnumerable<TSection>;
begin
  Result := FData.Values;
end;

{ TSection }

constructor TSection.Create(const aParent: TSection);
begin
  FParent := aParent;
  if aParent <> nil then CreatedBy := Root.FCreatedBy;

  FChildSections := TSectionDictionary.Create;
end;

destructor TSection.Destroy;
begin
  FChildSections.Free;
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
      if Assigned(v.ClearArrayValues) then Result := Result + sep +  '"' + SectionAddPrefix + v.SectionName +'"' +  sep + '"' + SectionReplacePrefix + v.SectionName +'"';
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

procedure TSection.LoadedState(const State: TArrayOverrideBehavior);
begin
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

function TSection.Root: TSection;
begin
  Result := Self;
  While Result.Parent <> nil do Result := Result.Parent;
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

class procedure TSection.GetFlowArray(const s: string; const ArrActions: TListOfActions; const CallAction: TAction; const ErrorInfo: TErrorInfo);
var
  Act: TAction;
  varr: TArray<string>;
  Content: string;
  BracketDepth: Integer;
  StartPos, i: Integer;
begin
  if not s.StartsWith('[') and not s.EndsWith(']') then raise Exception.Create('"' + s + '" is not a valid array. It must be between square brackets, like [value1, value2]. ' + ErrorInfo.ToString);
  Content := s.Substring(1, s.Length - 2);

  // Split by commas, but only at top level (not inside nested brackets)
  varr := [];
  BracketDepth := 0;
  StartPos := 1;
  for i := 1 to Length(Content) do
  begin
    if Content[i] = '[' then
      Inc(BracketDepth)
    else if Content[i] = ']' then
      Dec(BracketDepth)
    else if (Content[i] = ',') and (BracketDepth = 0) then
    begin
      varr := varr + [Copy(Content, StartPos, i - StartPos)];
      StartPos := i + 1;
    end;
  end;
  // Add the last element
  if StartPos <= Length(Content) then
    varr := varr + [Copy(Content, StartPos, Length(Content) - StartPos + 1)];

  for var v0 in varr do
  begin
    var v := BBYamlUnescapeString(v0.Trim);
    if ArrActions <> nil then
    begin
      if not ArrActions.TryGetValue(v, Act) then raise Exception.Create('The value "' + v +'" in the array ' + s + ' is not a valid value. It must be one of [' + GetActions(ArrActions) + ']. ' + ErrorInfo.ToString );
      Act(v, ErrorInfo);
    end else CallAction(v, ErrorInfo);
  end;
end;

function TSection.GetBool(const s: string;
  const ErrorInfo: TErrorInfo): boolean;
begin
 if IsBoolTrue(s) then exit(true);
 if IsBoolFalse(s) then exit(false);

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
    var ChildAction := ChildSectionAction(Line, ErrorInfo, KeepValues);
    if ChildAction <> nil then
    begin
      exit(ChildAction);
    end;

  end;
  if not ChildSections.TryGetValue(Line, Result, ErrorInfo, KeepValues) then
  begin
    raise Exception.Create('"' + Line +
      '" is an invalid child section for "' + FullSectionName + '". It must be one of: ['
      + ListSectionsAndActions + ']. '+ ErrorInfo.ToString);
  end;
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

function TArrayOverrideBehavior_FromString(const value: string): TArrayOverrideBehavior;
begin
  if SameText(value, 'none') then exit(TArrayOverrideBehavior.None);
  if SameText(value, 'add') then exit(TArrayOverrideBehavior.Add);
  if SameText(value, 'replace') then exit(TArrayOverrideBehavior.Replace);

  raise Exception.Create('Invalid value for Array behavior. Must be "none", "add" or "replace".');
end;

function TArrayOverrideBehavior_ToStringPrefix(const value: TArrayOverrideBehavior): string;
begin
  case value of
    TArrayOverrideBehavior.None: exit('');
    TArrayOverrideBehavior.Add: exit(SectionAddPrefix);
    TArrayOverrideBehavior.Replace: exit(SectionReplacePrefix);
  end;
  raise Exception.Create('Invalid value for TArrayOverrideBehavior.');
end;


end.

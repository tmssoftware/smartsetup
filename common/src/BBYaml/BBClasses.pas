unit BBClasses;
{$i ../tmscommon.inc}

interface
uses Classes, SysUtils, Generics.Collections, BBArrays, BBStrings, Character, BBError;

const
  SectionAddPrefix = 'add ';
  SectionReplacePrefix = 'replace ';

  function TArrayOverrideBehavior_FromString(const value: string): TArrayOverrideBehavior;
  function TArrayOverrideBehavior_ToStringPrefix(const value: TArrayOverrideBehavior): string;

type
TSection = class;

TSectionDictionary = class
private
  FData: TObjectDictionary<string, TSection>;
public
  constructor Create;
  destructor Destroy; override;
  function Values: TEnumerable<TSection>;
  function TryGetValue(const name: string; out Section: TSection; const ErrorInfo: TErrorInfo): boolean;
  procedure Add(const aKey: string; const aValue: TSection);
  function Count: integer;
  function Contains(const name: string): boolean;
  procedure Clear;
end;

TAction = reference to procedure(Value: string; ErrorInfo: TErrorInfo);
TActionNameValue = reference to procedure(Name, Value: string; ErrorInfo: TErrorInfo);
TChildSectionAction = reference to function(Name: string; ErrorInfo: TErrorInfo): TSection;

TListOfActions = class
private
  Actions: TDictionary<string, TActionNameValue>;
  GenericAction: TActionNameValue;
  Duplicates: THashSet<string>;
  SectionName: string;
public
  constructor Create; overload;
  constructor Create(const aSectionName: string; const aAllowDuplicates: boolean); overload;
  constructor Create(const aSectionName: string; aGenericAction: TActionNameValue; const aAllowDuplicates: boolean); overload;
  destructor Destroy; override;

  function Keys: TEnumerable<string>;
  function TryGetValue(const Key: string; var Value: TActionNameValue; const ErrorInfo: TErrorInfo): Boolean;
  function ContainsKey(const Key: string): Boolean;

  procedure Add(const Name: string; const Action: TAction); overload;
  procedure Add(const Name: string; const Action: TActionNameValue); overload;

  procedure ResetDuplicates;

end;

TSectionValueTypes = (Values, NoValues, Both);

TSection = class
private
  FParent: TSection;
  FCreatedBy: string;

  function ListSectionsAndActions: string;

public
  function FullPath: string;
  function GotoChild(const Line: string; const ErrorInfo: TErrorInfo): TSection;
  function GotoParent: TSection;
  class function RemoveDoubleSpaces(const s: string): string;

  function GetBool(const s: string; const ErrorInfo: TErrorInfo): boolean;
  function GetBoolEx(const s: string; const ErrorInfo: TErrorInfo): boolean;
  function GetInt(const s: string; const ErrorInfo: TErrorInfo): integer;

  property CreatedBy: string read FCreatedBy write FCreatedBy;

strict protected
  ClearArrayValues: TProc;  //allows to clear an array before adding new values.

public
  SectionValueTypes: TSectionValueTypes; //Only needed to set in data sections.

  // Go to a child section
  ChildSections: TSectionDictionary;
  ChildSectionAction: TChildSectionAction; //if defined, ChildSections is not used.

  // Name/value properties to set in the section.
  Actions: TListOfActions;

  ContainsArrays: Boolean;
  ArraysCanBeKeys: boolean; //For backwards compat. A key can't be repeated in yaml, but we allowed it sometimes. When set to true, we will allow both "- value:" and "value:" values. This property is *only* for wrong existing data. Don't use it for new data.


  procedure ThrowInvalidTag(const Name: string; const ErrorInfo: TErrorInfo);

  function ExtraInfo: string; virtual;

  procedure LoadedState(const State: TArrayOverrideBehavior); virtual;

  property Parent: TSection read FParent;
  function Root: TSection;

  class function GetActions(const Act: TListOfActions): string;

  procedure ClearValues;
  function SupportsAddReplace: boolean;

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

procedure TSectionDictionary.Clear;
begin
  FData.Clear;
end;

function TSectionDictionary.Contains(const name: string): boolean;
begin
  Result := FData.ContainsKey(name);
end;

function TSectionDictionary.Count: integer;
begin
  Result := FData.Count;
end;

procedure TSectionDictionary.Add(const aKey: string; const aValue: TSection);
begin
  FData.Add(aKey, aValue);
end;



function TSectionDictionary.TryGetValue(const name: string;
  out Section: TSection; const ErrorInfo: TErrorInfo): boolean;
begin
  if FData.TryGetValue(name, Section) then
  begin
    Section.ClearValues;
    Section.LoadedState(TArrayOverrideBehavior.None);
    exit(true);
  end;

  if name.StartsWith(SectionAddPrefix, true) then
  begin
    if FData.TryGetValue(name.Substring(SectionAddPrefix.Length), Section) then
    begin
      Section.LoadedState(TArrayOverrideBehavior.Add);
      exit(Section.SupportsAddReplace);
    end;
  end;

  if name.StartsWith(SectionReplacePrefix, true) then
  begin
    if FData.TryGetValue(name.Substring(SectionReplacePrefix.Length), Section) then
    begin
      Section.ClearValues;
      Section.LoadedState(TArrayOverrideBehavior.Replace);
      exit(Section.SupportsAddReplace);
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

  ChildSections := TSectionDictionary.Create;
end;

destructor TSection.Destroy;
begin
  ChildSections.Free;
  Actions.Free;
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
      if SupportsAddReplace then Result := Result + sep +  '"' + SectionAddPrefix + v.SectionName +'"' +  sep + '"' + SectionReplacePrefix + v.SectionName +'"';
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

procedure TSection.ClearValues;
begin
  if Assigned(ClearArrayValues) then ClearArrayValues;
  if Actions <> nil then Actions.ResetDuplicates;
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

function TSection.GotoChild(const Line: string; const ErrorInfo: TErrorInfo): TSection;
begin
  if Assigned(ChildSectionAction) then
  begin
    var ChildAction := ChildSectionAction(Line, ErrorInfo);
    if ChildAction <> nil then
    begin
      exit(ChildAction);
    end;

  end;
  if not ChildSections.TryGetValue(Line, Result, ErrorInfo) then
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

function TSection.SupportsAddReplace: boolean;
begin
  Result := Assigned(ClearArrayValues);
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

{ TListOfActions }

procedure TListOfActions.Add(const Name: string; const Action: TAction);
begin
  Add(Name,
    procedure(Name, Value: string; ErrorInfo: TErrorInfo)
    begin
      Action(Value, ErrorInfo);
    end);
end;

procedure TListOfActions.Add(const Name: string; const Action: TActionNameValue);
begin
  Actions.Add(Name, Action);
end;

procedure TListOfActions.ResetDuplicates;
begin
  if Duplicates <> nil then Duplicates.Clear;
end;

function TListOfActions.ContainsKey(const Key: string): Boolean;
begin
  if Assigned(GenericAction) then exit(true);
  Result := Actions.ContainsKey(Key);
end;

constructor TListOfActions.Create;
begin
  Actions := TDictionary<string, TActionNameValue>.Create;
end;

constructor TListOfActions.Create(const aSectionName: string; const aAllowDuplicates: boolean);
begin
  Create;
  if not aAllowDuplicates then
  begin
    Duplicates := THashSet<string>.Create;
  end;
  SectionName := aSectionName;
end;

constructor TListOfActions.Create(const aSectionName: string; aGenericAction: TActionNameValue; const aAllowDuplicates: boolean);
begin
  Create(aSectionName, aAllowDuplicates);
  GenericAction := aGenericAction;
end;

destructor TListOfActions.Destroy;
begin
  Duplicates.Free;
  Actions.Free;
  inherited;
end;

function TListOfActions.Keys: TEnumerable<string>;
begin
  Result := Actions.Keys;
end;

function TListOfActions.TryGetValue(const Key: string; var Value: TActionNameValue; const ErrorInfo: TErrorInfo): Boolean;
begin
  if (Duplicates <> nil) then
  begin
    if (Duplicates.Contains(Key)) then raise Exception.Create('Duplicated item in section ' + SectionName + ': "' + Key + '" is already defined. ' + ErrorInfo.ToString);
    Duplicates.Add(Key);
  end;

  Result := Actions.TryGetValue(Key, Value);
  if not Result and (Assigned(GenericAction)) then
  begin
    Value := GenericAction;
    exit(true);
  end;
end;

end.

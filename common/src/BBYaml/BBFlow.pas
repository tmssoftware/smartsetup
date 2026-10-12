unit BBFlow;

interface
uses Classes, SysUtils, Generics.Collections, Character,
     BBError, BBStrings, BBClasses;
type
  TBBFlowParser = class
  private
    Line: string;
    Index: integer;
    IndexStart: integer;
    ErrorInfo: TErrorInfo;

    Section: TSection;
    FlowEnd: Char;

    RecursionLevel: integer;

    procedure SkipWhitespace(const ErrorMsg: string);
    function GetFlowItemStopSet(const SectionValueType: TSectionValueTypes): TSysCharSet;
    function GetFlowItemFreeString(const StopSet: TSysCharSet): string;
    function GetFlowItemQuotedString(const StopSet: TSysCharSet): string;
    class function GetEndOfFlowItem(const c: char): char; static;
    procedure Parse(const aRecursionLevel: integer);
    procedure GetFlowItem;
    function GetFlowNameOrValue(const IsValue: Boolean; out IsFlowItem: boolean): string;
    function AtEndOfFlowElement: boolean;
    procedure ParseFlowElement(const IsValue: Boolean);
    procedure ProcessName(const Name: string);
    procedure ProcessNameAndValue(const Name, Value: string);
    function IsTrailingComma: boolean;
    function CheckValue(const ValueName: string;
      const ExpectedValueType: TSectionValueTypes): string;
  public
    constructor Create(const aLine: string; const aIndex: integer; const aSection: TSection; const aErrorInfo: TErrorInfo);
    destructor Destroy; override;

    class procedure GetFlowArray(const s: string; const aSection: TSection; const ErrorInfo: TErrorInfo); static;
    class procedure GetFlowObject(const s: string; const aSection: TSection; const ErrorInfo: TErrorInfo); static;
    class function IsEmptyObject(const s: string; var Index: integer): boolean; static;
  end;

{$IFDEF DEBUG}
  procedure BBFlow_InternalTests;
{$ENDIF}

implementation
constructor TBBFlowParser.Create(const aLine: string;
  const aIndex: integer;
  const aSection: TSection;
  const aErrorInfo: TErrorInfo);
begin
  Line := aLine;
  Index := aIndex;
  IndexStart := aIndex;
  Section := aSection;
  ErrorInfo := aErrorInfo;
end;

destructor TBBFlowParser.Destroy;
begin
  inherited;
end;

procedure TBBFlowParser.ProcessName(const Name: string);
begin
 if (Section.Actions <> nil) then
 begin
   var Action: TActionNameValue;
   if Section.Actions.TryGetValue(Name, Action, ErrorInfo) then
   begin
     Action(Name, '', ErrorInfo);
     exit;
   end;
 end;

 Section := Section.GotoChild(Name, ErrorInfo);
end;

procedure TBBFlowParser.ProcessNameAndValue(const Name, Value: string);
begin
  if (Section.Actions <> nil) then
  begin
    var Action: TActionNameValue;
    if (Section.Actions.TryGetValue(Name, Action, ErrorInfo)) then
    begin
      Action(Name, Value, ErrorInfo);
      exit;
    end;
  end;
  Section.ThrowInvalidTag(Name, ErrorInfo);
end;

class function TBBFlowParser.GetEndOfFlowItem(const c: char): char;
begin
  if c = '{' then exit('}');
  if c = '[' then exit(']');
  raise Exception.Create('Internal Error: GetEndOfFlowItem must be called with a { or a [.');
end;

procedure TBBFlowParser.SkipWhitespace(const ErrorMsg: string);
begin
  while (Index < Line.Length) and (Line.Chars[Index].IsWhiteSpace) do Inc(Index);
  if (Index >= Line.Length) and (ErrorMsg <> '') then raise Exception.Create(ErrorMsg);
end;


function TBBFlowParser.GetFlowItemStopSet(const SectionValueType: TSectionValueTypes): TSysCharSet;
begin
  case SectionValueType of
    TSectionValueTypes.Values: exit([':', '=']);
    TSectionValueTypes.NoValues: exit([',', FlowEnd]);
    TSectionValueTypes.Both: exit([',', FlowEnd, ':', '=']);
  end;

  raise Exception.Create('Internal error.');
end;

function TBBFlowParser.GetFlowItemQuotedString(const StopSet: TSysCharSet): string;
begin
  var Start := Index;
  Result := BBYamlUnescapeStringToEnd(Line, Index, ErrorInfo);
  SkipWhitespace('"' + Line.Substring(Start) + '" is not a valid flow item. It must end with a "' + FlowEnd + '". ' + ErrorInfo.ToString);
  if not CharInSet(Line.Chars[Index], StopSet) then raise Exception.Create('Unterminated item at position ' + IntToStr(Index + 1) +' of string: "' + Line.Substring(Start) + '". ' + ErrorInfo.ToString);
  Inc(Index);
end;

function TBBFlowParser.GetFlowItemFreeString(const StopSet: TSysCharSet): string;
begin
  var Start := Index;
  while true do
  begin
    Inc(Index);
    if Index - 1 >= Line.Length then raise Exception.Create('Unterminated item at the end of string: "' + Line.Substring(Start) + '". ' + ErrorInfo.ToString);

    var c := Line.Chars[Index - 1];
    if CharInSet(c, StopSet) then
    begin
      exit(Line.Substring(Start, Index - 1 - Start).Trim);
    end;

  end;
end;

function TBBFlowParser.CheckValue(const ValueName: string; const ExpectedValueType: TSectionValueTypes): string;
begin
  if ValueName = '' then exit('');

  var StopSet := GetFlowItemStopSet(ExpectedValueType);
  if not CharInSet(Line.Chars[Index - 1], StopSet) then
  begin
    case ExpectedValueType of
      TSectionValueTypes.Values: raise Exception.Create('There is no value specified for the Key "' + ValueName + '". It should be specified as Key:Value or Key=Value. ' + ErrorInfo.ToString);
      TSectionValueTypes.NoValues,
      TSectionValueTypes.Both: raise Exception.Create('Internal error. We should always stop correctly for SectionValueTypes.Both or NoValues. In value "' + ValueName + '". ' + ErrorInfo.ToString);
    end;
  end;
  Result := ValueName;  
end;

function TBBFlowParser.GetFlowNameOrValue(const IsValue: Boolean; out IsFlowItem: boolean): string;
begin
  IsFlowItem := false;
  SkipWhitespace('"' + Line.Substring(IndexStart) + '" is not valid. It must end with a "' + FlowEnd + '". ' + ErrorInfo.ToString);

  var ValueType := Section.SectionValueTypes;
  if (IsValue) then ValueType := TSectionValueTypes.NoValues;

  //we need to stop if we find one of comma or flowend, even if we are reading the key for a value.
  //but we will raise an exception unless the value is empty.
  var StopSet := GetFlowItemStopSet(ValueType) + [',', FlowEnd]; //Should not affect ValueTypes of NoValues or Both.

  Result := '';
  var c := Line.Chars[Index];

  if (c = '''') or (c = '"') then exit(CheckValue(GetFlowItemQuotedString(StopSet), ValueType))
  else if ((c = '[') or (c = '{'))  then
  begin
    IsFlowItem := true;
    exit('');
  end
  else exit(CheckValue(GetFlowItemFreeString(StopSet), ValueType));
end;

function TBBFlowParser.AtEndOfFlowElement: boolean;
begin
 exit(CharInSet(Line.Chars[Index - 1], [',', FlowEnd]));
end;

procedure TBBFlowParser.ParseFlowElement(const IsValue: Boolean);
begin
    var InnerParser := TBBFlowParser.Create(Line, Index, Section, ErrorInfo);
    try
       InnerParser.Parse(RecursionLevel + 1);
       Index := InnerParser.Index;
       SkipWhitespace('"' + Line.Substring(IndexStart) + '" is not a valid flow item. It must end with a "' + FlowEnd + '". ' + ErrorInfo.ToString);
       //After a nested [] or {} (be it a name or a value) the only valid things are a comma or the end of this collection.
       if not CharInSet(Line.Chars[Index], [',', FlowEnd])
         then raise Exception.Create('Unexpected data after item at position ' + IntToStr(Index + 1) +' of string: "' + Line.Substring(IndexStart) + '". ' + ErrorInfo.ToString);
       Inc(Index);
    finally
      InnerParser.Free;
    end;
end;

procedure TBBFlowParser.GetFlowItem;
begin
  if IsTrailingComma 
    then exit;
  
  var IsFlowItem := false;
  var Name := GetFlowNameOrValue(false, IsFlowItem);
  if IsFlowItem then ParseFlowElement(false);

  if AtEndOfFlowElement then
  begin
    if IsFlowItem then exit;

    if Section.SectionValueTypes = TSectionValueTypes.Values then
    begin
      if Name = '' then
      begin
        ProcessNameAndValue('','');
        exit;
      end;      
      raise Exception.Create('Error parsing object "' + Line + '". It refers to an element that doesn''t exist. ' + ErrorInfo.ToString);
    end;
    ProcessName(Name);
    exit;
  end;

  var Value := GetFlowNameOrValue(true, IsFlowItem);
  if IsFlowItem then
  begin
    ProcessName(Name);
    ParseFlowElement(true);
    exit;
  end;
  ProcessNameAndValue(Name, Value);
end;

procedure TBBFlowParser.Parse(const aRecursionLevel: integer);
begin
  //avoid too much recursion which would blow up the stack and crash the app.
  if (aRecursionLevel > 64) then raise Exception.Create('Too many nested levels reading the string "' + Line + '". ' + ErrorInfo.ToString);
  RecursionLevel := aRecursionLevel;
  var StartSection := Section;
  FlowEnd := GetEndOfFlowItem(Line.Chars[Index]);

  //Empty [] must have 0 values, not 1. But [''] should be 1.
  if IsEmptyObject(Line, Index) then
  begin
    Section.ClearValues;
    exit;
  end;



  //Starts reading at [, ends at the char after the ] + whitespace
  Inc(Index);
  while((Index < Line.Length) and (Line.Chars[Index - 1] <> FlowEnd)) do
  begin
    Section := StartSection;
    GetFlowItem;
  end;
  SkipWhitespace('');
  if (RecursionLevel = 0) and (Index < Line.Length)
    then if (Line.Chars[Index] <> '#') or ((Index > 0) and (not Line.Chars[Index - 1].IsWhiteSpace))
      then raise Exception.Create('"' + Line + '" is not a valid object/array. It has data after the end of the array: "' + Line.Substring(Index) + '". ' + ErrorInfo.ToString);

end;


class procedure TBBFlowParser.GetFlowArray(const s: string; const aSection: TSection; const ErrorInfo: TErrorInfo);
begin
  if s.Trim = '' then
  begin
    aSection.ClearValues;
    exit;
  end;

  if not s.StartsWith('[') then
  begin
    raise Exception.Create('"' + s + '" is not a valid array. It must be between square brackets, like [value1, value2]. ' + ErrorInfo.ToString);
  end;

  var FlowParser := TBBFlowParser.Create(s, 0, aSection, ErrorInfo);
  try
    FlowParser.Parse(0);
  finally
    FlowParser.Free;
  end;
end;

class procedure TBBFlowParser.GetFlowObject(const s: string; const aSection: TSection; const ErrorInfo: TErrorInfo);
begin
  if not s.StartsWith('{') then
  begin
    raise Exception.Create('"' + s + '" is not a valid object. It must be between brackets, like {value1, value2}. ' + ErrorInfo.ToString);
  end;

  var FlowParser := TBBFlowParser.Create(s, 0, aSection, ErrorInfo);
  try
    FlowParser.Parse(0);
  finally
    FlowParser.Free;
  end;
end;


class function TBBFlowParser.IsEmptyObject(const s: string; var Index: integer): boolean;
begin
  //Starts reading at [, ends at the char after the ] + whitespace
  var idx := Index;
  while (idx < s.Length) and (s.Chars[idx].IsWhiteSpace) do inc(idx);
  if idx >= s.Length then begin Index := idx; exit(true); end;


  if not CharInSet(s.Chars[idx], ['[','{']) then exit(false);
  var Last := TBBFlowParser.GetEndOfFlowItem(s.Chars[idx]);

  for var i := idx + 1 to s.Length - 1 do
  begin
    if not s.Chars[i].IsWhiteSpace then
    begin
      Result := s.Chars[i] = Last;
      if Result then
      begin
        Index := i + 1;
        while (Index < s.Length) and (s.Chars[Index].IsWhiteSpace) do Inc(Index);
      end;
      exit;
    end;
  end;

  Result := false; //starts with [ but ends without ]
end;

function TBBFlowParser.IsTrailingComma: boolean;
begin
  if Index - 1 >= Line.Length then exit(false);
  if (Line.Chars[Index - 1] <> ',') then exit(false);

  while Index < Line.Length do
  begin
    if not Line.Chars[Index].IsWhiteSpace then
    begin
      if Line.Chars[Index] = FlowEnd then
      begin
        Inc(Index);
        exit(true);
      end;
      exit(false);
    end;
    Inc(Index);
  end;

  Result := false;
end;

{$IFDEF DEBUG}
type
  TBBFlowTestNameValue = class
     Name: string;
     Value: string;
     constructor Create(const aName, aValue: string);
  end;

{ TBBFlowTestNameValue }

constructor TBBFlowTestNameValue.Create(const aName, aValue: string);
begin
  Name := aName;
  Value := aValue;
end;


type
  TBBFlowTestSection = class(TSection)
  private
    procedure CaptureAddAction(
      Names: TObjectList<TBBFlowTestNameValue>; const Name: string);
  public
    Names: TObjectList<TBBFlowTestNameValue>;
    class function SectionNameStatic: string; override;
    constructor Create(const ExpectedNames, ExpectedValues: TArray<string>);
    destructor Destroy; override;
  end;

{ TBBFlowTestSection }
constructor TBBFlowTestSection.Create(const ExpectedNames, ExpectedValues: TArray<string>);
begin
  inherited Create(nil);
  Names := TObjectList<TBBFlowTestNameValue>.Create;
  if ExpectedValues <> nil then
    begin
    Actions := TListOfActions.Create;
    for var i := Low(ExpectedNames) to High(ExpectedNames) do
    begin
      CaptureAddAction(Names, ExpectedNames[i]);
    end;
  end;

  ChildSectionAction :=
    function(Name: string; ErrorInfo: TErrorInfo): TSection
    begin
      Names.Add(TBBFlowTestNameValue.Create(Name, ''));
      Result := Self;
    end;
end;

destructor TBBFlowTestSection.Destroy;
begin
  Names.Free;
  inherited;
end;

procedure TBBFlowTestSection.CaptureAddAction(Names: TObjectList<TBBFlowTestNameValue>; const Name: string);
begin
  Actions.Add(Name,
  procedure (Value: string; ErrorInfo: TErrorInfo)
  begin
    Names.Add(TBBFlowTestNameValue.Create(Name, Value));
  end)
end;


class function TBBFlowTestSection.SectionNameStatic: string;
begin
  Result := 'bb-flow-test';
end;


procedure BBFlow_InternalTests;

procedure TestFlowArray(const s: string; const ExpectedNames, ExpectedValues: TArray<string>; const SectionValueTypes: TSectionValueTypes; const ErrorInfo: TErrorInfo);
begin
    var TestSection := TBBFlowTestSection.Create(ExpectedNames, ExpectedValues);
    try
      TestSection.SectionValueTypes := SectionValueTypes;
      TBBFlowParser.GetFlowArray(s, TestSection, ErrorInfo);

      Assert(Length(ExpectedNames) = TestSection.Names.Count, 'Array count was different');
      for var i := 0 to High(ExpectedNames) do
      begin
        Assert(ExpectedNames[i] = TestSection.Names[i].Name, 'Names don''t match. Expected "' + ExpectedNames[i] + '" and got "' + TestSection.Names[i].Name +'"');
        if ExpectedValues = nil
          then Assert('' = TestSection.Names[i].Value, 'Values don''t match. Expected "' + '" and got "' + TestSection.Names[i].Value +'"')
          else Assert(ExpectedValues[i] = TestSection.Names[i].Value, 'Values don''t match. Expected "' + ExpectedValues[i] + '" and got "' + TestSection.Names[i].Value +'"')
      end;
    finally
      TestSection.Free;
    end;
end;

procedure TestFlowArrayErr(const s: string; const ExpectedErr: string; const SectionValueTypes: TSectionValueTypes; const ErrorInfo: TErrorInfo);
begin
  var ok := false;
  try
    TestFlowArray(s, [], [], SectionValueTypes, ErrorInfo);
  except on ex: Exception do
  begin
    ok := ex.Message.Contains(ExpectedErr);
  end;
  end;

  if not ok then raise Exception.Create('String "' + s + '" should have raised an exception.');

end;

begin
  var ErrorInfo := TErrorInfo.Create(false);
  try
    TestFlowArray('[a: b   ,     ]', ['a'],['b'], TSectionValueTypes.Values, ErrorInfo);
    TestFlowArray('[a: b,'''':'''',]', ['a', ''],['b', ''], TSectionValueTypes.Values, ErrorInfo);
    TestFlowArray('[a: b,,]', ['a', ''],['b', ''], TSectionValueTypes.Values, ErrorInfo);

    TestFlowArray('[exe,vcl]', ['exe', 'vcl'], nil, TSectionValueTypes.NoValues, ErrorInfo);
    TestFlowArray('[exe,vcl] #', ['exe', 'vcl'], nil, TSectionValueTypes.NoValues, ErrorInfo);
    TestFlowArray('[runtime, rtl, legacy]', ['runtime', 'rtl', 'legacy'], nil, TSectionValueTypes.NoValues, ErrorInfo);
    TestFlowArray('[ runtime   , "rtl", legacy ]   ', ['runtime', 'rtl', 'legacy'], nil, TSectionValueTypes.NoValues, ErrorInfo);
    TestFlowArray('["a\"", ,c,]', ['a"', '', 'c'], nil, TSectionValueTypes.NoValues, ErrorInfo);
    TestFlowArray('["a, =b,"=''3,4''''='']', ['a, =b,'], ['3,4''='], TSectionValueTypes.Values, ErrorInfo);
    TestFlowArray('[a, d=b,   c  :  cop,"o,="="4,"  ]', ['a', 'd', 'c', 'o,='], ['', 'b', 'cop','4,'], TSectionValueTypes.Both, ErrorInfo);
    TestFlowArray('[]', [], nil, TSectionValueTypes.Both, ErrorInfo);
    TestFlowArray('[      ]', [], nil, TSectionValueTypes.Both, ErrorInfo);
    TestFlowArray('[,]', [''], nil, TSectionValueTypes.Both, ErrorInfo);
    TestFlowArray('[,'''']', ['',''], nil, TSectionValueTypes.Both, ErrorInfo);
    TestFlowArray('[   ,  ]  ', [''], nil, TSectionValueTypes.Both, ErrorInfo);
    TestFlowArray('[""]', [''], [''], TSectionValueTypes.Both, ErrorInfo);
    TestFlowArray('[a: b,]', ['a'],['b'], TSectionValueTypes.Values, ErrorInfo);
    TestFlowArray('["   "]', ['   '], nil, TSectionValueTypes.Both, ErrorInfo);
    TestFlowArray('[x: [a], b: c]',  ['x', 'a', 'b'], ['', '', 'c'], TSectionValueTypes.Both, ErrorInfo);
    TestFlowArrayErr('[exe, "vcl"d]', 'Unterminated item', TSectionValueTypes.NoValues, ErrorInfo);
    TestFlowArrayErr('[exe, "vcl"]]', 'It has data after the end of the array', TSectionValueTypes.NoValues, ErrorInfo);
    TestFlowArrayErr('[exe, "vcl"]  ]', 'It has data after the end of the array', TSectionValueTypes.NoValues, ErrorInfo);
    TestFlowArrayErr('[exe, "vcl"]#', 'It has data after the end of the array', TSectionValueTypes.NoValues, ErrorInfo);

    TestFlowArray('[[a], [b] ]', ['a', 'b'], nil, TSectionValueTypes.NoValues, ErrorInfo);
    TestFlowArray('[{a: [x]}]', ['a', 'x'], nil, TSectionValueTypes.Both, ErrorInfo);
    TestFlowArrayErr('[[a]junk, b]', 'Unexpected data after item', TSectionValueTypes.NoValues, ErrorInfo);
    TestFlowArrayErr('[a: [x]junk, b]', 'Unexpected data after item', TSectionValueTypes.Both, ErrorInfo);
    TestFlowArrayErr('[{a: [x]} junk]', 'Unexpected data after item', TSectionValueTypes.Both, ErrorInfo);

  finally
    ErrorInfo.Free;
  end;
end;

{$ENDIF}
end.


unit BBYaml;
{$i ../tmscommon.inc}

// This is *not* a YAML parser. Not even near.
// When investigating the best format to store our configuration files,
// we settled in YAML: It has a nice syntax (different from xml), it can have
// comments (different from JSON), and it handles multiple levels of hierarchy (different from ini)
// But there doesn't seem to exist a YAML parser in
// pure pascal, and we don't want to add dependencies to a C library, so this can compile anywhere.
// On the other side, we don't need to support all the YAML features, we basically need to store properties in hierarchies.
// So we created this "Bare-Bones" YAML parser, which doesn't actually try to parse YAML,
// but something similar enough so you can use YAML syntax highlighting
// in text editors. It is enough for our needs, but I wouldn't use as a general YAML parser unless you can control the format.

interface
uses Classes, SysUtils, Generics.Collections, BBError, BBClasses, BBStrings, BBFlow, Character;
type

TBBYamlReader = class
  private
    class function CountSpaces(const Line: string): integer;
    class function LineIsEmpty(const Line: string): boolean; static;
  public
    class procedure ProcessStream(const Reader: TTextReader; const DisplayFileName: string; const MainSection: TSection; const aStopAt: string; const aIgnoreOtherFiles: boolean);
    class procedure ProcessFile(const FileName: string; const MainSection: TSection; const aStopAt: string; const aIgnoreOtherFiles: boolean);
end;

const
  TrimWhiteSpace: Array[0..4] of char = (#0, #32, #09, #$A0, #$FEFF);


implementation
uses Math;

{ TNotLockingStreamReader }

type
  TNotLockingStreamReader = class(TStreamReader)
  public
    constructor Create(const FileName: string);
  end;

constructor TNotLockingStreamReader.Create(const FileName: string);
begin
  var TmpStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    inherited Create(TmpStream, TEncoding.UTF8);
  except
     TmpStream.Free;
     raise;
  end;

  OwnStream;
end;

{ TFileErrorInfo }
type
TFileErrorInfo = class(TErrorInfo)
  private
    FFileName: string;
    FLineNumber: integer;
  public
  property FileName: string read FFileName write FFileName;
  property LineNumber: integer read FLineNumber write FLineNumber;

  function ToString: string; override;
end;

{ TBBYamlProcessor }
TBBYamlSectionProcessor = class
private
  ErrorInfo: TFileErrorInfo;
  Levels: TStack<integer>;
  Aborted: boolean;
  StopAt: string;

  function ChangeSection(const Section: TSection; const Line, LineWithoutComments: string; const Level: integer): TSection;

  function SectionIsContainer(const Section: TSection; const childValue: string): boolean; virtual;

  procedure ProcessValue(const Section: TSection; const Line, LineWithoutComments: string; const Level: integer);

  function SkipArray(const Name: String; const ContainsArrays: boolean): integer;
  function GetKey(const name: string; const ContainsArrays, ArraysCanBeKeys: boolean): string;
  procedure ParseColon(const Line: string; out Name, Value: string; const CanBeNameOnly: boolean; const Section: TSection; const RemoveCommentsInFlowArrays: boolean);
  function RemoveComments(const s: string; const ContainsArrays, RemoveCommentsInFlowArrays: boolean): string;
    function FindColon(const Line: string; const ContainsArrays: boolean): integer;
    function GetValuePart(const Line: string; const Index: integer): string;

public
  constructor Create(const FileName: string; const aStopAt: string; const aIgnoreOtherFiles: boolean);
  destructor Destroy; override;
  function Process(const Section: TSection; const Line: string; const Level: integer): TSection;
  procedure IncrementLineNumber;

end;

constructor TBBYamlSectionProcessor.Create(const FileName: string; const aStopAt: string; const aIgnoreOtherFiles: boolean);
begin
  ErrorInfo := TFileErrorInfo.Create(aIgnoreOtherFiles);
  ErrorInfo.FileName := FileName;
  ErrorInfo.LineNumber := 0;
  Levels := TStack<integer>.Create;
  Levels.Push(-1);
  StopAt := aStopAt;

end;

destructor TBBYamlSectionProcessor.Destroy;
begin
  ErrorInfo.Free;
  Levels.Free;
  inherited;
end;

procedure TBBYamlSectionProcessor.IncrementLineNumber;
begin
  ErrorInfo.LineNumber := ErrorInfo.LineNumber + 1;
end;

function TBBYamlSectionProcessor.Process(const Section: TSection;
         const Line: string; const Level: integer): TSection;
begin
  var LineWithoutComments := RemoveComments(Line, Section.ContainsArrays, false);
  if (Level <= Levels.Peek) or (SectionIsContainer(Section, LineWithoutComments)) then
  begin
    Result := ChangeSection(Section, Line, LineWithoutComments, Level);
    if Aborted then exit(nil);
    if (StopAt <> '') and (Result <> nil) and (Result.FullSectionName = StopAt) then
    begin
      Aborted := true;
      exit(nil);
    end;

    exit;
  end;

  ProcessValue(Section, Line, LineWithoutComments, Level);
  if Aborted then exit(nil);

  Result := Section;
end;

procedure TBBYamlSectionProcessor.ProcessValue(const Section: TSection;
  const Line, LineWithoutComments: string;
  const Level: integer);
var
  Name, Value: string;
  Action: TActionNameValue;
begin
  if Aborted then exit;

  case Section.SectionValueTypes of
    TSectionValueTypes.Values:
      ParseColon(Line, Name, Value, false, Section, true);

    TSectionValueTypes.Both:
    begin
      ParseColon(Line, Name, Value, true, Section, true);
      //If the section doesn't have a colon Value will be ' '.
     // if (Value = '') then raise Exception.Create('Empty value for tag "' + Name + '". It must be have a value. ' + ErrorInfo.ToString);
      Value := Value.Trim(TrimWhiteSpace);
    end;

    else
    begin
      Name := GetKey(LineWithoutComments, Section.ContainsArrays, Section.ArraysCanBeKeys);
      Value := '';
    end;

  end;

  if ((Section.Actions <> nil) and Section.Actions.TryGetValue(Name, Action)) then
  begin
    Action(Name, Value, ErrorInfo);
  end
  else Section.ThrowInvalidTag(Name, ErrorInfo);

  if (StopAt <> '') and (Section.FullSectionName + ':' + Name = StopAt) then Aborted := true;

end;

function TBBYamlSectionProcessor.SkipArray(const Name: string; const ContainsArrays: boolean): integer;
begin
  Result := 0;
  if Name.StartsWith('-') then
  begin
    if ContainsArrays then
    begin
      Result := 1;
      while (Result < Name.Length) and (Name.Chars[Result].IsWhiteSpace)
        do Inc(Result);
      
    end;
  end;
end;

function TBBYamlSectionProcessor.GetKey(const name: string; const ContainsArrays, ArraysCanBeKeys: boolean): string;
begin
  Result := name;
  if ContainsArrays then
  begin
    if not name.StartsWith('-') then
    begin
      if not ArraysCanBeKeys then raise Exception.Create('The name "' + name + '" is part of an array and must start with "-". ' + ErrorInfo.ToString);
    end else
    begin
      Result := name.Substring(1);
    end;
  end;

  Result := Result.Trim(TrimWhitespace);
  if (Result = '') and ContainsArrays then raise Exception.Create('The name "' + name + '" is empty. It must be in the form "- value". ' + ErrorInfo.ToString);
  Result := BBYamlUnescapeString(Result, ErrorInfo);

end;

function TBBYamlSectionProcessor.RemoveComments(const s: string; const ContainsArrays, RemoveCommentsInFlowArrays: boolean): string;
begin
  Result := s.Trim(TrimWhitespace);

  var Index := SkipArray(Result, ContainsArrays);
  if Index >= Result.Length then exit;
  var Arr := '';
  if (Index > 0) then Arr := '- ';
  var StartIndex := Index;


  if RemoveCommentsInFlowArrays and ((Result.Chars[Index] = '[') or  (Result.Chars[Index] = '{'))
    then exit(Result); //We can't remove the comment yet, it could be ['Number # 1']

  if (Result.Length > Index) and ((Result.Chars[Index] = '''') or (Result.Chars[Index] = '"')) then
  begin
    BBYamlUnescapeStringToEnd(Result, Index, ErrorInfo);
    var IndexWs := Index;
    var LastNonWs := Index - 1;
    while IndexWs < Result.Length do
    begin
      if (Result.Chars[IndexWs] = '#') and ((IndexWs <= 0) or (Result.Chars[IndexWs - 1].IsWhitespace)) then break;
      if not (Result.Chars[IndexWs].IsWhiteSpace) then LastNonWs := IndexWs;
      inc(IndexWs);
    end;

    exit(Arr + Result.Substring(StartIndex, LastNonWs - StartIndex + 1));
  end;

  while Index < Result.Length - 1 do
    begin
    if (Result.Chars[Index] = '#') and ((Index <= 0) or (Result.Chars[Index - 1].IsWhitespace))
      then exit(Result.SubString(0, Index - 1).Trim(TrimWhiteSpace));
    Inc(Index);
  end;
end;

function TBBYamlSectionProcessor.SectionIsContainer(const Section: TSection; const childValue: string): boolean;
begin
  if ((Section.ChildSections = nil) or (Section.ChildSections.Count = 0)) and (Section.ChildSectionAction = nil) then exit(false);

  if (Section.Actions = nil) then exit(true);
  var Idx := FindColon(childValue, Section.ContainsArrays);
  if Idx <= 0 then exit (false);

  if (Section.Actions <> nil) and (Section.Actions.ContainsKey(BBYamlUnescapeString(childValue.Substring(0, Idx).Trim, ErrorInfo))) then exit(false);
  if Section.ChildSections = nil then raise Exception.Create('The section "' + Section.SectionName + '" doesn''t contain arrays or ChildSections');

  Result := Section.ChildSections.Count > 0;
end;

function TBBYamlSectionProcessor.ChangeSection(const Section: TSection; const Line, LineWithoutComments: string; const Level: integer): TSection;
var
  Name, Value: string;
begin
  Result := Section;
  var LevelDecreased := Level <= Levels.Peek;
  var LastLevel := -1;
  while Level <= Levels.Peek do
  begin
    if Result = nil then raise Exception.Create('Invalid Section. ' + ErrorInfo.ToString);

    LastLevel := Levels.Pop;
    if Result.Parent = nil then
    begin
      raise Exception.Create('Invalid Section. ' + ErrorInfo.ToString);
    end;
    Result := Result.Parent;
  end;

  if (LevelDecreased) and (Level <= Lastlevel) and (Level > Levels.Peek) and (not SectionIsContainer(Result, Section.RemoveDoubleSpaces(LineWithoutComments))) then
  begin
    //We are continuing an older section.
    ProcessValue(Result, Line, LineWithoutComments, Level);
    exit;
  end;


  if (Level = Levels.Peek) then
  begin
    Levels.Pop;
    Result := Result.Parent;
  end;

  if Result = nil then raise Exception.Create('Error in YAML reader definition. Section Parent is nil.');

  Levels.Push(Level);

  ParseColon(Line, Name, Value, false, Result, false);
  var IsFlowArray := Value.StartsWith('[');
  if (Value <> '') and not IsFlowArray then raise Exception.Create('Invalid value: "' + Value + '" for tag "' + Name + '". It must be empty. ' + ErrorInfo.ToString);

  Result := Result.GotoChild(Name, ErrorInfo);
  if IsFlowArray then
  begin
    TBBFlowParser.GetFlowArray(Value, Result, ErrorInfo);
  end;

end;

function TBBYamlSectionProcessor.FindColon(const Line: string; const ContainsArrays: boolean): integer;
begin
  var Index := SkipArray(Line, ContainsArrays);
  while (Index < Line.Length) and (Line.Chars[Index].IsWhiteSpace) do Inc(Index);
  var IndexStart := Index;
  while (Index < Line.Length - 1) do
  begin
    Inc(Index);
    if (IndexStart = Index - 1) and ((Line.Chars[Index] = '''') or (Line.Chars[Index] = '"')) then
    begin
      BBYamlUnescapeStringToEnd(Line, Index, ErrorInfo);
      while (Index < Line.Length) and (Line.Chars[Index].IsWhiteSpace) do Inc(Index);

      if (Index < Line.Length) and (Line.Chars[Index] = ':') then exit(Index);
      exit(-1);
    end;


    if (Line.Chars[Index] = '#') and ((Index <= 0) or (Line.Chars[Index - 1].IsWhitespace))
      then exit(-1);
    if (Line.Chars[Index] = ':') then exit(Index);
  end;
  exit(-1);
end;

function TBBYamlSectionProcessor.GetValuePart(const Line: string; const Index: integer): string;
begin
  Result := BBYamlUnescapeString(RemoveComments(Line.Substring(Index + 1), false, false), ErrorInfo);
end;

procedure TBBYamlSectionProcessor.ParseColon(const Line: string; out Name, Value: string; const CanBeNameOnly: boolean; const Section: TSection; const RemoveCommentsInFlowArrays: boolean);
var
  idx: integer;
begin
  idx := FindColon(Line, Section.ContainsArrays);
  if CanBeNameOnly and (idx < 0) then
  begin
    Name := GetKey(TSection.RemoveDoubleSpaces(RemoveComments(Line, Section.ContainsArrays, RemoveCommentsInFlowArrays)), Section.ContainsArrays, Section.ArraysCanBeKeys).Trim(TrimWhitespace);
    Value := ' ';
    exit;
  end;

  if (idx < 0) then raise Exception.Create('The text "' + Line + '" needs a colon. ' + ErrorInfo.ToString);
  Name := GetKey(TSection.RemoveDoubleSpaces(Line.Substring(0, idx).Trim(TrimWhitespace)), Section.ContainsArrays, Section.ArraysCanBeKeys);
  Value := GetValuePart(Line, idx);
end;


{ TBBYamlReader }

class function TBBYamlReader.CountSpaces(const Line: string): integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Line.Length do
  begin
    case Line[i] of
     ' ', #$A0: inc(Result);
     #9: inc(Result, 4);
     else exit;
    end;

  end;

end;

class procedure TBBYamlReader.ProcessFile(const FileName: string;
  const MainSection: TSection; const aStopAt: string; const aIgnoreOtherFiles: boolean);
var
  Reader: TStreamReader;
begin
  Reader := TNotLockingStreamReader.Create(FileName);
  try
    ProcessStream(Reader, FileName, MainSection, aStopAt, aIgnoreOtherFiles);
  finally
    Reader.Free;
  end;
end;

class procedure TBBYamlReader.ProcessStream(const Reader: TTextReader;
  const DisplayFileName: string;
  const MainSection: TSection; const aStopAt: string; const aIgnoreOtherFiles: boolean);
var
  Line, FullLine: string;
  Section: TSection;
  Level: integer;
  SectionProcessor: TBBYamlSectionProcessor;
begin
    Section := MainSection;
    SectionProcessor := TBBYamlSectionProcessor.Create(DisplayFileName, aStopAt, aIgnoreOtherFiles);
    try
      while not Reader.EndOfStream do
      begin
        FullLine := Reader.ReadLine;
        SectionProcessor.IncrementLineNumber;
        if LineIsEmpty(FullLine) then continue;

        Line := FullLine.Trim(TrimWhitespace);

        Level := CountSpaces(FullLine);
        Section := SectionProcessor.Process(Section, Line, Level);
        if Section = nil then exit;

      end;

    finally
      SectionProcessor.Free;
    end;
end;

class function TBBYamlReader.LineIsEmpty(const Line: string): boolean;
begin
  var Trimmed := Line.Trim(TrimWhitespace);
  //If the line starts with #, it is always a comment. If not, it has to have a space before.
  // We can't know yet here if a # is a comment: it depends on the state.
  // For example in the line 'a: "b#c"' # shouldn't be taken as comment.
  // So in this method we will only remove the simple case of a line starting with #. We need to
  // remove the comments when we know what we are reading.

  Result := (Trimmed.Length = 0) or Trimmed.StartsWith('#');
end;

{ TFileErrorInfo }

function TFileErrorInfo.ToString: string;
begin
  Result := 'In line ' + IntToStr(LineNumber) + ' of file "' + FileName + '"';
end;


end.

unit BBYaml.Writer;
{$i ../tmscommon.inc}
interface
uses Classes, SysUtils, JSON, Generics.Defaults, Generics.Collections, BBYaml.Types;
type
  TBBYamlWriter = class;
  TNameAndComment = record
  public
    Name: string;
    Comment: string;

    constructor Create(const aName, aComment: string);
  end;


  TMemberAction = function(const Sender: TBBYamlWriter; const FullName: string; const ArrayIndex: integer): TYamlValue of object;
  TCommentAction = function(const Sender: TBBYamlWriter; const  FullName :string; const Comment: string): string of object;
  TGetPatternMembersAction = function(const Sender: TBBYamlWriter; const Id:string; const ArrayIndex: integer): TArray<TNameAndComment> of object;
  TIsAddReplacePrefixedPropertyAction = function(const FullName: string): boolean of object;
  TGetAddReplacePrefixAction = function(const FullName: string): string of object;

  TBBYamlWriterState = class
  public
    Name: string;
    DisplayName: string;
    Comment: string;
    Indent: string;
    IsArray: boolean;
    IsFirstProperty: boolean;

  public
    NameWritten: boolean;
    constructor Create(const aName, aDisplayName, aComment, aIndent: string; const aIsArray: boolean);
  end;

  TBBYamlWriterStack = class
  private
    FList: TObjectList<TBBYamlWriterState>;
    function PreviousIndent(const i: integer): string;
  public
    constructor Create;
    destructor Destroy; override;

    function Indent: string;
    function FullName: string;
    function GetFullName(const aName: string): string;
    procedure WriteAll(const NameWrite: TProc<string, string, string, string>);

    procedure Push(const aName, aDisplayName: string; const aComment: string; const aIndent: string; const aIsArray: boolean);
    procedure Pop;

    procedure ResetArrIndent;
    procedure UsedArrIndent;
    function ArrIndent: string;
  end;

  TWritingFormat = (Minimal, NoComments, Full);

  TBBYamlWriter = class
  private
  const
    Whitespace: Array of char = [#9, #13, ' '];
    SingleIndent = '  ';

    procedure WriteLineRaw(const s: string);
    procedure WritePair(const IdName, IdComment, IdValue: string);
    procedure WriteComment(const Indent: string; const Comment: string; const Separate: boolean);
    class function Escape(const s: string): string;
    class function DoubleQuote(const s: string): string;
    class function SingleQuote(const s: string): string;
    procedure WriteArray(const name, comment: string; const value: TYamlValue; const Schema: TJSONObject);
    class function YamlToString(const WritingFormat: TWritingFormat; const jValue: TYamlValue; const IsArray: boolean;
      out IsValidValue: boolean): string;
    procedure WriteObject(const Schema: TJSONObject; const ObjectDef: TYamlValue; const ArrayIndex: integer);
    procedure WriteOneProperty(const Name, Comment: string; const Schema: TJSONObject; const ArrayIndex: integer);
    function AddExamples(const jValue: TYamlValue;
      const Schema: TJSONObject): TYamlValue;
    procedure WritePendingItems;
    function IsAddReplaceOverload(const Name: string): boolean;
    function GetValue<T>(const jVal: TJSONValue; const MemberName: string;
      const DefaultValue: T; const RecursionLevel: integer = 0): T;
    function GetJSONPath(const s: string): string;
    function GetAddReplaceOverload(const Name: string): string;
    function GetSingleJSONObject(const Obj: TJSONObject): string;
    function IsEmpty(const jValue: TYamlValue): boolean;
  var
    FWritingFormat: TWritingFormat;
    FOnMember: TMemberAction;
    FOnComment: TCommentAction;
    FGetPatternMembers: TGetPatternMembersAction;
    FIsAddReplacePrefixedProperty: TIsAddReplacePrefixedPropertyAction;
    FGetAddReplacePrefix: TGetAddReplacePrefixAction;
    Writer: TTextWriter;
    Stack: TBBYamlWriterStack;
    FullSchema: TJSONObject;
  public
    constructor Create(const aWritingFormat: TWritingFormat);
    destructor Destroy; override;

    property WritingFormat: TWritingFormat read FWritingFormat write FWritingFormat;

    property OnMember: TMemberAction read FOnMember write FOnMember;
    property OnComment: TCommentAction read FOnComment write FOnComment;
    property GetPatternMembers: TGetPatternMembersAction read FGetPatternMembers write FGetPatternMembers;
    property IsAddReplacePrefixedProperty: TIsAddReplacePrefixedPropertyAction read FIsAddReplacePrefixedProperty write FIsAddReplacePrefixedProperty;
    property GetAddReplacePrefix: TGetAddReplacePrefixAction read FGetAddReplacePrefix write FGetAddReplacePrefix;

    class function GetFlowArray(const value: TYamlValue): string; static;

    procedure Save(const aWriter: TTextWriter; const Schema: TJSONObject; const SchemaURL, HeaderComment: string);
  end;
implementation
uses BBClasses;

{ TBBYamlWriter }

constructor TBBYamlWriter.Create(const aWritingFormat: TWritingFormat);
begin
  FWritingFormat := aWritingFormat;
  Stack := TBBYamlWriterStack.Create;
end;

destructor TBBYamlWriter.Destroy;
begin
  Stack.Free;
  inherited;
end;

class function TBBYamlWriter.SingleQuote(const s: string): string;
begin
  Result := '''' + s.Replace('''', '''''', [TReplaceFlag.rfReplaceAll]) + '''';
end;

class function TBBYamlWriter.DoubleQuote(const s: string): string;
begin
  Result := '"' + s
             .Replace('\', '\\', [TReplaceFlag.rfReplaceAll])
             .Replace(#9, '\t', [TReplaceFlag.rfReplaceAll])
             .Replace(#10, '\n', [TReplaceFlag.rfReplaceAll])
             .Replace(#13, '\r', [TReplaceFlag.rfReplaceAll])
             .Replace('"', '\"', [TReplaceFlag.rfReplaceAll])
             + '"';
end;

class function TBBYamlWriter.Escape(const s: string): string;
begin
  //https://blogs.perl.org/users/tinita/2018/03/strings-in-yaml---to-quote-or-not-to-quote.html
  if s.IndexOfAny([#9, #10, #13]) >= 0 then exit(DoubleQuote(s));

  if (s.IndexOf(': ') >= 0) or (s.IndexOf(' #') >= 0) then exit(SingleQuote(s));
  if s.StartsWith('!')
  or s.StartsWith('&')
  or s.StartsWith('*')
  or s.StartsWith('- ')
  or s.StartsWith(': ')
  or s.StartsWith('? ')
  or s.StartsWith('{')
  or s.StartsWith('}')
  or s.StartsWith('[')
  or s.StartsWith(']')
  or s.StartsWith(',')
  or s.StartsWith(' ')
  or s.StartsWith('#')
  or s.StartsWith('|')
  or s.StartsWith('>')
  or s.StartsWith('@')
  or s.StartsWith('`')
  or s.StartsWith('"')
  or s.StartsWith('''')

  or s.EndsWith(' ')
  then exit(SingleQuote(s));


  exit(s);
end;

procedure TBBYamlWriter.WriteLineRaw(const s: string);
begin
   Writer.WriteLine(s);
end;

procedure TBBYamlWriter.WritePendingItems;
begin
  Stack.WriteAll(procedure (Indent, Name, FullName, Comment: string)
  begin
    var ActualComment := Comment;
    if Assigned(OnComment) then ActualComment := OnComment(Self, FullName, Comment);
    if ((WritingFormat = TWritingFormat.Full) or (Indent = ''))and (ActualComment <> '') then WriteComment(Indent, ActualComment, true);
    if Name <> '' then WriteLineRaw(Indent + Name + ':');
  end);

end;

procedure TBBYamlWriter.WritePair(const IdName, IdComment, IdValue: string);
begin
  WritePendingItems;

  var IdValueEx := IdValue;
  if IdValueEx <> '' then IdValueEx := ' ' + IdValueEx;

  var ActualComment := IdComment;
  if Assigned(OnComment) then ActualComment := OnComment(Self, Stack.GetFullName(IdName), IdComment);
  if(WritingFormat = TWritingFormat.Full) and (ActualComment <> '') then WriteComment(Stack.Indent, ActualComment, true);

  WriteLineRaw(Stack.Indent + Stack.ArrIndent + IdName + ':' + IdValueEx);
  Stack.UsedArrIndent;
end;

procedure TBBYamlWriter.WriteComment(const Indent: string; const Comment: string; const Separate: boolean);
begin
  if Separate then WriteLineRaw('');
  var Lines := Comment.Split([#10]);
  for var Line in Lines do
  begin
    WriteLineRaw(Indent + '# ' + Line.Trim(Whitespace));
  end;

end;

class function TBBYamlWriter.YamlToString(const WritingFormat: TWritingFormat; const jValue: TYamlValue; const IsArray: boolean; out IsValidValue: boolean): string;
begin
  var Prefix := '';
  if IsArray then Prefix := '- ';

  IsValidValue := true;
  case jValue.ValueType of
    TYamlValueType.Boolean: if jValue.AsBoolean then exit(Prefix + 'true') else exit(Prefix + 'false');
    TYamlValueType.String: exit(Prefix + Escape(jValue.AsString));
    TYamlValueType.Integer: exit(Prefix + IntToStr(jValue.AsInteger));
    TYamlValueType.Float: exit(Prefix + FloatToStr(jValue.AsFloat, TFormatSettings.Invariant));
    TYamlValueType.Empty: if (jValue.EmptyComment <> '') and (WritingFormat = TWritingFormat.Full) then exit ('#' + Prefix + jValue.EmptyComment) else exit('');
  end;

  IsValidValue := false;
end;


class function TBBYamlWriter.GetFlowArray(const value: TYamlValue): string;
begin
  Result := '';
  for var i := 0 to value.ArrayCount - 1 do
  begin
    var Item := value.GetArrayItem(i);
    if Item.ValueType = TYamlValueType.Empty then continue;

    if Length(Result) > 0 then Result := Result + ',';
    var IsValid: boolean;
    var s := YamlToString(TWritingFormat.Minimal, Item, false, IsValid);
    if not IsValid then raise Exception.Create('Invalid value for flow array.');

    Result := Result + s;
  end;
  Result := '[' + Result + ']';
end;

procedure TBBYamlWriter.WriteArray(const name, comment: string; const value: TYamlValue; const Schema: TJSONObject);
begin
  if value.IsFlowArray then
  begin
    WritePair(name, comment, GetFlowArray(value));
    exit;
  end;

  Stack.Push(name, GetAddReplaceOverload(name), comment, Stack.Indent + SingleIndent, true);
  if (WritingFormat <> TWritingFormat.Minimal) then WritePendingItems;

  for var i := 0 to value.ArrayCount - 1 do
  begin
    Stack.ResetArrIndent;
    var ArrayMember := value.GetArrayItem(i);
    var IsSimple: boolean;
    var v := YamlToString(WritingFormat, ArrayMember, true, IsSimple);
    if IsSimple then
    begin
      if (v <> '') then
      begin
        WritePendingItems;
        WriteLineRaw(Stack.Indent + v);
      end;
      continue;
    end;
    if ArrayMember.ValueType = TYamlValueType.Array then
    begin
      raise Exception.Create('Array of Array is not supported. Use an Array of Object instead.');

    end;
    if ArrayMember.ValueType = TYamlValueType.Object then
    begin
      var ItemSchema: TJSONObject := nil;
      if Schema <> nil then ItemSchema := GetValue<TJSONObject>(Schema, 'items', nil);

      WriteObject(ItemSchema, ArrayMember, i);
      continue;
    end;
    if (ArrayMember.ValueType = TYamlValueType.Null) then continue;

    raise Exception.Create('Unexpected value for "' + Stack.GetFullName(name) + '"');

  end;
  Stack.Pop;
end;

function TBBYamlWriter.GetSingleJSONObject(const Obj: TJSONObject): string;
begin
  if Obj.Count <> 1 then exit('Unsupported object');
  Result := Obj.Pairs[0].JsonString.GetValue<string> + ': ' +  Obj.Pairs[0].JsonValue.GetValue<string>;
end;

function TBBYamlWriter.AddExamples(const jValue: TYamlValue; const Schema: TJSONObject): TYamlValue;
begin
  Result := jValue;
  if Schema = nil then exit;

  var Examples := GetValue<TJSONArray>(Schema, 'examples', nil);
  if not Assigned(Examples) or not (Examples is TJSONArray) then exit;

  var ArrExamples := TJSONArray(Examples);
  if ArrExamples.Count < 1 then exit;


  if (jValue.ValueType = TYamlValueType.String) and (jValue.AsString = '') then exit(TYamlValue.MakeEmpty(ArrExamples.Items[0].GetValue<string>));

  if (jValue.ValueType = TYamlValueType.Array) and (jValue.ArrayCount = 0) then
  begin
    exit(TYamlValue.MakeArray(ArrExamples.Count,
      function(i: integer): TYamlValue
      begin
        //only single objects or strings supported.
        if ArrExamples[i] is TJSONObject then exit(TYamlValue.MakeEmpty(GetSingleJSONObject(ArrExamples[i] as TJSONObject)));
        if ArrExamples[i] is TJSONString then exit(TYamlValue.MakeEmpty(ArrExamples[i].GetValue<string>));
        Result := TYamlValue.MakeEmpty('example not supported');

      end,
      false));
  end;

end;

function TBBYamlWriter.IsEmpty(const jValue: TYamlValue): boolean;
begin
  if (jValue.ValueType = TYamlValueType.Empty) then exit(true);
  if (jValue.ValueType = TYamlValueType.String) and (jValue.AsString = '') then exit(true);
  Result := false;
end;

procedure TBBYamlWriter.WriteOneProperty(const Name, Comment: string; const Schema: TJSONObject; const ArrayIndex: integer);
begin
    var FullName := Stack.GetFullName(Name);

    var jValue := OnMember(Self, FullName, ArrayIndex);
    var IsValue: boolean;

    jValue := AddExamples(jValue, Schema);
    var s := YamlToString(WritingFormat, jValue, false, IsValue);
    if (WritingFormat = TWritingFormat.Minimal) and IsEmpty(jValue) then
    begin
    end
    else
    if IsValue then WritePair(GetAddReplaceOverload(Name), Comment, s)
    else if jValue.ValueType = TYamlValueType.Array then WriteArray(Name, Comment, jValue, Schema)
    else if jValue.ValueType = TYamlValueType.Object then
    begin
      Stack.Push(name, GetAddReplaceOverload(name), comment, Stack.Indent + SingleIndent, false);
      WriteObject(Schema, jValue, -1);
      Stack.Pop;
    end
    else if (jValue.ValueType = TYamlValueType.Null) then
    begin
    end
    else raise Exception.Create('Unexpected value for "' + FullName + '"');

end;

function TBBYamlWriter.IsAddReplaceOverload(const Name: string): boolean;
begin
  if not Assigned(IsAddReplacePrefixedProperty) then exit(false);

  if Name.StartsWith(SectionReplacePrefix) then exit(IsAddReplacePrefixedProperty(Stack.GetFullName(Name.Substring(SectionReplacePrefix.Length))));
  if Name.StartsWith(SectionAddPrefix) then exit(IsAddReplacePrefixedProperty(Stack.GetFullName(Name.Substring(SectionAddPrefix.Length))));
  Result := false;
end;

function TBBYamlWriter.GetAddReplaceOverload(const Name: string): string;
begin
  if not Assigned(GetAddReplacePrefix) then exit(Name);
  exit(GetAddReplacePrefix(Stack.GetFullName(Name)) + Name);
end;

function TBBYamlWriter.GetJSONPath(const s: string): string;
begin
  var Parts := s.Split(['/'], TStringSplitOptions.ExcludeEmpty);
  Result := '';
  for var Part in Parts do
  begin
    if Result <> '' then Result := Result + '.';
    Result := Result + '' + Part + '';
  end;
end;

function TBBYamlWriter.GetValue<T>(const jVal: TJSONValue; const MemberName: string; const DefaultValue: T; const RecursionLevel: integer): T;
begin
  if RecursionLevel > 30 then raise Exception.Create('Too many nested $ref in schema.');

  if jVal.TryGetValue<T>(MemberName, Result) then exit;
  var Path := '';
  if jVal.TryGetValue<string>('$ref', Path) and (Path.StartsWith('#')) then
  begin
    var LinkedType := FullSchema.GetValue<TJSONObject>(GetJSONPath(Path.Substring(1)), nil);
    if LinkedType <> nil then exit(GetValue<T>(LinkedType, MemberName, DefaultValue, RecursionLevel + 1));
  end;

  Result := DefaultValue;
end;

procedure TBBYamlWriter.WriteObject(const Schema: TJSONObject; const ObjectDef: TYamlValue; const ArrayIndex: integer);
begin
  var InternalMembers := ObjectDef.GetObjectProperties;
  if InternalMembers <> nil then
  begin
    for var Member in InternalMembers do
    begin
      WriteOneProperty(Member, '', nil, ArrayIndex);
    end;
    exit;
  end;
  if Schema = nil then exit;

  var Members := GetValue<TJSONObject>(Schema, 'properties', nil);
  if Members <> nil then
  begin
    for var Member in Members do
    begin
      var Name := Member.JsonString.GetValue<string>;
      if IsAddReplaceOverload(Name) then continue;
      var jMemberDef := Member.JsonValue as TJSONObject;
      var Comment := GetValue(jMemberDef, 'description','');

      WriteOneProperty(Name, Comment, jMemberDef, ArrayIndex);
    end;
  end;

  var PatternMembers := GetValue<TJSONObject>(Schema, 'patternProperties', nil);
  if PatternMembers <> nil then
  begin
    for var PatternMember in PatternMembers do
    begin
      var PatternKeys := GetPatternMembers(Self, Stack.GetFullName(PatternMember.JsonString.GetValue<string>), ArrayIndex);
      for var PatternKey in PatternKeys do
      begin
        WriteOneProperty(PatternKey.Name, PatternKey.Comment, PatternMember.JsonValue as TJSONObject, ArrayIndex);
      end;
    end;
  end;
end;

function MaxLength(const s: string): integer;
begin
  Result := 5;
  var Items := s.Split([#10]);
  for var Item in Items do
  begin
    var Len := Length(Item.Trim(TBBYamlWriter.Whitespace));
    if Len > Result then Result := Len;
    
  end;

end;

procedure TBBYamlWriter.Save(const aWriter: TTextWriter;
  const Schema: TJSONObject; const SchemaURL, HeaderComment: string);
begin
  Writer := aWriter;
  FullSchema := Schema;
  var SchemaMeta :=  '# yaml-language-server: $schema=' + SchemaUrl;
  var Line := '# ' + StringOfChar('=', MaxLength(HeaderComment)) + '==';
  WriteLineRaw(Line);
  WriteComment('', HeaderComment, false);
  WriteLineRaw(Line);
  WriteLineRaw(SchemaMeta);
  WriteLineRaw('');

  WriteObject(Schema, TYamlValue.MakeObject, -1);


end;

{ TBBYamlWriterStack }

procedure TBBYamlWriterStack.Pop;
begin
  FList.Delete(FList.Count - 1);
end;

procedure TBBYamlWriterStack.Push(const aName, aDisplayName, aComment, aIndent: string; const aIsArray: boolean);
begin
  FList.Add(TBBYamlWriterState.Create(aName, aDisplayName, aComment, aIndent, aIsArray));
end;

function TBBYamlWriterStack.ArrIndent: string;
begin
  if FList.Count = 0 then exit('');
  
  if not FList.Last.IsArray then exit('');
  if FList.Last.IsFirstProperty then exit('- ') else exit('  ');
end;

procedure TBBYamlWriterStack.ResetArrIndent;
begin
  FList.Last.IsFirstProperty := true;
end;

procedure TBBYamlWriterStack.UsedArrIndent;
begin
  FList.Last.IsFirstProperty := false;
end;

constructor TBBYamlWriterStack.Create;
begin
  FList := TObjectList<TBBYamlWriterState>.Create;
end;

destructor TBBYamlWriterStack.Destroy;
begin
  FList.Free;
  inherited;
end;

function TBBYamlWriterStack.Indent: string;
begin
  if FList.Count = 0 then exit('');
  Result := FList.Last.Indent;
end;

function TBBYamlWriterStack.FullName: string;
begin
  Result := '';
  for var i := 0 to FList.Count - 1 do
  begin
    Result := Result + FList[i].Name + ':';
  end;
end;

function TBBYamlWriterStack.GetFullName(const aName: string): string;
begin
  Result := FullName + aName + ':';
end;

function TBBYamlWriterStack.PreviousIndent(const i: integer): string;
begin
  if (i < 1) then exit('');
  exit(FList[i - 1].Indent);
end;

procedure TBBYamlWriterStack.WriteAll(const NameWrite: TProc<string, string, string, string>);
begin
  var FullName := '';
  for var i := 0 to FList.Count - 1 do
  begin
    if (FList[i].Name <> '') then FullName := FullName + FList[i].Name + ':';
    if (FList[i].NameWritten) then continue;
    NameWrite(PreviousIndent(i), FList[i].DisplayName, FullName, FList[i].Comment);
    FList[i].NameWritten := true;
  end;

end;

{ TBBYamlWriterState }

constructor TBBYamlWriterState.Create(const aName, aDisplayName, aComment, aIndent: string; const aIsArray: boolean);
begin
  Name := aName;
  DisplayName := aDisplayName;
  Comment := aComment;
  Indent := aIndent;
  IsArray := aIsArray;
end;

{ TNameAndComment }

constructor TNameAndComment.Create(const aName, aComment: string);
begin
  Name := aName;
  Comment := aComment;
end;

end.

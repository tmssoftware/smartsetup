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

  TBBYamlWriterState = class
  public
    Name: string;
    Comment: string;
    Indent: string;
    IsArray: boolean;
    IsFirstProperty: boolean;

  public
    NameWritten: boolean;
    constructor Create(const aName, aComment, aIndent: string; const aIsArray: boolean);
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

    procedure Push(const aName: string; const aComment: string; const aIndent: string; const aIsArray: boolean);
    procedure Pop;

    procedure ResetArrIndent;
    procedure UsedArrIndent;
    function ArrIndent: string;
  end;


  TBBYamlWriter = class
  private
  const
    Whitespace: Array of char = [#9, #13, ' '];
    SingleIndent = '  ';

    procedure WriteLineRaw(const s: string);
    procedure WritePair(const IdName, IdComment, IdValue: string);
    procedure WriteComment(const Indent: string; const Comment: string; const Separate: boolean);
    function Escape(const s: string): string;
    function DoubleQuote(const s: string): string;
    function SingleQuote(const s: string): string;
    function GetFlowArray(const value: TYamlValue): string;
    procedure WriteArray(const name, comment: string; const value: TYamlValue; const Schema: TJSONObject);
    function YamlToString(const jValue: TYamlValue; const IsArray: boolean;
      out IsValidValue: boolean): string;
    procedure WriteObject(const Schema: TJSONObject; const ObjectDef: TYamlValue; const ArrayIndex: integer);
    procedure WriteOneProperty(const Name, Comment: string; const Schema: TJSONObject; const ArrayIndex: integer);
    function AddExamples(const jValue: TYamlValue;
      const Schema: TJSONObject): TYamlValue;
    procedure WritePendingItems;
  var
    InvariantCulture : TFormatSettings;
    FWriteComments: boolean;
    FOuputJSON: boolean;
    FOutputJSON: boolean;
    FOnMember: TMemberAction;
    FOnComment: TCommentAction;
    FGetPatternMembers: TGetPatternMembersAction;
    Writer: TTextWriter;
    Stack: TBBYamlWriterStack;
  public
    constructor Create(const aWriteComments, aOutputJSON: boolean);
    destructor Destroy; override;

    property WriteComments: boolean read FWriteComments write FWriteComments;
    property OutputJSON: boolean read FOutputJSON;

    property OnMember: TMemberAction read FOnMember write FOnMember;
    property OnComment: TCommentAction read FOnComment write FOnComment;
    property GetPatternMembers: TGetPatternMembersAction read FGetPatternMembers write FGetPatternMembers;

    procedure Save(const aWriter: TTextWriter; const Schema: TJSONObject; const SchemaURL, HeaderComment: string);
  end;
implementation

{ TBBYamlWriter }

constructor TBBYamlWriter.Create(const aWriteComments, aOutputJSON: boolean);
begin
  InvariantCulture := TFormatSettings.Create('en-US');
  FWriteComments := aWriteComments;
  FOuputJSON := aOutputJSON;
  Stack := TBBYamlWriterStack.Create;
end;

destructor TBBYamlWriter.Destroy;
begin
  Stack.Free;
  inherited;
end;

function TBBYamlWriter.SingleQuote(const s: string): string;
begin
  Result := '''' + s.Replace('''', '''''', [TReplaceFlag.rfReplaceAll]) + '''';
end;

function TBBYamlWriter.DoubleQuote(const s: string): string;
begin
  Result := '"' + s
             .Replace('\', '\\', [TReplaceFlag.rfReplaceAll])
             .Replace(#9, '\t', [TReplaceFlag.rfReplaceAll])
             .Replace(#10, '\n', [TReplaceFlag.rfReplaceAll])
             .Replace(#13, '\r', [TReplaceFlag.rfReplaceAll])
             .Replace('"', '\"', [TReplaceFlag.rfReplaceAll])
             + '"';
end;

function TBBYamlWriter.Escape(const s: string): string;
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
    if WriteComments and (ActualComment <> '') then WriteComment(Indent, ActualComment, true);
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
  if WriteComments and (ActualComment <> '') then WriteComment(Stack.Indent, ActualComment, true);

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

function TBBYamlWriter.YamlToString(const jValue: TYamlValue; const IsArray: boolean; out IsValidValue: boolean): string;
begin
  var Prefix := '';
  if IsArray then Prefix := '- ';

  IsValidValue := true;
  case jValue.ValueType of
    TYamlValueType.Boolean: if jValue.AsBoolean then exit(Prefix + 'true') else exit(Prefix + 'false');
    TYamlValueType.String: exit(Prefix + escape(jValue.AsString));
    TYamlValueType.Integer: exit(Prefix + IntToStr(jValue.AsInteger));
    TYamlValueType.Float: exit(Prefix + FloatToStr(jValue.AsFloat, InvariantCulture));
    TYamlValueType.Empty: if (jValue.EmptyComment <> '') and WriteComments then exit ('#' + Prefix + jValue.EmptyComment) else exit('');
  end;

  IsValidValue := false;
end;


function TBBYamlWriter.GetFlowArray(const value: TYamlValue): string;
begin
  Result := '';
  for var i := 0 to value.ArrayCount - 1 do
  begin
    var Item := value.GetArrayItem(i);
    if Item.ValueType = TYamlValueType.Empty then continue;

    if Length(Result) > 0 then Result := Result + ', ';
    var IsValid: boolean;
    var s := YamlToString(Item, false, IsValid);
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

  Stack.Push(name, comment, Stack.Indent + SingleIndent, true);
  for var i := 0 to value.ArrayCount - 1 do
  begin
    Stack.ResetArrIndent;
    var ArrayMember := value.GetArrayItem(i);
    var IsSimple: boolean;
    var v := YamlToString(ArrayMember, true, IsSimple);
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
      if Schema <> nil then ItemSchema := Schema.GetValue('items') as TJSONObject;

      WriteObject(ItemSchema, ArrayMember, i);
      continue;
    end;
    if (ArrayMember.ValueType = TYamlValueType.Null) then continue;

    raise Exception.Create('Unexpected value for "' + Stack.GetFullName(name) + '"');

  end;
  Stack.Pop;
end;

function TBBYamlWriter.AddExamples(const jValue: TYamlValue; const Schema: TJSONObject): TYamlValue;
begin
  Result := jValue;
  if Schema = nil then exit;

  var Examples := Schema.GetValue('examples');
  if not Assigned(Examples) or not (Examples is TJSONArray) then exit;

  var ArrExamples := TJSONArray(Examples);
  if ArrExamples.Count < 1 then exit;


  if (jValue.ValueType = TYamlValueType.String) and (jValue.AsString = '') then exit(TYamlValue.MakeEmpty(ArrExamples.Items[0].GetValue<string>));



end;

procedure TBBYamlWriter.WriteOneProperty(const Name, Comment: string; const Schema: TJSONObject; const ArrayIndex: integer);
begin
    var FullName := Stack.GetFullName(Name);

    var jValue := OnMember(Self, FullName, ArrayIndex);
    var IsValue: boolean;

    jValue := AddExamples(jValue, Schema);

    var s := YamlToString(jValue, false, IsValue);
    if IsValue then WritePair(Name, Comment, s)
    else if jValue.ValueType = TYamlValueType.Array then WriteArray(Name, Comment, jValue, Schema)
    else if jValue.ValueType = TYamlValueType.Object then
    begin
      Stack.Push(name, comment, Stack.Indent + SingleIndent, false);
      WriteObject(Schema, jValue, -1);
      Stack.Pop;
    end
    else if (jValue.ValueType = TYamlValueType.Null) then
    begin
    end
    else raise Exception.Create('Unexpected value for "' + FullName + '"');

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

  var Members := Schema.GetValue<TJSONObject>('properties', nil);
  if Members <> nil then
  begin
    for var Member in Members do
    begin
      var jMemberDef := Member.JsonValue as TJSONObject;
      var Name := Member.JsonString.GetValue<string>;
      var Comment := jMemberDef.GetValue<string>('description','');

      WriteOneProperty(Name, Comment, jMemberDef, ArrayIndex);
    end;
  end;

  var PatternMembers := Schema.GetValue<TJSONObject>('patternProperties', nil);
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

procedure TBBYamlWriterStack.Push(const aName, aComment, aIndent: string; const aIsArray: boolean);
begin
  FList.Add(TBBYamlWriterState.Create(aName.ToLowerInvariant, aComment, aIndent, aIsArray));
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
  Result := FullName + aName.ToLowerInvariant + ':';
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
    NameWrite(PreviousIndent(i), FList[i].Name, FullName, FList[i].Comment);
    FList[i].NameWritten := true;
  end;

end;

{ TBBYamlWriterState }

constructor TBBYamlWriterState.Create(const aName, aComment, aIndent: string; const aIsArray: boolean);
begin
  Name := aName;
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

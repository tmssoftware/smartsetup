unit BBYaml.Writer;
{$i ../tmscommon.inc}
interface
uses Classes, SysUtils, JSON, Generics.Defaults, Generics.Collections;
type
  TValueType = (&Object, &Boolean, &String, &Integer, &Float, &Array, &Union);

type
  TBBYamlWriter = class;
  TNameAndComment = record
  public
    Name: string;
    Comment: string;

    constructor Create(const aName, aComment: string);
  end;


  TMemberAction = TFunc<TBBYamlWriter, string, TJSONValue>;
  TCommentAction = TFunc<TBBYamlWriter, string, string, string>;
  TGetPatternMembersAction = TFunc<TBBYamlWriter, string, TArray<TNameAndComment>>;

  TBBYamlWriterState = class
  public
    Name: string;
    Comment: string;
    Indent: string;
  public
    NameWritten: boolean;
    constructor Create(const aName, aComment, aIndent: string);
  end;

  TBBYamlWriterStack = class
  private
    FList: TObjectList<TBBYamlWriterState>;
  public
    constructor Create;
    destructor Destroy; override;

    function Indent: string;
    function FullName: string;
    function GetFullName(const aName: string): string;
    procedure WriteAll(const NameWrite: TProc<string, string, string, string>);

    procedure Push(const aName: string; const aComment: string; const aIndent: string);
    procedure Pop;
  end;


  TBBYamlWriter = class
  private
  const
    Whitespace: Array of char = [#9, #13, ' '];
    SingleIndent = '  ';

    procedure WriteLineRaw(const s: string);
    procedure WritePair(const IdName, IdComment, IdValue: string);
    procedure WriteComment(const Indent: string; const Comment: string);
    function Escape(const s: string): string;
    function DoubleQuote(const s: string): string;
    function SingleQuote(const s: string): string;
    function GetCompactArray(const value: TJSONArray): string;
    procedure WriteArray(const name, comment: string; const value: TJSONArray; const Compact: boolean);
    function JSONToString(const jValue: TJSONValue;
      out IsValidValue: boolean): string;
    function JSONToStringOrObject(const jValue: TJSONValue): string;
    procedure WriteObject(const Schema: TJSONObject);
    procedure WriteOneProperty(const Name, Comment: string);
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

procedure TBBYamlWriter.WritePair(const IdName, IdComment, IdValue: string);
begin
  Stack.WriteAll(procedure (Indent, Name, FullName, Comment: string)
  begin
    var ActualComment := Comment;
    if Assigned(OnComment) then ActualComment := OnComment(Self, FullName, Comment);
    if WriteComments and (ActualComment <> '') then WriteComment(Indent, ActualComment);
    if Name <> '' then WriteLineRaw(Indent + Name + ':');
  end);


  var IdValueEx := IdValue;
  if IdValueEx <> '' then IdValueEx := ' ' + IdValueEx;

  var ActualComment := IdComment;
  if Assigned(OnComment) then ActualComment := OnComment(Self, Stack.FullName, IdComment);
  if WriteComments and (ActualComment <> '') then WriteComment(Stack.Indent, ActualComment);
  WriteLineRaw(Stack.Indent + IdName + ':' + IdValueEx);
end;

procedure TBBYamlWriter.WriteComment(const Indent: string; const Comment: string);
begin
  var Lines := Comment.Split([#10]);
  for var Line in Lines do
  begin
    WriteLineRaw(Indent + '# ' + Line.Trim(Whitespace));
  end;

end;

function TBBYamlWriter.JSONToString(const jValue: TJSONValue; out IsValidValue: boolean): string;
begin
  IsValidValue := true;

  if jValue is TJSONBool then
  begin
    if jValue.GetValue<boolean> then exit('true') else exit('false');
  end
  else if jValue is TJSONString then exit(Escape(jValue.GetValue<string>))
  else if jValue is TJSONNumber then exit(FloatToStr(jValue.GetValue<double>, InvariantCulture));

  IsValidValue := false;
end;

function TBBYamlWriter.JSONToStringOrObject(const jValue: TJSONValue): string;
begin
  var IsValidValue: boolean;
  var s := JSONToString(jValue, IsValidValue);
  if IsValidValue then exit(s);

  if jValue is TJSONObject then
  begin
    var jObject := jValue as TJSONObject;
    if jObject.Count = 1 then
    begin
      var so := JSONToString(jObject.Pairs[0].JsonValue, IsValidValue);
      if IsValidValue then exit(jObject.Pairs[0].JsonString.GetValue<string> + ': ' + so);
    end;
  end;
  raise Exception.Create('Invalid value for array: ' + jValue.ToJSON);

end;

function TBBYamlWriter.GetCompactArray(const value: TJSONArray): string;
begin
  Result := '';
  for var v in value do
  begin
    if Length(Result) > 0 then Result := Result + ', ';
    var IsValid: boolean;
    var s := JSONToString(v, IsValid);
    if not IsValid then raise Exception.Create('Invalid value for compact array: ' + v.ToJSON);

    Result := Result + s;
  end;
  Result := '[' + Result + ']';
end;

procedure TBBYamlWriter.WriteArray(const name, comment: string; const value: TJSONArray; const Compact: boolean);
begin
  if Compact then
  begin
    WritePair(name, comment, GetCompactArray(value));
    exit;
  end;

  WritePair(name, comment, '');
  for var v in value do
  begin
    WriteLineRaw(Stack.Indent + SingleIndent + '- ' + JSONToStringOrObject(v));
  end;
end;

procedure TBBYamlWriter.WriteOneProperty(const Name, Comment: string);
begin
    var FullName := Stack.GetFullName(Name);

    var jValue := OnMember(Self, FullName);
    var IsValue: boolean;

    var s := JSONToString(jValue, IsValue);
    if IsValue then WritePair(Name, Comment, s)
    else if jValue is TJSONArray then WriteArray(Name, Comment, jValue as TJSONArray, true)
    else if jValue is TJSONObject then
    begin
      Stack.Push(name, comment, Stack.Indent + SingleIndent);
      WriteObject(jValue as TJSONObject);
      Stack.Pop;
    end
    else raise Exception.Create('Unexpected value for "' + FullName + '": ' + jValue.ToJSON);

end;

procedure TBBYamlWriter.WriteObject(const Schema: TJSONObject);
begin
  var Members := Schema.GetValue<TJSONObject>('properties');
  for var Member in Members do
  begin
    var jMemberDef := Member.JsonValue as TJSONObject;
    var Name := Member.JsonString.GetValue<string>;
    var Comment := jMemberDef.GetValue<string>('description','');

    WriteOneProperty(Name, Comment);
  end;

  var PatternMembers := Schema.GetValue<TJSONObject>('patternProperties');
  for var PatternMember in PatternMembers do
  begin
    var PatternKeys := GetPatternMembers(Self, Stack.GetFullName(PatternMember.JsonString.GetValue<string>));
    for var PatternKey in PatternKeys do
    begin
      WriteOneProperty(PatternKey.Name, PatternKey.Comment);
    end;
  end;

end;

procedure TBBYamlWriter.Save(const aWriter: TTextWriter;
  const Schema: TJSONObject; const SchemaURL, HeaderComment: string);
begin
  Writer := aWriter;
  var SchemaMeta :=  '# yaml-language-server: $schema=' + SchemaUrl;
  var Line := '# ' + StringOfChar('=', Length(SchemaMeta) - 4) + ' #';
  WriteLineRaw(Line);
  WriteComment('', HeaderComment);
  WriteLineRaw(Line);
  WriteLineRaw(SchemaMeta);
  WriteLineRaw('');

  WriteObject(Schema);


end;

{ TBBYamlWriterStack }

procedure TBBYamlWriterStack.Pop;
begin
  FList.Delete(FList.Count - 1);
end;

procedure TBBYamlWriterStack.Push(const aName, aComment, aIndent: string);
begin
  FList.Add(TBBYamlWriterState.Create(aName, aComment, aIndent));
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
  exit (FList.Last.Indent);
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

procedure TBBYamlWriterStack.WriteAll(const NameWrite: TProc<string, string, string, string>);
begin
  var FullName := '';
  for var i := 0 to FList.Count - 1 do
  begin
    if (FList[i].Name <> '') then FullName := FullName + FList[i].Name + ':';
    if (FList[i].NameWritten) then continue;
    NameWrite(FList[i].Indent, FList[i].Name, FullName, FList[i].Comment);
    FList[i].NameWritten := true;
  end;

end;

{ TBBYamlWriterState }

constructor TBBYamlWriterState.Create(const aName, aComment, aIndent: string);
begin
  Name := aName;
  Comment := aComment;
  Indent := aIndent;
end;

{ TNameAndComment }

constructor TNameAndComment.Create(const aName, aComment: string);
begin
  Name := aName;
  Comment := aComment;
end;

end.

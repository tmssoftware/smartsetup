unit BBYaml.Writer;
{$i ../tmscommon.inc}
interface
uses Classes, SysUtils, JSON, Generics.Defaults, Generics.Collections, BBYaml.Types, BBStrings;
type
  TBBYamlWriter = class;
  TNameAndComment = record
  public
    Name: string;
    Comment: string;

    constructor Create(const aName, aComment: string);
  end;

  TYamlCollectionType = (&Object, FlowArray, BlockArray);

  TMemberAction = function(const Sender: TBBYamlWriter; const FullName: string; const ArrayIndex: integer): TYamlValue of object;
  TCommentAction = function(const Sender: TBBYamlWriter; const  FullName :string; const Comment: string): string of object;
  TGetPatternMembersAction = function(const Sender: TBBYamlWriter; const Id:string; const ArrayIndex: integer): TArray<TNameAndComment> of object;
  TIsAddReplacePrefixedPropertyAction = function(const FullName: string): boolean of object;
  TGetAddReplacePrefixAction = function(const FullName: string): string of object;
  TOpenObjectAction = procedure(const CollectionType: TYamlCollectionType; const i: integer) of object;
  TCloseObjectAction = procedure(const CollectionType: TYamlCollectionType) of object;

  TBBYamlWriterState = class
  public
    Name: string;
    DisplayName: string;
    Comment: string;
    Indent: string;
    CollectionType: TYamlCollectionType;
    IsFirstProperty: boolean;

  public
    NameWritten: boolean;
    constructor Create(const aName, aDisplayName, aComment, aIndent: string; const aCollectionType: TYamlCollectionType);
  end;

  TBBYamlWriterStack = class
  private
    FList: TObjectList<TBBYamlWriterState>;
    FFilter: string;
    OpenObject: TOpenObjectAction;
    CloseObject: TCloseObjectAction;
    FInsideFlowArray: boolean;
  public
    constructor Create(const aFilter: string; const aOpenObject: TOpenObjectAction; const aCloseObject: TCloseObjectAction);
    destructor Destroy; override;

    function Indent: string;
    function PreviousIndent: string; overload;
    function GetIndent(const i: integer): string; overload;
    function FullName: string;
    function GetFullName(const aName: string): string;
    procedure WriteAll(const NameWrite: TProc<string, string, string, string>);

    procedure Push(const aName, aDisplayName: string; const aComment: string; const aIndent: string; const aCollectionType: TYamlCollectionType);
    procedure Pop;

    procedure ResetArrIndent;
    procedure UsedArrIndent;
    function ArrIndent: string;
    function InsideFilter: boolean; overload;
    function InsideFilter(const aFullName: string): boolean; overload;

    property InsideFlowArray: boolean read FInsideFlowArray;
  end;

  TWritingFormat = (Minimal, NoComments, Full);

  TBBYamlWriter = class
  private
    PendingLineFeed: boolean;
    procedure CheckPendingLineFeed;
    procedure WriteJSONComma(var IsFirst: boolean);
    function ISJsonNull(const s: string): boolean;
  const
    Whitespace: Array of char = [#9, #13, ' '];
    SingleIndent = '  ';

    procedure WriteRaw(const s: string);
    procedure WriteLineRaw(const s: string);
    procedure WritePair(const IdName, IdComment, IdValue: string; var IsFirst: boolean);
    procedure WriteComment(const Indent: string; const Comment: string; const Separate: boolean);
    procedure WriteArray(const name, comment: string; const value: TYamlValue; const Schema: TJSONObject);
    function YamlToString(const WritingFormat: TWritingFormat; const jValue: TYamlValue; const IsArray: boolean;
      out IsValidValue: boolean): string;
    procedure WriteObject(const Schema: TJSONObject; const ObjectDef: TYamlValue; const ArrayIndex: integer);
    procedure WriteOneProperty(const Name, Comment: string; const Schema: TJSONObject; const ArrayIndex: integer; var IsFirst: boolean);
    function AddExamples(const jValue: TYamlValue;
      const Schema: TJSONObject): TYamlValue;
    procedure WritePendingItems;
    function IsAddReplaceOverload(const Name: string): boolean;
    function GetValue<T>(const jVal: TJSONValue; const MemberName: string;
      const DefaultValue: T; const RecursionLevel: integer = 0): T;
    function GetJSONPath(const s: string): string;
    function GetAddReplaceOverload(const Name: string): string;
    class function GetSingleJSONObject(const Obj: TJSONObject): string; static;
    function IsEmpty(const jValue: TYamlValue): boolean;
    procedure OpenObject(const CollectionType: TYamlCollectionType; const i: integer);
    procedure CloseObject(const CollectionType: TYamlCollectionType);
    function Quote: string;
  var
    FWritingFormat: TWritingFormat;
    ToJSON: boolean;
    FOnMember: TMemberAction;
    FOnComment: TCommentAction;
    FGetPatternMembers: TGetPatternMembersAction;
    FIsAddReplacePrefixedProperty: TIsAddReplacePrefixedPropertyAction;
    FGetAddReplacePrefix: TGetAddReplacePrefixAction;
    Writer: TTextWriter;
    Stack: TBBYamlWriterStack;
    FullSchema: TJSONObject;
  public
    constructor Create(const aWritingFormat: TWritingFormat; const aFilter: string; const aToJSON: boolean);
    destructor Destroy; override;

    property WritingFormat: TWritingFormat read FWritingFormat write FWritingFormat;

    property OnMember: TMemberAction read FOnMember write FOnMember;
    property OnComment: TCommentAction read FOnComment write FOnComment;
    property GetPatternMembers: TGetPatternMembersAction read FGetPatternMembers write FGetPatternMembers;
    property IsAddReplacePrefixedProperty: TIsAddReplacePrefixedPropertyAction read FIsAddReplacePrefixedProperty write FIsAddReplacePrefixedProperty;
    property GetAddReplacePrefix: TGetAddReplacePrefixAction read FGetAddReplacePrefix write FGetAddReplacePrefix;

    procedure Save(const aWriter: TTextWriter; const Schema: TJSONObject; const SchemaURL, HeaderComment: string);
  end;
implementation
uses BBClasses;

{ TBBYamlWriter }

constructor TBBYamlWriter.Create(const aWritingFormat: TWritingFormat; const aFilter: string; const aToJSON: boolean);
begin
  FWritingFormat := aWritingFormat;
  Stack := TBBYamlWriterStack.Create(aFilter, OpenObject, CloseObject);
  ToJSON := aToJSON;
end;

destructor TBBYamlWriter.Destroy;
begin
  Stack.Free;
  inherited;
end;

procedure TBBYamlWriter.CheckPendingLineFeed;
begin
  if PendingLineFeed then Writer.WriteLine;
  PendingLineFeed := false;
end;

procedure TBBYamlWriter.WriteRaw(const s: string);
begin
  CheckPendingLineFeed;
  if not Stack.InsideFilter then exit;

  Writer.Write(s);
end;

procedure TBBYamlWriter.WriteLineRaw(const s: string);
begin
  CheckPendingLineFeed;
  if not Stack.InsideFilter then exit;

  Writer.Write(s);
  if not Stack.InsideFlowArray then PendingLineFeed := true;
end;

procedure TBBYamlWriter.WriteJSONComma(var IsFirst: boolean);
begin
  if not Stack.InsideFilter then exit;
  if IsFirst then
  begin
    IsFirst := false;
  end else
  begin
    if ToJSON then Writer.Write(',');
  end;
end;

procedure TBBYamlWriter.WritePendingItems;
begin
  Stack.WriteAll(procedure (Indent, Name, FullName, Comment: string)
  begin
    var ActualComment := Comment;
    if Assigned(OnComment) then ActualComment := OnComment(Self, FullName, Comment);
    if ((WritingFormat = TWritingFormat.Full) or (Indent = ''))and (ActualComment <> '') then WriteComment(Indent, ActualComment, true);
    if Name <> '' then WriteLineRaw(Indent + Quote + Name + Quote + ':');
  end);

end;

function TBBYamlWriter.Quote: string;
begin
  if ToJSON then Result := '"' else Result := '';
end;

procedure TBBYamlWriter.WritePair(const IdName, IdComment, IdValue: string; var IsFirst: boolean);
begin
  WritePendingItems;

  var ValueSep := '';
  if IdValue <> '' then ValueSep := ' ';

  var ActualComment := IdComment;
  if Assigned(OnComment) then ActualComment := OnComment(Self, Stack.GetFullName(IdName), IdComment);
  if(WritingFormat = TWritingFormat.Full) and (ActualComment <> '') then WriteComment(Stack.Indent, ActualComment, true);
  WriteJSONComma(IsFirst);

  WriteRaw(Stack.Indent + Stack.ArrIndent + Quote + IdName + Quote + ':' + ValueSep);
  Stack.Push(IdName, IdName, '', '', TYamlCollectionType.Object);
  WriteLineRaw(IdValue);
  Stack.Pop;
  Stack.UsedArrIndent;
end;

procedure TBBYamlWriter.WriteComment(const Indent: string; const Comment: string; const Separate: boolean);
begin
  if ToJSON then exit;

  if Separate then WriteLineRaw('');
  var Lines := Comment.Split([#10]);
  for var Line in Lines do
  begin
    WriteLineRaw(Indent + '# ' + Line.Trim(Whitespace));
  end;

end;

procedure TBBYamlWriter.OpenObject(const CollectionType: TYamlCollectionType; const i: integer);
begin
  if ToJSON and (CollectionType = TYamlCollectionType.Object) then WriteLineRaw(Stack.GetIndent(i - 1) + '{');
  if CollectionType = TYamlCollectionType.FlowArray then
  begin
    Stack.FInsideFlowArray := true;
    PendingLineFeed := false;
    WriteRaw(' [');
  end;

end;

procedure TBBYamlWriter.CloseObject(const CollectionType: TYamlCollectionType);
begin
  var Indent := Stack.PreviousIndent;

  if ToJSON and (CollectionType = TYamlCollectionType.Object) then WriteLineRaw(Indent + '}');
  if CollectionType = TYamlCollectionType.FlowArray then
  begin
    WriteLineRaw(']');
    PendingLineFeed := true;
    Stack.FInsideFlowArray := false;
  end;
end;

function TBBYamlWriter.YamlToString(const WritingFormat: TWritingFormat; const jValue: TYamlValue; const IsArray: boolean; out IsValidValue: boolean): string;
begin
  var Prefix := '';
  if IsArray then Prefix := '- ';

  IsValidValue := true;
  case jValue.ValueType of
    TYamlValueType.Boolean: if jValue.AsBoolean then exit(Prefix + 'true') else exit(Prefix + 'false');
    TYamlValueType.String: exit(Prefix + BBYamlEscapeString(jValue.AsString, ToJSON));
    TYamlValueType.Integer: exit(Prefix + IntToStr(jValue.AsInteger));
    TYamlValueType.Float: exit(Prefix + FloatToStr(jValue.AsFloat, TFormatSettings.Invariant));
    TYamlValueType.Empty:
    begin
      if (ToJSON) then exit('null');
      if (jValue.EmptyComment <> '') and (WritingFormat = TWritingFormat.Full)
         then exit ('#' + Prefix + jValue.EmptyComment)
         else exit('');
    end;
  end;

  IsValidValue := false;
end;

function TBBYamlWriter.ISJsonNull(const s: string): boolean;
begin
  if not ToJSON then exit(false);
  Result := s = 'null';
end;

procedure TBBYamlWriter.WriteArray(const name, comment: string; const value: TYamlValue; const Schema: TJSONObject);
begin
  var Flow := ToJSON or value.IsFlowArray;
  var FlowSeparator := '';

  var ArrayType := TYamlCollectionType.BlockArray;
  if Flow then ArrayType := TYamlCollectionType.FlowArray;

  Stack.Push(name, GetAddReplaceOverload(name), comment, Stack.Indent + SingleIndent, ArrayType);
  if (WritingFormat <> TWritingFormat.Minimal) then WritePendingItems;
  for var i := 0 to value.ArrayCount - 1 do
  begin
    Stack.ResetArrIndent;
    var ArrayMember := value.GetArrayItem(i);
    var IsSimple: boolean;
    var v := YamlToString(WritingFormat, ArrayMember, not Flow, IsSimple);
    if IsSimple then
    begin
      if (v <> '') and not IsJsonNull(v) then
      begin
        WritePendingItems;
        if Flow then WriteRaw(FlowSeparator + v) else WriteLineRaw(Stack.Indent + v);
        FlowSeparator := ',';
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
      WritePendingItems;
      if Flow then
      begin
        WriteRaw(FlowSeparator);
        Stack.Push('', '', '', '', TYamlCollectionType.Object);
      end;
      WritePendingItems;
      FlowSeparator := ',';
      WriteObject(ItemSchema, ArrayMember, i);
      if Flow then
      begin
        Stack.Pop;
      end;
      continue;
    end;
    if (ArrayMember.ValueType = TYamlValueType.Null) then continue;

    raise Exception.Create('Unexpected value for "' + Stack.GetFullName(name) + '"');

  end;
  Stack.Pop;
end;

class function TBBYamlWriter.GetSingleJSONObject(const Obj: TJSONObject): string;
begin
  if Obj.Count <> 1 then exit('Unsupported object');
  Result := Obj.Pairs[0].JsonString.GetValue<string> + ': ' +  Obj.Pairs[0].JsonValue.GetValue<string>;
end;


function TBBYamlWriter.AddExamples(const jValue: TYamlValue; const Schema: TJSONObject): TYamlValue;
begin
  Result := jValue;
  if ToJSON then exit;  
  if Schema = nil then exit;
  if WritingFormat <> TWritingFormat.Full then exit;
  

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

procedure TBBYamlWriter.WriteOneProperty(const Name, Comment: string; const Schema: TJSONObject; const ArrayIndex: integer; var IsFirst: boolean);
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
    if IsValue then WritePair(GetAddReplaceOverload(Name), Comment, s, IsFirst)
    else if jValue.ValueType = TYamlValueType.Array then
    begin
      WriteJSONComma(IsFirst);
      WriteArray(Name, Comment, jValue, Schema);
    end
    else if jValue.ValueType = TYamlValueType.Object then
    begin
      WriteJSONComma(IsFirst);
      Stack.Push(name, GetAddReplaceOverload(name), comment, Stack.Indent + SingleIndent, TYamlCollectionType.Object);
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
  var IsFirst := true;
  if InternalMembers <> nil then
  begin
    for var Member in InternalMembers do
    begin
      WriteOneProperty(Member, '', nil, ArrayIndex, IsFirst);
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

      WriteOneProperty(Name, Comment, jMemberDef, ArrayIndex, IsFirst);
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
        WriteOneProperty(PatternKey.Name, PatternKey.Comment, PatternMember.JsonValue as TJSONObject, ArrayIndex, IsFirst);
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
  if not ToJSON and (SchemaURL <> '') then
  begin
    var SchemaMeta :=  '# yaml-language-server: $schema=' + SchemaUrl;
    var Line := '# ' + StringOfChar('=', MaxLength(HeaderComment)) + '==';
    WriteLineRaw(Line);
    WriteComment('', HeaderComment, false);
    WriteLineRaw(Line);
    WriteLineRaw(SchemaMeta);
    WriteLineRaw('');
  end;

  if ToJSON then Stack.Push('','','', SingleIndent, TYamlCollectionType.Object);
  WriteObject(Schema, TYamlValue.MakeObject, -1);
  if ToJSON then Stack.Pop;
  CheckPendingLineFeed;
end;


{ TBBYamlWriterStack }

procedure TBBYamlWriterStack.Pop;
begin
  var NameWritten := FList.Last.NameWritten;
  var CollectionType := Flist.Last.CollectionType;
  if NameWritten then CloseObject(CollectionType);
  FList.Delete(FList.Count - 1);
end;

procedure TBBYamlWriterStack.Push(const aName, aDisplayName, aComment, aIndent: string; const aCollectionType: TYamlCollectionType);
begin
  var ActualIndent := aIndent;
  if not InsideFilter then ActualIndent := '';

  FList.Add(TBBYamlWriterState.Create(aName, aDisplayName, aComment, ActualIndent, aCollectionType));
end;

function TBBYamlWriterStack.ArrIndent: string;
begin
  if FList.Count = 0 then exit('');
  
  if FList.Last.CollectionType <> TYamlCollectionType.BlockArray then exit('');
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

constructor TBBYamlWriterStack.Create(const aFilter: string; const aOpenObject: TOpenObjectAction; const aCloseObject: TCloseObjectAction);
begin
  FList := TObjectList<TBBYamlWriterState>.Create;
  FFilter := aFilter;
  if (FFilter <> '') and not FFilter.EndsWith(':') then FFilter := FFilter + ':';
  OpenObject := aOpenObject;
  CloseObject := aCloseObject;
end;

destructor TBBYamlWriterStack.Destroy;
begin
  FList.Free;
  inherited;
end;

function TBBYamlWriterStack.Indent: string;
begin
  Result := GetIndent(FList.Count - 1);
end;

function TBBYamlWriterStack.PreviousIndent: string;
begin
  Result := GetIndent(FList.Count - 2);
end;

function TBBYamlWriterStack.GetIndent(const i: integer): string;
begin
  if (i < 0) then exit('');
  if (InsideFlowArray) then exit('');

  exit(FList[i].Indent);
end;


function TBBYamlWriterStack.InsideFilter: boolean;
begin
  Result := FullName.StartsWith(FFilter);
end;

function TBBYamlWriterStack.InsideFilter(const aFullName: string): boolean;
begin
  Result := aFullName.StartsWith(FFilter);
end;

function TBBYamlWriterStack.FullName: string;
begin
  Result := '';
  for var i := 0 to FList.Count - 1 do
  begin
    if FList[i].Name = '' then continue;
    
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
    var OldFullName := FullName;
    if (FList[i].Name <> '') then FullName := FullName + FList[i].Name + ':';
    if (FList[i].NameWritten) then continue;
    FList[i].NameWritten := true;
    if not InsideFilter(FullName) then continue;

    if InsideFilter(OldFullName) then NameWrite(GetIndent(i - 1), FList[i].DisplayName, FullName, FList[i].Comment);
    OpenObject(FList[i].CollectionType, i);
  end;

end;

{ TBBYamlWriterState }

constructor TBBYamlWriterState.Create(const aName, aDisplayName, aComment, aIndent: string; const aCollectionType: TYamlCollectionType);
begin
  Name := aName;
  DisplayName := aDisplayName;
  Comment := aComment;
  Indent := aIndent;
  CollectionType := aCollectionType;
end;

{ TNameAndComment }

constructor TNameAndComment.Create(const aName, aComment: string);
begin
  Name := aName;
  Comment := aComment;
end;

end.

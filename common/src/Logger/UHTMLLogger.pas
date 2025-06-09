unit UHTMLLogger;
{$i ../tmscommon.inc}

interface
uses ULogger, Classes, SysUtils, UFileLogger, Generics.Collections;

type
  TMessageWriterAction = reference to procedure(const Writer: TStreamWriter; const Msg: TLogMessage; var SectionId: integer);
  IMessageWriter = interface
    procedure Run(const Writer: TStreamWriter; const Msg: TLogMessage; var SectionId: integer);
  end;

  TMessageWriter = class(TInterfacedObject, IMessageWriter)
  protected
   FWriteMessage: TMessageWriterAction;
  public
    constructor Create(const AWriteMessage: TMessageWriterAction);
    procedure Run(const Writer: TStreamWriter; const Msg: TLogMessage; var SectionId: integer); virtual; abstract;
  end;

  TFirstPassMessageWriter = class(TMessageWriter)
  private
    Stack: TStack<TLogMessage>;
    InRelevantSection: boolean;
    procedure MarkAllRed(const List: TArray<TLogMessage>; const Count: integer);
  public
    constructor Create(const AWriteMessage: TMessageWriterAction);
    destructor Destroy; override;
    procedure Run(const Writer: TStreamWriter; const Msg: TLogMessage; var SectionId: integer); override;
  end;

  TSecondPassMessageWriter = class(TMessageWriter)
  private
    InIrrelevantSection: boolean;
  public
    procedure Run(const Writer: TStreamWriter; const Msg: TLogMessage; var SectionId: integer); override;
  end;


  THTMLLogger = class(TFileLogger)
  private
    RootLogger: TLogMessageList;
    ConnectionPoints: TDictionary<string, TLogMessageList>;
    class threadvar
      HTMLLogger: TLogMessageList;
  private
    const
      LogMessageClass: Array[TVerbosity] of string = ('trace', 'info', 'error');
      LogMessageName: Array[TVerbosity] of string = ('Trace', 'Info', 'Error');

    procedure WriteHTMLStart(const Writer: TStreamWriter);
    procedure WriteHTMLEnd(const Writer: TStreamWriter);
    procedure WriteHTMLStyles(const Writer: TStreamWriter);
    procedure WriteHTMLJs(const Writer: TStreamWriter);
    procedure WriteMessage(const Writer: TStreamWriter; const Msg: TLogMessage; var SectionId: integer);
    procedure OpenMessage(const Writer: TStreamWriter; const Caption: string; var SectionId: integer; const HasErrors, IsOpen: boolean);
    procedure CloseMessage(const Writer: TStreamWriter; const Number: integer);
    procedure OpenText(const Writer: TStreamWriter);
    procedure CloseText(const Writer: TStreamWriter);
    procedure RecurseTree(const Writer: TStreamWriter; const LogMessages: TLogMessageList; var SectionId: integer; Action: IMessageWriter);
    function EscapeHTML(const s: string): string;
    function GetMessageKindClass(const MessageKind: TLogMessageKind): string;
  protected
    function Logger: TLogMessageList; override;
    procedure CheckLogger; override;
    procedure SaveLog; override;

  public
    procedure CreateConnectionPoint(const ConnectionPoint: TConnectionPoint); override;
    procedure ConnectTo(const ConnectionPoint: TConnectionPoint); override;
    function Push: TObject; override;
    procedure Pop(const State: TObject); override;

  public
    constructor Create(const aFileName: string);
    destructor Destroy; override;
    procedure StartSection(const MessageType: TMessageType; const MessageLabel: string); override;
    procedure FinishSection(const MessageType: TMessageType; const IsError: boolean); override;
  end;


implementation
uses IOUtils, Character;


{ THTMLLogger }
constructor THTMLLogger.Create(const aFileName: string);
begin
  inherited Create(aFileName);
  RootLogger := TLogMessageList.Create;
  HTMLLogger := RootLogger; //For the main thread.
  ConnectionPoints := TDictionary<string, TLogMessageList>.Create;
end;

destructor THTMLLogger.Destroy;
begin
  SaveLog;
  ConnectionPoints.Free;
  RootLogger.Free;
  inherited;
end;

procedure THTMLLogger.CreateConnectionPoint(
  const ConnectionPoint: TConnectionPoint);
begin
  //Called from main thread. Still a thread might be reading ConnectionPoints while we write to it.
  MonitorEnter(ConnectionPoints);
  try
    var Joint := TLogMessage.CreateJoint;
    HTMLLogger.Add(Joint);
    ConnectionPoints.Add(GUIDToString(ConnectionPoint.Guid), Joint.Children);
  finally
    MonitorExit(ConnectionPoints);
  end;
end;

procedure THTMLLogger.ConnectTo(const ConnectionPoint: TConnectionPoint);
begin
  //Called from child thread.
  MonitorEnter(ConnectionPoints);
  try
    HTMLLogger := ConnectionPoints[GUIDToString(ConnectionPoint.Guid)];
  finally
    MonitorExit(ConnectionPoints);
  end;
end;


function THTMLLogger.Logger: TLogMessageList;
begin
  Result := HtmlLogger;
end;

procedure THTMLLogger.CheckLogger;
begin
  // nothing here. The logger is setup in ConnectTo
end;


procedure THTMLLogger.StartSection(const MessageType: TMessageType;
  const MessageLabel: string);
begin
  HTMLLogger.Add(TLogMessage.CreateStartSection(MessageType, MessageLabel));
end;

procedure THTMLLogger.FinishSection(const MessageType: TMessageType;
  const IsError: boolean);
begin
  HTMLLogger.Add(TLogMessage.CreateEndSection(MessageType, IsError));
end;


procedure THTMLLogger.OpenMessage(const Writer: TStreamWriter; const Caption: string;
     var SectionId: integer; const HasErrors, IsOpen: boolean);
begin
  CloseText(Writer);
  Writer.WriteLine('<div class="wrap-collapsible">');
  var ErrorClass := '';
  if HasErrors then ErrorClass := ' box-error';
  var Checked := '';
  if IsOpen then Checked := 'checked';


  Writer.WriteLine('<input id="Section' + IntToStr(SectionId) + '" class="toggle" type="checkbox"' + Checked + '>');
  Writer.WriteLine('<label for="Section' + IntToStr(SectionId) + '" class="lbl-toggle' + ErrorClass+ '">' + Caption + '</label>');
  Writer.WriteLine('<div class = "collapsible-content">');
  Inc(SectionId);
  OpenText(Writer);
end;

procedure THTMLLogger.CloseMessage(const Writer: TStreamWriter; const Number: integer);
begin
  CloseText(Writer);
  for var i := 1 to number do
  begin
    Writer.WriteLine('</div>');
    Writer.WriteLine('</div>');
  end;
  OpenText(Writer);
end;

function GetDelphiVersion(const s: string): string;
begin
  var idx := s.IndexOf('_');
  if idx <= 0 then exit('');
  var idx2 := s.IndexOf('_', idx + 1);
  if idx2 <= 0 then exit('');
  Result := ' /' + s.Substring(idx + 1, idx2 - idx - 1);

end;

function GetMsbuildParameter(const command, searchStr: string): string;
begin
  var idx := command.ToLowerInvariant.IndexOf(searchStr.ToLowerInvariant);
  if idx <= 0 then exit('');

  Result := '';
  var c2 := idx + searchStr.Length;
  while (c2 < command.Length) and command.Chars[c2].IsWhitespace do inc(c2);
  if (c2 >= command.Length) or (command.Chars[c2] <> '=') then exit('');
  inc(c2);
  while (c2 < command.Length) and command.Chars[c2].IsWhitespace do inc(c2);
  if (c2 < command.Length) and (command.Chars[c2] = '"') then
  begin
    inc(c2);
    while (c2 < command.Length) and (command.Chars[c2] <> '"') do
    begin
      Result := Result + command.Chars[c2];
      inc(c2);
    end;
  end
  else
  begin
    var idx3 := (command + ' ').IndexOf(' ', c2);
    if idx3 < 0 then exit ('');
    Result := command.Substring(c2, idx3 - c2).Trim;
  end;

  if Result <> '' then Result := ' /' + Result;
end;

function ParseCommand(const aCommand: string): string;
const
  max = 60;
begin
  var command := aCommand.Trim;
  if command.Length <= max then exit(command);
  Result := command.Substring(0, max - 1) + '...';

  var exe := '';
  var idx1 := 0;
  var idx2 := command.IndexOf(' ');
  if command.StartsWith('"') then
  begin
    idx1 := 1;
    idx2 := command.IndexOf('"', 1);
  end;

  if idx2 < 0 then exit;
  try
    exe := TPath.GetFileName(command.Substring(idx1, idx2 - idx1));
  except
    exit;
  end;

  var param := '';
  var c2 := idx2;
  while (c2 < command.Length) and command.Chars[c2].IsWhitespace do inc(c2);
  if (c2 < command.Length) and (command.Chars[c2] = '"') then
  begin
    inc(c2);
    while (c2 < command.Length) and (command.Chars[c2] <> '"') do
    begin
      param := param + command.Chars[c2];
      inc(c2);
    end;
  end
  else
  begin
    var idx3 := (command + ' ').IndexOf(' ', idx2 + 1);
    if idx3 < 0 then exit (exe);
    param := command.Substring(idx2 + 1, idx3 - idx2).Trim;
  end;
  try
    var paramClean := param;
    if (exe = 'msbuild.exe') and command.ToLowerInvariant.Contains('/p:config') then
    begin
      paramClean := TPath.GetFileNameWithoutExtension(param);
      paramClean := paramClean + GetDelphiVersion(TPath.GetFileName(TPath.GetDirectoryName(param)));
      paramClean := paramClean + GetMsbuildParameter(command, '/p:Platform');
      paramClean := paramClean + GetMsbuildParameter(command, '/p:config')
    end;

    exit(exe + ' ' + paramClean);
  except
    exit(exe + param);
  end;

end;

procedure AddText(var Result: string; const NewText: string; var Position: integer);
begin
  for var i := 1 to NewText.Length do
  begin
    Result[Position] := NewText[i];
    Inc(Position);
  end;
end;


function THTMLLogger.EscapeHTML(const s: string): string;
begin
 SetLength(Result, s.Length * 6);
  var Position := 1;
  for var i := 1 to s.Length do
    case s[i] of
      '<': AddText(Result, '&lt;', Position);
      '>': AddText(Result, '&gt;', Position);
      '&': AddText(Result, '&amp;', Position);
      '"': AddText(Result, '&quot;', Position);
    else
      result[Position] := s[i];
      Inc(Position);
    end;
  SetLength(result, Position - 1);
end;

function THTMLLogger.GetMessageKindClass(const MessageKind: TLogMessageKind): string;
begin
  Result := '';
  case MessageKind of
    TLogMessageKind.Text: ;
    TLogMessageKind.Caption: exit(' msg-caption');
    TLogMessageKind.Question:  exit(' msg-question');
    TLogMessageKind.Comment:  exit(' msg-comment');
    TLogMessageKind.Conclusion:  exit(' msg-conclusion');
  end;
end;

procedure THTMLLogger.WriteMessage(const Writer: TStreamWriter; const Msg: TLogMessage; var SectionId: integer);
begin
  // Writer.WriteLine('<!-- type: ' + IntToStr(integer(Msg.MessageType)) + ' / open: ' + BoolToStr(Msg.IsSectionOpen, true) + '-->');
  case Msg.MessageType of
    TMessageType.BasicInfo:
      if Msg.SectionType = TSectionType.Open then OpenMessage(Writer, 'Information', SectionId, Msg.SectionHasErrors, true)
      else if Msg.SectionType = TSectionType.Close then CloseMessage(Writer, 1);

    TMessageType.Summary:
      if Msg.SectionType = TSectionType.Open then OpenMessage(Writer, Msg.Message, SectionId, Msg.SectionHasErrors, true)
      else if Msg.SectionType = TSectionType.Close then CloseMessage(Writer, 1);

    TMessageType.None:
    begin

    end;
    TMessageType.Command:
    begin
       if Msg.SectionType = TSectionType.Open then OpenMessage(Writer, ParseCommand(Msg.Message), SectionId, Msg.SectionHasErrors, Msg.SectionHasErrors)
      else if Msg.SectionType = TSectionType.Close then CloseMessage(Writer, 1);
    end;
    else
    begin
      if Msg.SectionType = TSectionType.Open then OpenMessage(Writer, Msg.Message, SectionId, Msg.SectionHasErrors, Msg.SectionHasErrors)
      else if Msg.SectionType = TSectionType.Close then CloseMessage(Writer, 1);
    end;
  end;

  if Msg.MessageType <> TMessageType.None then exit;


  var TagStart := '';
  var TagEnd := '';
  case Msg.MessageVerbosity of
    TVerbosity.error:
    begin
      TagStart := '<span class="error-text">';
      TagEnd := '</span>';
    end;
    TVerbosity.info:
    begin
      TagStart := '<span class="info-text' + GetMessageKindClass(Msg.LogMessageKind) + '">';
      TagEnd := '</span>';
    end;
  end;
  Writer.WriteLine(TagStart + EscapeHTML(Msg.Message) + TagEnd);

end;


procedure THTMLLogger.OpenText(const Writer: TStreamWriter);
begin
  Writer.WriteLine('<div class="preformatted">');
end;

procedure THTMLLogger.Pop(const State: TObject);
begin
  inherited;
  HTMLLogger := TLogMessageList(State);
end;

function THTMLLogger.Push: TObject;
begin
  Result := HTMLLogger;
end;

procedure THTMLLogger.CloseText(const Writer: TStreamWriter);
begin
  Writer.WriteLine('</div>');
end;

procedure THTMLLogger.RecurseTree(const Writer: TStreamWriter; const LogMessages: TLogMessageList; var SectionId: integer; Action: IMessageWriter);
begin
  for var log in LogMessages do
  begin
    if log.Children <> nil then RecurseTree(Writer, log.Children, SectionId, Action)
    else
    begin
      Action.Run(Writer, log, SectionId);
    end;
  end;

   // CloseMessage(Writer, ord(StartVerbosity(HasErrors)) - ord(OldMessageVerbosity) + 1);

end;

procedure THTMLLogger.SaveLog;
begin
  var Writer := TStreamWriter.Create(FileName, false, TEncoding.UTF8);
  try
    var SectionId := 1;
    WriteHTMLStart(Writer);
    OpenText(Writer);
    RecurseTree(Writer, RootLogger, SectionId, TFirstPassMessageWriter.Create(WriteMessage));
    RecurseTree(Writer, RootLogger, SectionId, TSecondPassMessageWriter.Create(WriteMessage));
    CloseText(Writer);
    WriteHTMLEnd(Writer);
  finally
    Writer.Free;
  end;

end;

procedure THTMLLogger.WriteHTMLStart(const Writer: TStreamWriter);
begin
  Writer.WriteLine('<!DOCTYPE html>');
  Writer.WriteLine('<html lang="en">');
  Writer.WriteLine('<head>');
  Writer.WriteLine('<meta charset="utf-8">');
  Writer.WriteLine('<title>TMS Smart Setup HTML Log</title>');
  Writer.WriteLine('<meta name="viewport" content="width=device-width">');
  WriteHTMLStyles(Writer);
  WriteHTMLJs(Writer);
  Writer.WriteLine('</head>');
  Writer.WriteLine('<body>');
  Writer.WriteLine('<div class="topbar">');
  var LogDate := '';
  if (RootLogger.Count > 0) then LogDate := FormatDateTime('mmmm d, yyyy  h:mm:ss', RootLogger[0].Time, TFormatSettings.Invariant);
  Writer.WriteLine('TMS Smart Setup Log. ' + LogDate);
  Writer.WriteLine('<button class="action-button" onclick="wrapText(false)">Wrap</button>');
  Writer.WriteLine('<button class="action-button" onclick="expandAll(false)">Collapse</button>');
  Writer.WriteLine('<button class="action-button" onclick="expandAll(true)">Expand</button>');
  Writer.WriteLine('</div>');
  Writer.WriteLine('<div class="content">');
end;

procedure THTMLLogger.WriteHTMLStyles(const Writer: TStreamWriter);
begin
  // from https://www.digitalocean.com/community/tutorials/css-collapsible
  Writer.WriteLine('<style>');

  Writer.WriteLine(':root {');
  Writer.WriteLine('  --wrap: break-spaces;');
  Writer.WriteLine('}');


  Writer.WriteLine('body {');
  Writer.WriteLine('  font-family: monospace;');
  Writer.WriteLine('}');

  Writer.WriteLine('.topbar {');
  Writer.WriteLine('padding: 10px 16px;');
  Writer.WriteLine('background: white;font-size: 12pt;font-weight: bold;');
  Writer.WriteLine('color: black;');
  Writer.WriteLine('position: fixed;');
  Writer.WriteLine('box-sizing: border-box;');
  Writer.WriteLine('top: 0;');
  Writer.WriteLine('width: 100%;');
  Writer.WriteLine('}');

  Writer.WriteLine('.action-button {');
  Writer.WriteLine('  float: right;');
  Writer.WriteLine('  border: 1px solid #e5e7eb;');
  Writer.WriteLine('  border-radius: .5rem;');
  Writer.WriteLine('  box-sizing: border-box;');
  Writer.WriteLine('  column-gap: 1rem;');
  Writer.WriteLine('  cursor: pointer;');
  Writer.WriteLine('  font-size: 10pt;');
  Writer.WriteLine('  font-weight: 700;');
  Writer.WriteLine('  line-height: 10px;');
  Writer.WriteLine('  padding: 3px 10px;');
  Writer.WriteLine('  margin-right: 20px;');
  Writer.WriteLine('  outline: 2px solid transparent;');
  Writer.WriteLine('  text-align: center;');
  Writer.WriteLine('  text-transform: none;');
  Writer.WriteLine('  transition: all .1s cubic-bezier(.4, 0, .2, 1);');
  Writer.WriteLine('}');

  Writer.WriteLine('.action-button:hover {');
  Writer.WriteLine('  box-shadow: -6px 8px 10px rgba(81,41,10,0.1),0px 2px 2px rgba(81,41,10,0.2);');
  Writer.WriteLine('}');

  Writer.WriteLine('.action-button:active {');
  Writer.WriteLine('  background-color: #f3f4f6;');
  Writer.WriteLine('  box-shadow: -1px 2px 5px rgba(81,41,10,0.15),0px 1px 1px rgba(81,41,10,0.15);');
  Writer.WriteLine('  transform: translateY(0.125rem);');
  Writer.WriteLine('}');

  Writer.WriteLine('.content {');
  Writer.WriteLine('  margin-top: 15px;');
  Writer.WriteLine('}');

  Writer.WriteLine('.wrap-collapsible {');
  Writer.WriteLine(' margin-bottom: 1.2rem 0;');
  Writer.WriteLine('}');

  Writer.WriteLine('input[type="checkbox"] {');
  Writer.WriteLine('  display: none;');
  Writer.WriteLine('}');

  Writer.WriteLine('.lbl-toggle {');
  Writer.WriteLine('  display: block;');
  Writer.WriteLine('  background-color: #777;');
  Writer.WriteLine('  color: white;');
  Writer.WriteLine('  cursor: pointer;');
  Writer.WriteLine('  padding: 6px;');
  Writer.WriteLine('  box-sizing: border-box;');
  Writer.WriteLine('  width: 100%;');
  Writer.WriteLine('  border: none;');
  Writer.WriteLine('  text-align: left;');
  Writer.WriteLine('  outline: none;');
  Writer.WriteLine('  font-size: 11pt;');
  Writer.WriteLine('  border-radius: 7px;');
  Writer.WriteLine('  margin-top: 7px;');
  Writer.WriteLine('  transition: all 0.25s ease-out;');
  Writer.WriteLine('}');

  Writer.WriteLine('.lbl-toggle:hover {');
  Writer.WriteLine('  background-color: #555;');
  Writer.WriteLine('}');

  Writer.WriteLine('.lbl-toggle::before {');
  Writer.WriteLine('  content: " ";');
  Writer.WriteLine('  display: inline-block;');
  Writer.WriteLine('  border-top: 5px solid transparent;');
  Writer.WriteLine('  border-bottom: 5px solid transparent;');
  Writer.WriteLine('  border-left: 5px solid currentColor;');

  Writer.WriteLine('  vertical-align: middle;');
  Writer.WriteLine('  margin-right: .7rem;');
  Writer.WriteLine('  transform: translateY(-2px);');

  Writer.WriteLine('  transition: transform .2s ease-out;');
  Writer.WriteLine('}');

  Writer.WriteLine('.toggle:checked + .lbl-toggle::before {');
  Writer.WriteLine('  transform: rotate(90deg) translateX(-3px);');
  Writer.WriteLine('}');

  Writer.WriteLine('.toggle:checked + .lbl-toggle + .collapsible-content {');
  Writer.WriteLine('  max-height: 10000vh;');
  Writer.WriteLine('}');

  Writer.WriteLine('.toggle:checked + .lbl-toggle {');
  Writer.WriteLine('border-bottom-right-radius: 0;');
  Writer.WriteLine('border-bottom-left-radius: 0;');
  Writer.WriteLine('}');


  Writer.WriteLine('.collapsible-content {');
  Writer.WriteLine('  max-height: 0px;');
  Writer.WriteLine('  overflow: auto;');
  Writer.WriteLine('  padding: 0 18px;');
  Writer.WriteLine('  font-family: monospace;');

 // Writer.WriteLine('  border-bottom: 1px solid rgba(250, 224, 66, .45);');
  Writer.WriteLine('  border: 1px solid rgba(250, 224, 66, .45);');

  Writer.WriteLine('  border-bottom-left-radius: 7px;');
  Writer.WriteLine('  border-bottom-right-radius: 7px;');
  Writer.WriteLine('}');

  Writer.WriteLine('.preformatted {');
  Writer.WriteLine('  white-space: var(--wrap);');
  Writer.WriteLine('  overflow-wrap: break-word;');
  Writer.WriteLine('}');

  Writer.WriteLine('.normal {');
  Writer.WriteLine('  white-space: normal;');
  Writer.WriteLine('}');

  Writer.WriteLine('.box-error {');
  Writer.WriteLine('  background-color: #f82323;');
  Writer.WriteLine('}');

  Writer.WriteLine('.box-error:hover {');
  Writer.WriteLine('  background-color: red;');
  Writer.WriteLine('}');

  Writer.WriteLine('.error-text {');
  Writer.WriteLine('  color: red;');
  Writer.WriteLine('  font-weight: bold;');
  Writer.WriteLine('}');

  Writer.WriteLine('.info-text {');
  Writer.WriteLine('  font-weight: bold;');
  Writer.WriteLine('}');

  Writer.WriteLine('.msg-caption {');
  Writer.WriteLine('  color: navy;');
  Writer.WriteLine('}');

  Writer.WriteLine('.msg-question {');
  Writer.WriteLine('  color: purple;');
  Writer.WriteLine('}');

  Writer.WriteLine('.msg-comment {');
  Writer.WriteLine('  color: gray;');
  Writer.WriteLine('  font-style: italic;');
  Writer.WriteLine('}');

  Writer.WriteLine('.msg-conclusion {');
  Writer.WriteLine('  color: green;');
  Writer.WriteLine('}');

  Writer.WriteLine('</style>');
end;

procedure THTMLLogger.WriteHTMLJs(const Writer: TStreamWriter);
begin
 Writer.WriteLine('<script>');
 Writer.WriteLine('function expandAll(value) {');
 Writer.WriteLine('    var aa = document.querySelectorAll("input.toggle[type=checkbox]");');
 Writer.WriteLine('    for (var i = 0; i < aa.length; i++){');
 Writer.WriteLine('        aa[i].checked = value;');
 Writer.WriteLine('    }');
 Writer.WriteLine('}');
 Writer.WriteLine('function wrapText() {');
 Writer.WriteLine('  var r = document.querySelector('':root'');');
 Writer.WriteLine('  var p = r.style.getPropertyValue(''--wrap'')');
 Writer.WriteLine('  if (p=="break-spaces" || p=="")  r.style.setProperty(''--wrap'', ''pre'');');
 Writer.WriteLine('  else r.style.setProperty(''--wrap'', ''break-spaces'');');
 Writer.WriteLine('}');

 Writer.WriteLine('</script>');

end;

procedure THTMLLogger.WriteHTMLEnd(const Writer: TStreamWriter);
begin
  Writer.WriteLine('</div>');  //content
  Writer.WriteLine('</body>');
  Writer.WriteLine('</html>');
end;


{ TMessageWriter }

constructor TMessageWriter.Create(const AWriteMessage: TMessageWriterAction);
begin
  FWriteMessage := AWriteMessage;
end;

{ TFirstPassMessageWriter }

constructor TFirstPassMessageWriter.Create(const AWriteMessage: TMessageWriterAction);
begin
  inherited Create(AWriteMessage);
  Stack := TStack<TLogMessage>.Create;
end;

destructor TFirstPassMessageWriter.Destroy;
begin
  Stack.Free;
  inherited;
end;

procedure TFirstPassMessageWriter.MarkAllRed(const List: TArray<TLogMessage>; const Count: integer);
begin
  for var i := 0 to Count - 1 do List[i].SectionHasErrors := true;

end;

procedure TFirstPassMessageWriter.Run(const Writer: TStreamWriter;
  const Msg: TLogMessage; var SectionId: integer);
begin
   if Msg.MessageVerbosity = TVerbosity.error then
   begin
     MarkAllRed(Stack.List, Stack.Count);
   end;

   if Msg.MessageType <> TMessageType.None then
   begin
     if Msg.SectionType = TSectionType.Open then Stack.Push(Msg)
     else
     if Msg.SectionType = TSectionType.Close then
     begin
       if Msg.SectionHasErrors then MarkAllRed(Stack.List, Stack.Count);
       Stack.Pop;
     end;
   end;

   if (Msg.MessageType = TMessageType.BasicInfo) or (Msg.MessageType = TMessageType.Summary) or (Msg.MessageType = TMessageType.Skipped) then
   begin
     InRelevantSection := Msg.SectionType = TSectionType.Open;
     FWriteMessage(Writer, Msg, SectionId);
     exit;
   end;

   if InRelevantSection
     then FWriteMessage(Writer, Msg, SectionId);

end;

{ TSecondPassMessageWriter }

procedure TSecondPassMessageWriter.Run(const Writer: TStreamWriter;
  const Msg: TLogMessage; var SectionId: integer);
begin
   if (Msg.MessageType = TMessageType.BasicInfo) or (Msg.MessageType = TMessageType.Summary) or (Msg.MessageType = TMessageType.Skipped) then
   begin
     InIrrelevantSection := Msg.SectionType = TSectionType.Open;
     exit;
   end;

   if InIrrelevantSection then exit;
   FWriteMessage(Writer, Msg, SectionId)
end;


end.

unit Deget.RCFileFormat;
{$i ../tmscommon.inc}

interface
uses Classes, SysUtils, Character;
type
  TRCModifier = class
  private
    const NewLine = #10;
  private
    LineNumber: integer;
    Data: string;
    DataPos: integer;
    SourceFile: string;
    DestFolder: string;
    PathModifier: TFunc<string, string>;
    Writer: TStreamWriter;
    State: string;
    SkipWriting: boolean;
    FileEncoding: TEncoding;

    function Eof(const ExtraChars: integer = 0): boolean;
    function GetChar: char;
    procedure MoveNext;
    function SkipWhitespace: boolean;
    function SkipSingleComments(const AlreadyStarted: boolean = false): boolean;
    function SkipMultiComments(const AlreadyStarted: boolean = false): boolean;
    procedure ErrorInLine;
    procedure Parse;


    constructor Create(const ASourceFile, ADestFile: string; const APathModifier: TFunc<string, string>);
    function ProcessDirectives: boolean;
    function ProcessStatements: boolean;
    function ProcessResources: boolean;
    procedure ProcessAll;

    function ReadWord(const extra: boolean = false): string;
    function ReadString: string;
    procedure SkipParameter;
    function GetFileName: string;
    function GetWord: string;
    procedure SkipBracketsOrBeginBlock;
  public
    destructor Destroy; override;
    class procedure AdaptAllPaths(const SourceFile, DestFile: string; const PathModifier: TFunc<string, string>); static;
  end;


implementation
uses IOUtils;

{ TRCModifier }

function FindDotFolder(const FileName: string): string;
begin
  var Folders := FileName;
  while Folders <> '' do
  begin
    Folders := TPath.GetDirectoryName(Folders);
    var OneFolder := TPath.GetFileName(Folders);
    if OneFolder.StartsWith('.') then exit(OneFolder);
  end;
  Result := ''
end;

constructor TRCModifier.Create(const ASourceFile, ADestFile: string;
  const APathModifier: TFunc<string, string>);
begin
  //I don't think BRCC will understand unicode anyway. The examples like in d7's German richedit_DRC.rc are in ANSI.
  //But ansi is problematic as it depends in the locale of the user using SmartSetup. But the actual encoding
  //doesn't matter, since there is a #pragma codepage inside the rc files used for localization. So we will open them
  //and save them as Win1252. It actually doesn't matter if the strings have the wrong charaters, they will be saved with the same codepoint.
  FileEncoding := TEncoding.GetEncoding(1252);

  SourceFile := ASourceFile;
  var Reader := TStreamReader.Create(SourceFile, FileEncoding);
  try
    Data := Reader.ReadToEnd;
  finally
    Reader.Free;
  end;

  DestFolder := TPath.GetDirectoryName(ADestFile);
  var FullDestFile := TPath.GetFullPath(ADestFile);

  //See https://github.com/tmssoftware/tms-smartsetup/issues/97
  var DotFolder := FindDotFolder(FullDestFile);
  if DotFolder <> '' then raise Exception.Create('Paths to .rc files can''t contain folders that start with a "." because BRCC32 can''t process them. The path "' + FullDestFile + '" contains the folder "' + DotFolder + '".');

  //see https://github.com/tmssoftware/tms-smartsetup/issues/134#issuecomment-1803873525
  //Do NOT use ANSI as codepage here.
  Writer := TStreamWriter.Create(ADestFile, false, FileEncoding);
  PathModifier := APathModifier;
  LineNumber := 1;
end;

destructor TRCModifier.Destroy;
begin
  Writer.Free;
  FileEncoding.Free;
  inherited;
end;

function TRCModifier.Eof(const ExtraChars: integer): boolean;
begin
  Result := DataPos + ExtraChars >= Data.Length;

end;

procedure TRCModifier.ErrorInLine;
begin
  raise Exception.Create('Cannot process line at file "' + SourceFile + ':' + IntToStr(LineNumber) + '".');
end;


procedure TRCModifier.MoveNext;
begin
  if not SkipWriting and not Eof then Writer.Write(GetChar);

  inc(DataPos);
  if not Eof and (GetChar = NewLine) then inc(LineNumber);

end;

function TRCModifier.GetChar: char;
begin
  if Eof then raise Exception.Create('Reached end of file "' + SourceFile + '".' + State);
  Result := Data[DataPos + 1];
end;

function TRCModifier.ReadWord(const extra: boolean = false): string;
begin
  Result := '';
  while True do
  begin
    var c := GetChar;
    var IsOk := extra and (not c.IsWhiteSpace);
    if (not c.IsLetterOrDigit) and (c <> '_') and (not IsOk) then break;
    Result := Result + c;

    MoveNext;
    if Eof then break;
  end;
end;

function TRCModifier.ReadString: string;
begin
  Result := '';
  if GetChar <> '"' then raise Exception.Create('Internal error. We should be in a string.');
  while true do
  begin
    MoveNext;
    var c := GetChar;
    if c = '"' then begin; MoveNext; exit; end;

    //doesn't look like it does escaping. c:\test works. But ms rc examples are escaped...
    //if (c = '\') then
    //begin
    //  MoveNext;
    //  c := GetChar;
    //end;
    Result := Result + c;

  end;

end;

function TRCModifier.GetWord: string;
begin
  var SaveLastPos := DataPos;
  var SaveLastLine := LineNumber;
  SkipWriting := true;
  try
    Result := ReadWord;
    DataPos := SaveLastPos;
    LineNumber := SaveLastLine;
  finally
    SkipWriting := false;
  end;

end;

function TRCModifier.GetFileName: string;
begin
  SkipWriting := true;
  try
    Result := '';
    if GetChar = '"' then exit(ReadString);
    exit(ReadWord(true));

  finally
    SkipWriting := false;
  end;

end;

procedure TRCModifier.SkipParameter;
begin
  SkipWhitespace;
  var c := GetChar;
  if c = '"' then begin; ReadString; exit; end;
  ReadWord;
end;

function TRCModifier.ProcessDirectives: boolean;
begin
  Result := GetChar = '#';
  if not Result then exit;

  while (not Eof) and (GetChar <> NewLine) do
  begin
    MoveNext;
  end;
end;

procedure TRCModifier.SkipBracketsOrBeginBlock;
begin
  var Level := 0;
  while True do
  begin
    SkipWhitespace;
    var NewWord := ReadWord(true);
    if NewWord = '' then MoveNext
    else if NewWord = '//' then SkipSingleComments(true)
    else if NewWord = '/*' then SkipMultiComments(true)
    else if (NewWord = 'BEGIN') or (NewWord = '{') then inc(Level)
    else if (NewWord = 'END') or (NewWord = '}') then
    begin
      dec(Level);
      if Level < 0 then ErrorInLine;
      if Level = 0 then exit;
    end;

  end;
end;

function TRCModifier.ProcessStatements: boolean;
begin
  Result := true;
  var Id := GetWord;
  if (Id = 'CAPTION')
   or(Id = 'CHARACTERISTICS')
   or(Id = 'CLASS')
   or(Id = 'EXSTYLE')
   or(Id = 'MENU')
   or(Id = 'STYLE')
   or(Id = 'VERSION')
  then
  begin
    ReadWord;
    SkipParameter;
    exit;
  end;

  if (Id = 'LANGUAGE')
  then begin
    ReadWord;
    SkipWhitespace;
    ReadWord;
    SkipWhitespace;
    if GetChar = ',' then MoveNext;
    SkipWhitespace;
    ReadWord;
    exit;
  end;
 // "FONT"
 // "MENUITEM"

  Result := false;

end;

function TRCModifier.ProcessResources: boolean;
begin
  Result := true;
  var Id := ReadWord;
  if not Skipwhitespace then exit(false);
  var ResourceType := ReadWord;
  if (ResourceType = 'ACCELERATORS')
   or(ResourceType = 'DIALOG')
   or(ResourceType = 'DIALOGEX')
   or(ResourceType = 'MENU')
   or(ResourceType = 'MENUEX')
   or(ResourceType = 'POPUP')
   or(ResourceType = 'STRINGTABLE')
   or(ResourceType = 'VERSIONINFO')
   or(ResourceType = 'DIALOGEX')
  then
  begin
    SkipWhitespace;
    while ProcessStatements do
    begin
      SkipWhitespace;
    end;
      SkipBracketsOrBeginBlock;
    exit;
  end;

  if (ResourceType = 'BITMAP')
   or(ResourceType = 'CURSOR')
   or(ResourceType = 'FONT')
   or(ResourceType = 'HTML')
   or(ResourceType = 'ICON')
   or(ResourceType = 'MESSAGETABLE')
   or(ResourceType = 'RCDATA')
  then
  begin
    SkipWhitespace;
    var FileName := GetFileName;
    var NewFileName := PathModifier(FileName);
    var NewFullFileName := TPath.GetFullPath(TPath.Combine(DestFolder, NewFileName));
    if not TFile.Exists(NewFullFileName) then raise Exception.Create('Can''t find resource "' + FileName +'" at location "' + NewFullFileName + '"');

    Writer.Write('"' + NewFileName +'"');
    exit;
  end;

  Result := false;

end;

procedure TRCModifier.ProcessAll;
begin
  //https://learn.microsoft.com/en-us/windows/win32/menurc/resource-definition-statements
  if Eof then exit;
  if ProcessDirectives then exit;
  if ProcessStatements then exit;
  //if ProcessControls then exit;   //not doing it by now.
  if ProcessResources then exit;

  ErrorInLine;

end;

function TRCModifier.SkipWhitespace: boolean;
begin
  Result := false;
  if Eof then exit;
  while (not Eof) and GetChar.IsWhiteSpace do
  begin
    Result := true;
    MoveNext;
  end;
end;

function TRCModifier.SkipSingleComments(const AlreadyStarted: boolean = false): boolean;
begin
  if not AlreadyStarted then
  begin
    if SkipWhitespace then exit(true);

    Result := false;
    if Eof(1) then exit;

    if (Data[DataPos + 1] <> '/') or (Data[DataPos + 2] <> '/') then exit;
    MoveNext; MoveNext;
  end;

  State := 'Reading // comment starting at line ' + IntToStr(LineNumber);
  while (not Eof) and (GetChar <> NewLine) do MoveNext;
  MoveNext;
  State :='';

  Result := true;
end;

function TRCModifier.SkipMultiComments(const AlreadyStarted: boolean = false): boolean;
begin
  if not AlreadyStarted then
  begin
    if SkipWhitespace then exit(true);

    Result := false;
    if Eof(1) then exit;

    if (Data[DataPos + 1] <> '/') or (Data[DataPos + 2] <> '*') then exit;
    MoveNext;
    MoveNext;
  end;

  State := 'Reading /* comment starting at line ' + IntToStr(LineNumber);
  repeat
    while GetChar <> '*' do MoveNext;
    MoveNext;
  until GetChar = '/';
  MoveNext;
  State :='';

  Result := true;
end;

procedure TRCModifier.Parse;
begin
  while DataPos < Data.Length do
  begin
    if SkipSingleComments then continue;
    if SkipMultiComments then continue;
    if SkipWhitespace then continue;
    ProcessAll;
  end;
end;


class procedure TRCModifier.AdaptAllPaths(const SourceFile, DestFile: string;
  const PathModifier: TFunc<string, string>);
begin
  //We try to be careful here, as it is better to err if there is something wrong than to keep going.
  //So we will raise an exception in anything we don't understand. Then we can modify this method
  //to understand it, or modify the RC files of the project to not have that.

  //See https://learn.microsoft.com/en-us/windows/win32/menurc/about-resource-files for the format doc.
  //It is basically a bastardized C file, and we are only going to support "Standard" things. (one res per line, etc),
  //not do out own C parser.
  //If the RC file is more complex, it likely will need to be changed to be used with Smart Setup.

  var Modifier := TRCModifier.Create(SourceFile, DestFile, PathModifier);
  try
    Modifier.Parse;
  finally
    Modifier.Free;
  end;
end;


end.

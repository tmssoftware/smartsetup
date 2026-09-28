unit BBStrings;

interface
uses Classes, SysUtils, Character, BBError;

  function BBYamlUnescapeString(const s: string; const ErrorInfo: TErrorInfo): string;
  function BBYamlUnescapeStringToEnd(const s: string; var Index: integer; const ErrorInfo: TErrorInfo): string;
  function BBYamlEscapeString(const s: string; const ToJSON: boolean): string;
  function IsBoolTrue(const s: string): boolean;
  function IsBoolFalse(const s: string): boolean;

{$IFDEF DEBUG}
  procedure BBStrings_InternalTests;
{$ENDIF}


implementation

function IsBoolTrue(const s: string): boolean;
begin
  var s1 := AnsiLowerCase(s);
  Result := (s1 = 'true') or (s1='1') or (s1 = 'on') or (s1 = 'yes');
end;

function IsBoolFalse(const s: string): boolean;
begin
  var s1 := AnsiLowerCase(s);
  Result := (s1 = 'false') or (s1='0') or (s1 = 'off') or (s1 = 'no')
end;

procedure RaiseUnterminatedString(const s: string; const ErrorInfo: TErrorInfo); //as its own proc so it doesn't slow down the loop
begin
  raise Exception.Create('Unterminated string: "' + s + '". ' + ErrorInfo.ToString);
end;

type
  TEscapeReplacement = record
  public
    ReplacedString: string;
    SkipInSourceString: integer;
  end;

  TStringReplacementFunction = reference to function(const s: string; const index: integer; const ErrorInfo: TErrorInfo): TEscapeReplacement;

function WalkEscapedString(const s: string; var Start: integer; const Escape, StringDelim: char;
         const OnEscape: TStringReplacementFunction; const ErrorInfo: TErrorInfo): string;
begin
  var Builder: TStringBuilder := nil;
  try
    var i := Start - 1;
    while (i < s.Length) do
    begin
      Inc(i);
      if i >= s.Length then RaiseUnterminatedString(s.Substring(Start), ErrorInfo);

      if s.Chars[i] = Escape then
      begin
        var Replacement := OnEscape(s, i + 1, ErrorInfo);
        if Replacement.SkipInSourceString > 0 then
        begin
          if Builder = nil then Builder := TStringBuilder.Create(s.Length - Start);
          Builder.Append(s, Start, i - Start);
          Builder.Append(Replacement.ReplacedString);
          Inc(i, Replacement.SkipInSourceString);

          Start := i + 1;
          continue;
        end;
      end;

      if s.Chars[i] = StringDelim then
      begin
        break;
      end;

    end;

    //i is positioned at the end quote.
    if Builder = nil then
    begin
      Result := s.Substring(Start, i - Start);
    end else
    begin
      Builder.Append(s, Start, i - Start);
      Result := Builder.ToString;
    end;
    Start := i + 1;
  finally
    Builder.Free;
  end;

end;

procedure RaiseUnterminatedEscapeString(const s: string; const ErrorInfo: TErrorInfo);
begin
  raise Exception.Create('Unterminated escape at the end of string: "' + s + '". ' + ErrorInfo.ToString);
end;

procedure RaiseInvalidHexValue(const s: string; const index: integer; const ErrorInfo: TErrorInfo);
begin
  raise Exception.Create('Invalid HEX escape at position ' + IntToStr(index) + ' of string: "' + s + '". ' + ErrorInfo.ToString);
end;

procedure RaiseUnknownEscapeChar(const s: string; const index: integer; const ErrorInfo: TErrorInfo);
begin
  raise Exception.Create('Unknown escape character "' + s.Chars[index] + '" at position ' + IntToStr(index) + ' of string "' + s + '". ' + ErrorInfo.ToString);
end;

procedure RaiseInvalidUnicodeChar(const s: string; const index: integer; const ErrorInfo: TErrorInfo);
begin
  raise Exception.Create('Invalid unicode character at position ' + IntToStr(index) + ' of string "' + s + '". ' + ErrorInfo.ToString);
end;


function GetUTF32RawChar(const s: string; const index, length: integer; const ErrorInfo: TErrorInfo): UCS4Char;
begin
  if index + length > s.Length then RaiseUnterminatedEscapeString(s, ErrorInfo);
  var Hex := s.Substring(index, length);
  var HexInt := 0;
  if not TryStrToInt('$' + Hex, HexInt) then RaiseInvalidHexValue(s, index, ErrorInfo);
  if (HexInt < 0) or (HexInt > Char.MaxCodePoint) or not Char.IsDefined(UCS4Char(HexInt)) then RaiseInvalidUnicodeChar(s, index, ErrorInfo);

  Result := UCS4Char(HexInt);
end;

function GetUTF32Char(const s: string; const index: integer; const ErrorInfo: TErrorInfo): string;
begin
  var Character := GetUTF32RawChar(s, index, 8, ErrorInfo);
  Result := Char.ConvertFromUtf32(Character);
end;

function GetUTF16Char(const s: string; const index: integer; const ErrorInfo: TErrorInfo): string;
begin
  var Character := GetUTF32RawChar(s, index, 4, ErrorInfo);
  Result := Char(Character);
end;

function NextIsEscape(const s: string; const index: integer; const Escape: string): boolean;
begin
  Result := s.Substring(index, 2) = Escape;
end;

function GetUTF8Char(const s: string; const index: integer; const Escape: string; const ErrorInfo: TErrorInfo): TEscapeReplacement;
const
  CharLength = 2;
begin
  var Utf8Bytes: TArray<byte> := nil;
  var UTF8BytesLen := 0;
  Result.SkipInSourceString := 0;
  var i := index;
  while True do
  begin
    if i + CharLength > s.Length then RaiseUnterminatedEscapeString(s, ErrorInfo);
    var Hex := s.Substring(i, CharLength);
    var HexInt := 0;
    if not TryStrToInt('$' + Hex, HexInt) then RaiseInvalidHexValue(s, i, ErrorInfo);
    if (HexInt < 0) or (HexInt > 255) then RaiseInvalidUnicodeChar(s, i, ErrorInfo);

    if Length(Utf8Bytes) <= UTF8BytesLen then SetLength(Utf8Bytes, UTF8BytesLen + 24);
    Utf8Bytes[UTF8BytesLen] := byte(HexInt);
    Inc(UTF8BytesLen);
    inc(i, CharLength);
    if not NextIsEscape(s, i, Escape) then break;
    inc(i, Escape.Length);
  end;

  Result.SkipInSourceString := i - index + 1;

  result.ReplacedString := TEncoding.UTF8.GetString(Utf8Bytes, 0, UTF8BytesLen);
end;

function UnEscapeSingleQuote(const s: string; const index: Integer; const ErrorInfo: TErrorInfo): TEscapeReplacement;
begin
  if index < s.Length then
  begin
    var c := s.Chars[index];
    if c = '''' then
    begin
      Result.ReplacedString := '''';
      Result.SkipInSourceString := 1;
      exit;
    end;
  end;

  Result.ReplacedString := '';
  Result.SkipInSourceString := -1;

end;

function UnEscapeDoubleQuote(const s: string; const index: Integer; const ErrorInfo: TErrorInfo): TEscapeReplacement;
begin
  //See '5.7. Escaped Characters' in https://yaml.org/spec/1.2.2/
  if index >= s.Length then RaiseUnterminatedEscapeString(s, ErrorInfo);

  var c := s.Chars[index];
  Result.SkipInSourceString := 1;
  case c of
    '0': begin Result.ReplacedString := #$00; exit; end;
    'a': begin Result.ReplacedString := #$07; exit; end;
    'b': begin Result.ReplacedString := #$08; exit; end;
    't': begin Result.ReplacedString := #$09; exit; end;
    'n': begin Result.ReplacedString := #$0A; exit; end;
    'v': begin Result.ReplacedString := #$0B; exit; end;
    'f': begin Result.ReplacedString := #$0C; exit; end;
    'r': begin Result.ReplacedString := #$0D; exit; end;
    'e': begin Result.ReplacedString := #$1B; exit; end;
    ' ',
    '"',
    '/',
    '\': begin Result.ReplacedString := c; exit; end;

    'N': begin Result.ReplacedString := #$85; exit; end;
    '_': begin Result.ReplacedString := #$A0; exit; end;
    'L': begin Result.ReplacedString := #$2028; exit; end;
    'P': begin Result.ReplacedString := #$2029; exit; end;

    'x': begin Result := GetUTF8Char(s, index + 1, '\x', ErrorInfo); exit; end;
    'u': begin Result.SkipInSourceString := 4 + 1; Result.ReplacedString := GetUTF16Char(s, index + 1, ErrorInfo); exit; end;
    'U': begin Result.SkipInSourceString := 8 + 1; Result.ReplacedString := GetUTF32Char(s, index + 1, ErrorInfo); exit; end;
  end;

  RaiseUnknownEscapeChar(s, index, ErrorInfo);
end;

function BBYamlUnescapeString(const s: string; const ErrorInfo: TErrorInfo): string;
begin
  var Start := 1;
  if s.StartsWith('''') and s.EndsWith('''') and (s.Length > 1)
    then exit(WalkEscapedString(s, Start, '''', '''', UnEscapeSingleQuote, ErrorInfo));

  if s.StartsWith('"') and s.EndsWith('"') and (s.Length > 1)
    then exit(WalkEscapedString(s, Start, '\', '"', UnEscapeDoubleQuote, ErrorInfo));
  Result := s;
end;

function BBYamlUnescapeStringToEnd(const s: string; var Index: integer; const ErrorInfo: TErrorInfo): string;
begin
  Inc(Index);
  if Index >= s.Length then raise Exception.Create('Error: Invalid index to string "' + s + '"' + ErrorInfo.ToString);

  if s.Chars[Index - 1] = ''''
    then exit(WalkEscapedString(s, Index, '''', '''', UnEscapeSingleQuote, ErrorInfo));

  if s.Chars[Index - 1] = '"'
    then exit(WalkEscapedString(s, Index, '\', '"', UnEscapeDoubleQuote, ErrorInfo));

  raise Exception.Create('Error: The string "' + s + '" must start with a string delimiter. ' + ErrorInfo.ToString);
end;

function SingleQuote(const s: string): string;
begin
  Result := '''' + s.Replace('''', '''''', [TReplaceFlag.rfReplaceAll]) + '''';
end;

function DoubleQuote(const s: string): string;
begin
  Result := '"' + s
             .Replace('\', '\\', [TReplaceFlag.rfReplaceAll])
             .Replace(#8, '\b', [TReplaceFlag.rfReplaceAll])
             .Replace(#9, '\t', [TReplaceFlag.rfReplaceAll])
             .Replace(#10, '\n', [TReplaceFlag.rfReplaceAll])
             .Replace(#13, '\r', [TReplaceFlag.rfReplaceAll])
             .Replace('"', '\"', [TReplaceFlag.rfReplaceAll])
             + '"';
end;

function BBYamlEscapeString(const s: string; const ToJSON: boolean): string;
begin
  //https://blogs.perl.org/users/tinita/2018/03/strings-in-yaml---to-quote-or-not-to-quote.html
  if ToJSON or (s.IndexOfAny([#9, #10, #13]) >= 0) then exit(DoubleQuote(s));

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
  or s.StartsWith('%')
  or s.EndsWith(':')
  or s.EndsWith(' ')
  then exit(SingleQuote(s));

  var value: integer;
  if (TryStrToInt(s, value)) then exit(SingleQuote(s));
  if (IsBoolTrue(s)) then exit(SingleQuote(s));
  if (IsBoolFalse(s)) then exit(SingleQuote(s));


  exit(s);
end;


{$IFDEF DEBUG}
procedure BBStrings_InternalTests;
var
  ErrorInfo: TErrorInfo;

  procedure AssertErr(const s: string; const ErrMessage: string);
  begin
    var ok := false;
    try
      BBYamlUnescapeString(s, ErrorInfo);
    except on e: exception do
    begin
      ok := e.Message.Contains(ErrMessage);
    end;
    end;
    if not ok then raise Exception.Create('Error testing yaml strings. "' + s + '" should return an error decoding.');
  end;

  procedure AssertSame(const s, sorig: string);
  begin
    var snew := BBYamlUnescapeString(s, ErrorInfo);
    Assert(snew = sorig, 'string "' + snew + '" is different from string "' + sorig + '"');
  end;

begin
  ErrorInfo := TErrorInfo.Create(false);
  try
    AssertErr('"\W"', 'Unknown escape character "W" at position 2 of string');
    AssertErr('"\x0"', 'Invalid HEX escape at position 3 of string: ""\x0""');
    AssertErr('"\x0t"', 'Invalid HEX escape at position 3 of string: ""\x0t""');
    AssertSame('"\x0a"', #$0A);
    AssertSame('"\x0A"', #$0A);
    AssertSame('"\n"', #$0A);
    AssertSame('"a\nn"', 'a'#$0A'n');
    AssertSame('"c:\\b"', 'c:\b');
    AssertSame('"c:\\_"', 'c:\_');
    AssertSame('"c:\\\_"', 'c:\'#$A0);
    AssertSame('"c:\\\"a"', 'c:\"a');
    AssertSame('"c:\\\ba"', 'c:\'#8'a');
    AssertErr('"a\"', 'Unterminated string: ""');
    AssertErr('''a''''', 'Unterminated string: ""');

    AssertErr('"\uD83"', 'Invalid HEX escape at position 3 of string: ""\uD83""');
    AssertErr('"\uD83D\uDE0"', 'Invalid HEX escape at position 9 of string: ""\uD83D\uDE0""');
    AssertSame('"\uD83D\uDE0A"', #$d83d#$de0a);
    AssertErr('"\U1F60A"', 'Unterminated escape at the end of string: ""\U1F60A""');
    AssertSame('"\U0001F60A"', #$d83d#$de0a);
    AssertSame('"\xf0\x9F\x98\x80\xF0\x9F\x94\xA5"', #$D83D#$DE00#$D83D#$DD25);
    AssertSame('"\xf0\x9F\x98\x80 this \x65\x66 \x67\u0043\U00000045is on \xF0\x9F\x94\xA5\n\t\""', #$D83D#$DE00' this ef gCEis on '#$D83D#$DD25#$0A#$09'"');
    AssertSame('''*''', '*');
    AssertSame('''*''''*''', '*''*');

    Assert(BBYamlEscapeString('\"', true) = '"\\\""');
  finally
    ErrorInfo.Free;
  end;
end;

{$endif}
end.

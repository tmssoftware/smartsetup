unit BBStrings;

interface
uses Classes, SysUtils;

  function BBYamlUnescapeString(const s: string): string;
  function BBYamlEscapeString(const s: string; const ToJSON: boolean): string;
  function IsBoolTrue(const s: string): boolean;
  function IsBoolFalse(const s: string): boolean;

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

function UnEscapeDoubleQuote(const s: string): string;
begin
  //Someday we can do a better parser here.
  Result := s;
  Result := Result.Replace('\b', #8, [rfReplaceAll]);
  Result := Result.Replace('\t', #9, [rfReplaceAll]);
  Result := Result.Replace('\n', #10, [rfReplaceAll]);
  Result := Result.Replace('\r', #13, [rfReplaceAll]);
  Result := Result.Replace('\\', '\', [rfReplaceAll]);
  Result := Result.Replace('\/', '/', [rfReplaceAll]);
  Result := Result.Replace('\"', '"', [rfReplaceAll]);
end;

function BBYamlUnescapeString(const s: string): string;
begin
  if s.StartsWith('''') and s.EndsWith('''') and (s.Length > 1) then exit(s.Substring(1, s.Length - 2).Replace('''''', '''', [rfReplaceAll]));
  if s.StartsWith('"') and s.EndsWith('"') and (s.Length > 1) then exit(UnEscapeDoubleQuote(s.Substring(1, s.Length - 2)));
  Result := s;
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


end.

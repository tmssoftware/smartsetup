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
  //Single pass, so an escaped backslash can't be combined with the next character.
  //Sequential replaces turned "C:\\builds" into "C:\" + #8 + "uilds".
  var sb := TStringBuilder.Create(s.Length);
  try
    var i := 0;
    while i < s.Length do
    begin
      var c := s.Chars[i];
      if (c = '\') and (i + 1 < s.Length) then
      begin
        var e := s.Chars[i + 1];
        case e of
          'b': sb.Append(#8);
          't': sb.Append(#9);
          'n': sb.Append(#10);
          'r': sb.Append(#13);
          '\', '/', '"': sb.Append(e);
          else sb.Append(c).Append(e); //unknown escapes are kept as they were.
        end;
        inc(i, 2);
        continue;
      end;
      sb.Append(c);
      inc(i);
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
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

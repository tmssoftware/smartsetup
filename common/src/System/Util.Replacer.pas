unit Util.Replacer;

(*
  Replaces %variable% by values. It also includes code or not depending on a variable.
  For example:
  %{#is_registered}%
  %delphi_version_selector%
  %delphi_platform_selector%
  %{/is_registered}%

  For those blocks, return a BoolToStr. For example:
  if varName = 'is_registered' then exit(BoolToStr(SetupType = TSetupType.Registered, true));
  We will include it only if the result is 'true'

  To escape a variable, put it between duplicated %. Example: %%variable%% will be returned as %variable%
*)

interface
uses Classes, SysUtils, Character, IOUtils;

function ParseString(s: string; const Replacer: TFunc<string, string>; const RecursionLevel: integer = 0): string;
procedure CopyAndReplace(const Source, Target: string; const Replacer: TFunc<string, string>);

implementation
function IsValidVarName(const s: string; const k: integer): boolean;
begin
  Result := Char.IsLetterOrDigit(s, k) or (s.Chars[k] = '_') or (s.Chars[k] = '-') or (s.Chars[k] = '.');
end;

function ReadVar(const s: string; const enddelim: char; const i: integer; const CanEscape: boolean; out k: integer; out IsEscaped: boolean): string; overload;
begin
  k := i + 1;
  var Level := 0;
  if CanEscape then while (k < s.Length) and (s.Chars[k] = enddelim) do begin inc(k); inc(Level); end;
  IsEscaped := Level > 0;
  while (k < s.Length) and IsValidVarName(s, k)  do inc(k);
  if (k >= s.Length) or (s.Chars[k] <> enddelim) then exit('');
  if CanEscape then while (k < s.Length) and (s.Chars[k] = enddelim) and (Level > 0) do begin inc(k); dec(Level); end;
  if Level <> 0 then exit('');

  Result := s.Substring(i + 1, k - i - 1);
end;

function ReadVar(const s: string; const enddelim: char; const i: integer; out k: integer): string; overload;
begin
  var IsEscaped := false;
  Result := ReadVar(s, enddelim, i, false, k, IsEscaped);
end;

function IsSpace(const c: char): boolean;
begin
  Result := (c = #$0020) or (c = #$0009);
end;

procedure SkipEnter(const s: string; var k: integer; const Offset: integer = 0);
begin
  while (k + Offset < s.Length) and IsSpace(s.Chars[k + Offset]) do inc(k);

  if (k + Offset < s.Length) and (s.Chars[k + Offset] = #13) then inc(k);
  if (k + Offset < s.Length) and (s.Chars[k + Offset] = #10) then inc(k);
end;

function StartsInNewLine(const s: string; k: integer): boolean;
begin
  while (k >= 0) and IsSpace(s.Chars[k]) do dec(k);
  if k < 0 then exit(true);
  exit (s.Chars[k] = #10);
end;

function ReadSection(const s: string; const i: integer; out k, start_section, end_section: integer): string;
begin
  Result := '';
  k := i + 1;
  if (k >= s.Length) or  (s.Chars[k] <> '{') then exit;
  inc(k);
  if (k >= s.Length) or  (s.Chars[k] <> '#') then exit;

  var OpenTagStartsInNewLine := StartsInNewLine(s, i - 1);

  var k2 := 0;
  var Result1 := ReadVar(s, '}', k , k2);
  if (k2 >= s.Length) or (s.Chars[k2 + 1] <> '%') then raise Exception.Create('Invalid variable in section: ' + Result1);

  k := k2 + 2;
  if OpenTagStartsInNewLine then SkipEnter(s, k);
  start_section := k;


  while(true) do
  begin
    while (k + 2 < s.Length) and ((s.Chars[k] <> '%') or (s.Chars[k + 1] <> '{') or (s.Chars[k + 2] <> '/')) do inc(k);
    if (k >= s.Length) or (s.Chars[k] <> '%') then exit('');

    end_section := k - 1;

    var Result2 := ReadVar(s, '}', k + 2 , k2);
    if (k2 >= s.Length) or (s.Chars[k2 + 1] <> '%') then raise Exception.Create('Invalid variable in section: ' + Result2);
    if Result1 = Result2 then break;
    inc(k);
  end;
  var EndTagStartsInNewLine := StartsInNewLine(s, k - 1);
  k := k2 + 1;
  if EndTagStartsInNewLine then SkipEnter(s, k, 1);

  Result := Result1;
end;

function ParseString(s: string; const Replacer: TFunc<string, string>; const RecursionLevel: integer = 0): string;
begin
  if RecursionLevel > 20 then raise Exception.Create('Too many recursion levels when replacing variables.');

  Result := '';
  var i := 0;
  var LastPos := 0;
  while i < s.Length do
  begin
    if s.Chars[i] = '%' then
    begin
      var k := 0; var IsEscaped := false;
      var Variable := ReadVar(s, '%', i, true, k, IsEscaped);
      if Variable <> '' then
      begin
        var VarValue := Variable;
        if not IsEscaped then VarValue := Replacer(Variable);

        Result := Result + s.Substring(LastPos, i - LastPos) + VarValue;
        i := k;
        LastPos := k + 1;
      end
      else
      begin
        var sec_start, sec_end: integer;
        var Section := ReadSection(s, i, k, sec_start, sec_end);
        if Section <> '' then
        begin
          var IncludeSection := SameText(Replacer(Section), 'true');
          Result := Result + s.Substring(LastPos, i - LastPos);
          if IncludeSection then
          begin
            var SectionText := s.Substring(sec_start, sec_end - sec_start + 1);
            Result := Result + ParseString(SectionText, Replacer, RecursionLevel + 1);
          end;

          i := k;
          LastPos := k + 1;
        end

      end;
    end;
    inc(i);
  end;
  Result := Result + s.Substring(LastPos);

end;

procedure CopyAndReplace(const Source, Target: string; const Replacer: TFunc<string, string>);
begin
  var Text := TFile.ReadAllText(Source);
  var NewText := ParseString(Text, Replacer);
  TFile.WriteAllText(Target, NewText, TEncoding.UTF8);
end;


end.

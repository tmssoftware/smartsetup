unit Deget.Filer.DprFile;

interface
uses Classes, SysUtils, Generics.Defaults, Generics.Collections;

procedure ChangePath(const SourceFileName, DestFileName: string; const Replacement, RCReplacement: TFunc<string, string>);

implementation
uses IOUtils, UTmsBuildSystemUtils;

function ReplaceIn(const Line: string; const in_idx: integer; const Replacement: TFunc<string, string>): string;
begin
  var UnitNameFull := Line.SubString(0, in_idx);
  var UnitName := UnitNameFull.Trim;
  if UnitName.Contains(' ') then exit(Line);
  var Right := Line.SubString(in_idx + 3).Trim;
  var Colon := '';
  if Right.EndsWith(';') then Colon := ';';
  if Right.EndsWith(',') then Colon := ',';
  if Colon = '' then exit(Line);
  Right := Right.SubString(0, Right.Length - 1).Trim;
  if not Right.StartsWith('''') then exit(Line);
  //it can be like Forms.Main in 'src\Forms.Main.pas' {MainForm},
  var endq_idx := Right.IndexOf('''', 1); //we assume no ' in filenames...
  if endq_idx < 0 then exit(Line);
  Colon := Right.Substring(endq_idx + 1) + Colon;
  Right := Right.Substring(0, endq_idx + 1);


  if not Right.ToLower.Contains(UnitName.ToLower) then exit;

  Result := UnitNameFull + ' in ''' + Replacement(Right.Substring(1, Right.Length - 2)) + '''' + Colon;
end;

function GetLastPart(const Resources: string; const Replacement: TFunc<string, string>): string;
begin
  var start: integer;
  var HasQuotes := Resources.EndsWith('''');
  if HasQuotes then
  begin
    start := Resources.Substring(0, Resources.Length - 1).LastIndexOf(''''); //we assume no escaped quotes. I am not even sure that would work in delphi.
    if start <= 0  then exit('');
  end
  else
  begin
    start := Resources.LastIndexOf(' ');
    if start <= 0  then start := -1; //{ $R a.res}
  end;

  var ResToReplace := Resources.Substring(start + 1).Trim;
  if ResToReplace.Contains('*') then exit; // we don't replace those.

  if HasQuotes then ResToReplace := ResToReplace.Substring(0, ResToReplace.Length - 1);

  Result := Resources.Substring(0, start + 1) + Replacement(ResToReplace);
  if HasQuotes then Result := Result + '''';

end;

function ReplaceResources(const id, Line, LineLower: string; const Replacement: TFunc<string, string>): string;
begin
  var idx_res := LineLower.IndexOf(id);
  var Resources := Line.SubString(idx_res + id.Length).Trim;
  if not Resources.EndsWith('}') then exit(Line);
  Resources := Resources.SubString(0, Resources.Length - 1).Trim;

  //Unit names and project names cannot have spaces. But resources can. Some examples:
  //{$R 'New Text Document.res' 'spaces\New Text Document.rc'}
  //{$R 'spaces\New Text Document - Copy.res'}
  //And of course, it can have no spaces:
  //{$R 'tms_resources.res' 'tms_resources.rc'}
  var LastPart := GetLastPart(Resources, Replacement);

  if LastPart <> '' then exit(Line.Substring(0, idx_res + id.Length) + LastPart + '}');

  Result := line;
end;

function Adapt(const Line: string; const Replacement, RCReplacement: TFunc<string, string>): string;
begin
  var LineLower := Line.ToLower;
  var in_idx := LineLower.IndexOf(' in ');
  if in_idx > 0 then exit(ReplaceIn(Line, in_idx, Replacement));

  var LineLowerTrim := LineLower.Trim;
  if LineLowerTrim.EndsWith('}') then
  begin
    if LineLowerTrim.StartsWith('{$r ') then exit(ReplaceResources('{$r ', Line, LineLower, RCReplacement));
    if LineLowerTrim.StartsWith('{$resource ') then exit(ReplaceResources('{$resource ', Line, LineLower, RCReplacement));
    if LineLowerTrim.StartsWith('{$i ') then exit(ReplaceResources('{$i ', Line, LineLower, Replacement));
    if LineLowerTrim.StartsWith('{$include ') then exit(ReplaceResources('{$include ', Line, LineLower, Replacement));
  end;


  Result := Line;
end;


procedure ChangePath(const SourceFileName, DestFileName: string; const Replacement, RCReplacement: TFunc<string, string>);
begin
var Reader := TStreamReader.Create(SourceFileName);
  try
    //TEncoding.UTF8 won't work in D7, as it writes a bom.
    var Encoding := TUTF8NoBOMEncoding.Create;
    try
      var Writer := TStreamWriter.Create(DestFileName, false, Encoding);
      try
        while not Reader.EndOfStream do
        begin
          var Line := Reader.ReadLine;
          Writer.WriteLine(Adapt(Line, Replacement, RCReplacement));
        end;
      finally
        Writer.Free;
      end;
    finally
      Encoding.Free;
    end;
  finally
    Reader.Free;
  end;
end;

end.

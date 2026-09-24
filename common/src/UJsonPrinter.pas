unit UJsonPrinter;

interface

uses
  System.Classes, System.JSON;

procedure OutputJson(Value: TJSONValue);

implementation

uses
  System.SysUtils;

/// <summary>Replaces every character above 127 with its \uXXXX escape, so the
///   result is pure ASCII.
///
///   This is safe on already formatted JSON: the structure - braces, colons,
///   commas, indentation - is ASCII by definition, so anything above 127 can
///   only be inside a string literal, where \uXXXX is the escape JSON defines
///   for it (RFC 8259, section 7).
///
///   Characters outside the BMP are a surrogate pair in a Delphi string and
///   come out as two escapes, which is exactly what the RFC asks for.</summary>
function EscapeNonAscii(const Json: string): string;
begin
  var Builder := TStringBuilder.Create(Length(Json));
  try
    for var Ch in Json do
      if Ch <= #127 then
        Builder.Append(Ch)
      else
        Builder.Append(System.SysUtils.Format('\u%.4x', [Ord(Ch)]));
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

procedure OutputJson(Value: TJSONValue);
begin
  var Lines := TStringList.Create;
  try
    // Escaping everything above 127 makes the output pure ASCII, which reads
    // identically in every code page. Without it the text goes out in the code
    // page of Output - the ANSI code page on Windows - and a single non-ASCII
    // character makes the whole response invalid UTF-8, so a strict reader
    // fails on all of it instead of on that one character.
    Lines.Text := EscapeNonAscii(Value.Format(2));
    for var Line in Lines do
      WriteLn(Line);
  finally
    Lines.Free;
  end;
end;

end.

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
///   come out as two escapes, which is exactly what the RFC asks for.
///
///   Every JSON response written to the console passes through here, so the
///   common case - no character above 127 at all - does no work: it returns
///   the input untouched, without allocating a builder. When there is work to
///   do, the text between two escapes is appended as one chunk rather than
///   character by character, and the escape itself avoids System.Format,
///   which is slow on a path that may run for every character.</summary>
function EscapeNonAscii(const Json: string): string;
begin
  var Start := 0;
  var Builder: TStringBuilder := nil;
  try
    for var I := 0 to Json.Length - 1 do
      if Json.Chars[I] > #127 then
      begin
        // The result is longer than the input - one character becomes six.
        if Builder = nil then
          Builder := TStringBuilder.Create(Round(Json.Length * 1.3));

        Builder.Append(Json, Start, I - Start);
        Builder.Append('\u');
        Builder.Append(IntToHex(Ord(Json.Chars[I]), 4));
        Start := I + 1;
      end;

    if Builder = nil then
      Exit(Json);

    Builder.Append(Json, Start, Json.Length - Start);
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

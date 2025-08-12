unit Convert.SummaryToJson;

interface
uses SysUtils, Classes, StrUtils, Windows;

procedure RegisterConvertSummaryToJsonCommand;

implementation
uses VSoft.CommandLine.Options, UCommandLine, JSon, UJsonPrinter;

//windows only. to make it work everywhere, it should be changed to old blockwrite/read stuff: https://stackoverflow.com/questions/46293975/delphi-tokyo-for-linux-stream-file-contents-to-stdout-console-output
function GetInputStream: TStream;
begin
  var StdInHandle := GetStdHandle(STD_INPUT_HANDLE);

  if StdInHandle = INVALID_HANDLE_VALUE then
    begin
      raise Exception.Create('Invalid standard input handle.');
    end;

  Result := THandleStream.Create(StdInHandle);

end;

function RemoveLogMetadata(const S: string): string;
begin
  var P := Pos(']', S);
  if P > 0 then
    Result := Copy(S, P + 1)
  else
    Result := S;
end;

procedure ExtractSummary(const Output: string);
begin
  var InSummary := False;
  var Root := TJSONObject.Create;
  try
    for var Line in Output.Split([#10]) do
    begin
      var CleanLine := RemoveLogMetadata(Line.Trim);
      if System.Pos('=== Build Summary ===', CleanLine) > 0 then
      begin
        InSummary := True;
        Continue;
      end;
      if InSummary then
      begin
        if Trim(CleanLine) = '' then
          Break;

        if not CleanLine.StartsWith('   -') then continue;

        var Item := TJSONObject.Create;
        var Data := CleanLine.Split(['->']);
        if Length(Data) <> 2 then continue;
        Root.AddPair(Data[0].Trim.Substring(1).Trim, Data[1].Trim);
      end;
    end;
    OutputJson(Root);
  finally
    Root.Free;
  end;
end;

procedure ConvertSummaryToJsonCommand;
const
  BufferSize = 4095;
begin
  var Buffer: TBytes := nil;
  SetLength(Buffer, BufferSize + 1 );
  var TextOutput := '';
  var InputStream := GetInputStream;
  try
    while true do
    begin
      var BytesRead := InputStream.Read(Buffer, BufferSize);
      if BytesRead < 1 then Break;
      TextOutput := TextOutput + TEncoding.UTF8.GetString(Buffer, 0, BytesRead);
    end;
  finally
    InputStream.Free;
  end;

  ExtractSummary(TextOutput);
end;


procedure RegisterConvertSummaryToJsonCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('summary-to-json', '', 'Reads the input stream and returns the summary as a json object.',
    '',
    '');

  AddCommand(cmd.Name, 'Convert', ConvertSummaryToJsonCommand);
end;


end.


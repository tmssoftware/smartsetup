unit Deget.DpkFileformat;
{$i ../tmscommon.inc}

interface
function GetFilesFromDpk(const dpk: string; const root: string; const includeDfm: boolean): TArray<string>;
procedure CopyDpk(const Source, Dest, Add: string);

type
  TStringProc = reference to procedure(var Value: string);

implementation
uses IOUtils, SysUtils, Classes, UTmsBuildSystemUtils, Generics.Collections;

function GetFilesFromDpk(const dpk: string; const root: string; const includeDfm: boolean): TArray<string>;
begin
  var Lines := TFile.ReadAllLines(dpk);
  var InContains := false;
  Result := nil;
  var Files := TList<string>.Create;
  try
    for var Line in Lines do
    begin
      var LineTrim := Line.Trim;
      var LineDown := Lowercase(LineTrim);
      if not InContains and (LineDown <> 'contains') then continue;
      InContains := true;

      var idx := LineDown.IndexOf(' in ''');
      if idx >= 0 then
      begin
        var iend := LineDown.LastIndexOf('''');
        var FileName := TPath.Combine(root, LineTrim.Substring(idx + 5, iend - idx - 5));
        {$IFNDEF MSWINDOWS}
        FileName := FileName.Replace('\', '/');
        {$ENDIF}
        Files.Add(FileName);
        if includeDfm then
        begin
          var DfmFileName := TPath.ChangeExtension(FileName, '.dfm');
          if TFile.Exists(DfmFileName) then
          begin
            Files.Add(DfmFileName);
          end;

        end;
      end;
    end;

    Result := Files.ToArray;
  finally
    Files.Free;
  end;

end;

function AdaptPath(const Line, Add: string): string;
begin
  if Line.Trim.StartsWith('{$R ''.', true) then
  begin
    Result := Line.Trim.Substring(0, 5) + Add + Line.Trim.Substring(5);
    exit;
  end;
  exit(Line);
end;

procedure CopyDpk(const Source, Dest, Add: string);
begin
  TDirectory_CreateDirectory(TPath.GetDirectoryName(Dest));
  var Lines := TFile.ReadAllLines(Source);
  var InContains := false;

  var StreamWriter := TStreamWriter.Create(Dest);
  try
    for var Line in Lines do
    begin
      var LineTrim := Line.Trim;
      var LineDown := Lowercase(LineTrim);
      if not InContains and (LineDown <> 'contains') then
      begin
        StreamWriter.WriteLine(AdaptPath(Line, Add));
        continue;
      end;
      InContains := true;
      var idx := LineDown.IndexOf(' in ''');
      if idx >= 0 then
      begin
        StreamWriter.Write(LineTrim.Substring(0, idx + 5));
        StreamWriter.Write(Add);
        StreamWriter.WriteLine(LineTrim.Substring(idx + 5));
      end
      else
      begin
        StreamWriter.WriteLine(Line);
      end;
    end;
  finally
    StreamWriter.Free;
  end;
end;

function AdaptLine(const Line: string; OnFile: TStringProc): string;
begin
  Result := Line;
  var idx := Pos('{$R ''', Line.ToUpper);
  if idx > 0 then
  begin
    idx := idx + 4;
    var FileName := Line.Substring(idx);
    var idx2 := FileName.IndexOf('''');
    FileName := FileName.Substring(0, idx2);

    var NewLine := Line.Remove(idx, Length(FileName));
    if Assigned(OnFile) then
      OnFile(FileName);
    NewLine.Insert(idx, FileName);
    Result := NewLine;
  end
end;

end.

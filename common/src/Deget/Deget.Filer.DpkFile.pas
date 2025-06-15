unit Deget.Filer.DpkFile;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, System.Types, Deget.CoreTypes, Deget.Filer.Types;

type
  TDpkData = class
  private
    FDcrFiles: TIncludeFiles;
    FPasFiles: TPasIncludeFiles;
    FRequires: TStrings;
    FDescription: string;
    FUsage: TPackageUsage;
    FExplicitRebuild: Boolean;
    FLibSuffix: string;
    FPackageName: string;
  public
    constructor Create;
    destructor Destroy; override;
    property DcrFiles: TIncludeFiles read FDcrFiles;
    property PasFiles: TPasIncludeFiles read FPasFiles;
    property Requires: TStrings read FRequires;

    property PackageName: string read FPackageName write FPackageName;
    property Description: string read FDescription write FDescription;
    property Usage: TPackageUsage read FUsage write FUsage;
    property ExplicitRebuild: Boolean read FExplicitRebuild write FExplicitRebuild;
    property LibSuffix: string read FLibSuffix write FLibSuffix;
  end;

  TDpkFiler = class
  strict private
    FIDEName: TIDEName;
    FFileName: string;
    FLines: TStrings;
  strict protected
    function IndexOf(const Value: string): Integer;
    function IndexOfStart(const Value: string): Integer;
    function ReadSectionItems(const Section: string): TArray<string>;
    function IsDirective(const Line, Directive: string; var Value: string): Boolean;
    function FindDirective(const Directive: string; var Value: string): Integer;
    property Lines: TStrings read FLines;
  public
    constructor Create(const AFileName: string; AIDEName: TIDEName);
    destructor Destroy; override;
    property FileName: string read FFileName;
  end;

  TDpkReader = class(TDpkFiler)
  strict protected
    procedure ReadPasFiles(PasFiles: TPasIncludeFiles);
    procedure ReadRequires(Requires: TStrings);
    procedure ReadDcrFiles(DcrFiles: TIncludeFiles);
  public
    procedure ReadData(Data: TDpkData);
  end;

  TDpkWriter = class(TDpkFiler)
  strict private
    procedure UpdateDirective(const Directive: string; const Value: string = '');
    procedure RemoveDirective(const Directive: string);
    function NextDirectiveIndex: Integer;
    function CalculateContainsIndex: Integer;
    function CalculateRequiresIndex: Integer;
    function DeleteSection(const Section: string): Integer; // returns the position of the deleted section, if existed
  public
    class procedure GenerateEmptyFile(const TargetDpkFile: string; IDEName: TIDEName; DeleteExisting: Boolean);
  public
    procedure UpdatePackageName(const PackageName: string);
    procedure UpdateDescription(const Description: string);
    procedure UpdateUsage(const Usage: TPackageUsage);
    procedure UpdateDllSuffix(const Value: string);
    procedure UpdateExplicitRebuild(ExplicitRebuild: boolean);

    procedure UpdatePasFiles(PasFiles: TPasIncludeFiles);
    procedure UpdateRequires(Requires: TStrings);
    procedure UpdateDcrFiles(DcrFiles: TIncludeFiles);

    procedure Flush;
  end;

implementation

uses
  System.Zip, UTmsBuildSystemUtils;

{ TDpkData }

constructor TDpkData.Create;
begin
  inherited Create;
  FDcrFiles := TIncludeFiles.Create('.dcr');
  FPasFiles := TPasIncludeFiles.Create('.pas');
  FRequires := TStringList.Create;
end;

destructor TDpkData.Destroy;
begin
  FDcrFiles.Free;
  FPasFiles.Free;
  FRequires.Free;
  inherited;
end;

{ TDpkWriter }

//function TDpkWriter.BuildIncludeText(AInclude: TPasIncludeFile): string;
//begin
//  Result := Format('%s in ''%s''',
//    [TPath.GetFileNameWithoutExtension(AInclude.FileName),
//     AInclude.FileName]);
//  if AInclude.FormName <> '' then
//    Result := Result + ' {' + AInclude.FormName + '}';
//end;

//destructor TDpkWriter.Destroy;
//begin
//  FPack.Free;
//  inherited;
//end;

//procedure TDpkWriter.WriteData(Data: TPackageWriteData);
//var
//  sw: TStreamWriter;
//  i: integer;
//begin
//  sw := TStreamWriter.Create(Pack.DpkFileName);
//  try
//    sw.WriteLine('{$R *.res}');
//    for var DcrFile in Data.DcrFiles do
//      sw.WriteLine(Format('{$R ''%s''}', [DcrFile.FileName]));
//
//    sw.WriteLine('');
//    sw.WriteLine('requires');
//    for i := 0 to Data.DcpFiles.Count - 1 do
//    begin
//      sw.Write('  ' + TPath.GetFileNameWithoutExtension(Data.DcpFiles[i].FileName));
//      if i = Data.DcpFiles.Count - 1 then
//        sw.WriteLine(';')
//      else
//        sw.WriteLine(',');
//    end;
//
//    if Data.PasFiles.Count > 0 then
//    begin
//      sw.WriteLine;
//      sw.WriteLine('contains');
//      for i := 0 to Data.PasFiles.Count - 1 do
//      begin
//        if (i > 0) then
//          sw.WriteLine(',');
//        sw.Write('  ' + BuildIncludeText(Data.PasFiles[I]));
//      end;
//      sw.WriteLine(';');
//    end;
////  finally
//    FreeAndNil(sw);
//  end;
//end;

{ TDpkFiler }

constructor TDpkFiler.Create(const AFileName: string; AIDEName: TIDEName);
begin
  inherited Create;
  FFileName := AFileName;
  FIDEName := AIDEName;
  FLines := TStringList.Create;
  if TFile.Exists(AFileName) then
    FLines.LoadFromFile(AFileName);
end;

destructor TDpkFiler.Destroy;
begin
  FLines.Free;
  inherited;
end;

function TDpkFiler.FindDirective(const Directive: string; var Value: string): Integer;
begin
  for var I := 0 to Lines.Count - 1 do
    if IsDirective(Lines[I], Directive, Value) then
      Exit(I);
  Result := -1;
end;

function TDpkFiler.IndexOf(const Value: string): Integer;
begin
  for var I := 0 to Lines.Count - 1 do
    if SameText(Lines[I].Trim, Value) then
      Exit(I);
  Result := -1;
end;

function TDpkFiler.IndexOfStart(const Value: string): Integer;
begin
  for var I := 0 to Lines.Count - 1 do
    if Lines[I].Trim.StartsWith(Value, True) then
      Exit(I);
  Result := -1;
end;

function TDpkFiler.IsDirective(const Line, Directive: string; var Value: string): Boolean;
begin
  Result := False;
  var TrimLine := Line.Trim;
  if TrimLine.StartsWith('{$' + Directive, True) and TrimLine.EndsWith('}') then
  begin
    TrimLine := Copy(TrimLine, Length(Directive) + 3, Length(TrimLine) - 3 - Length(Directive));
    if TrimLine = '' then
    begin
      Value := '';
      Exit(True);
    end;

    if TrimLine[1] <> #32 then Exit(False);

    Value := TrimLine.Trim;
    Result := True;
  end;
end;

function TDpkFiler.ReadSectionItems(const Section: string): TArray<string>;
begin
  SetLength(Result, 0);
  var Index := IndexOf(Section);
  if Index < 0 then Exit;

  Inc(Index);
  while Index < Lines.Count do
  begin
    var Line := Trim(Lines[Index]);
    try
    if Line = '' then Continue;
    if Line.StartsWith('{') or Line.StartsWith('//') then Continue;


    var LastChar: Char := Line[Length(Line)];
    Result := Result + [Copy(Line, 1, Length(Line) - 1)];

    case LastChar of
      ',': ;
      ';': Exit;
    else
      raise Exception.CreateFmt('Unexpected last char %s in line %d for section %s (%s)', [LastChar, Index, Section, FileName]);
    end;
    finally
      Inc(Index);
    end;
  end;
end;

{ TDpkReader }

procedure TDpkReader.ReadData(Data: TDpkData);
begin
  ReadPasFiles(Data.PasFiles);
  ReadRequires(Data.Requires);
  ReadDcrFiles(Data.DcrFiles);

  Data.PackageName := TPath.GetFileNameWithoutExtension(FileName);

  // Read description
  var Value: string;
  if FindDirective('DESCRIPTION', Value) >= 0 then
    Data.Description := Value.DeQuotedString('''');

  // Read usage
  if (FindDirective('RUNONLY', Value) >= 0) and not SameText(Value, 'OFF') then
    Data.Usage := TPackageUsage.Runtime
  else
  if (FindDirective('DESIGNONLY', Value) >= 0) and not SameText(Value, 'OFF')  then
    Data.Usage := TPackageUsage.DesignTime
  else
    Data.Usage := TPackageUsage.RuntimeAndDesignTime;

  // Read implicit build
  if (FindDirective('IMPLICITBUILD', Value) >= 0) and SameText(Value, 'OFF') then
    Data.ExplicitRebuild := True
  else
    Data.ExplicitRebuild := False;

  // Read libx suffix
  if (FindDirective('LIBSUFFIX', Value) >= 0) then
    Data.LibSuffix := Value.DeQuotedString('''');
end;

procedure TDpkReader.ReadDcrFiles(DcrFiles: TIncludeFiles);
var
  Value: string;
begin
  for var Line in Lines do
  begin
    if IsDirective(Line, 'R', Value) then
    begin
      Value := Value.DeQuotedString('''');
      if Value.ToLower = '*.res' then Continue;

      // it will fail if it's not a .dcr file
      DcrFiles.Add(Value);
    end;
  end;
end;

procedure TDpkReader.ReadPasFiles(PasFiles: TPasIncludeFiles);
begin
  for var Item in ReadSectionItems('contains') do
  begin
    // parse contains line
    var idx := Pos(' in ', Item.ToLower);
    if (idx <= 0) then
      raise Exception.CreateFmt('Cannot find in keyword in contains section: %s (%s)', [Item, FileName]);
    var PasFileName := Trim(Copy(Item, idx + 4));

    if (PasFileName = '') or (PasFileName[1] <> '''') then
      raise Exception.CreateFmt('Invalid line in contains section: %s (%s)', [Item, FileName]);

    var iend := Pos('''', PasFileName, 2);

    PasFileName := Copy(PasFileName, 2, iend - 2);
    var PasFormName := '';

    idx := Pos('{', Item);
    iend := Pos('}', Item, idx);
    if (idx > 0) then
    begin
      if (iend <= idx + 1) then
        raise Exception.CreateFmt('Invalid line in contains section: %s (%s)', [Item, FileName]);
      PasFormName := Copy(Item, idx + 1, iend - idx - 1);
    end;

    PasFiles.Add(PasFileName, PasFormName);
  end;
end;

procedure TDpkReader.ReadRequires(Requires: TStrings);
begin
  for var Require in ReadSectionItems('requires') do
    Requires.Add(Require);
end;

{ TDpkWriter }

function TDpkWriter.CalculateContainsIndex: Integer;
begin
  var Index: Integer;
  Index := IndexOf('end.');
  if Index > 0 then Exit(Index);

  raise Exception.CreateFmt('Could not find a place to add the contains section (%s)', [FileName]);
end;

function TDpkWriter.CalculateRequiresIndex: Integer;
begin
  var Index: Integer;

  Index := IndexOf('contains');
  if Index > 0 then Exit(Index);

  Index := IndexOf('end.');
  if Index > 0 then Exit(Index);

  raise Exception.CreateFmt('Could not find a place to add the requires section (%s)', [FileName]);
end;

function TDpkWriter.DeleteSection(const Section: string): Integer;
begin
  var Index := IndexOf(Section);
  if Index < 0 then Exit(-1);

  Result := Index;
  Lines.Delete(Index);
  while Index < Lines.Count do
  begin
    var Line := Trim(Lines[Index]);
    Lines.Delete(Index);

    // check if it was last line
    if (Line <> '') and (Line[Length(Line)] = ';') then
    begin
      // if next line is empty, delete it as well
      if Lines[Index].Trim = '' then
        Lines.Delete(Index);
      Exit;
    end;
  end;
end;

procedure TDpkWriter.Flush;
begin
  Lines.SaveToFile(FileName);
end;

class procedure TDpkWriter.GenerateEmptyFile(const TargetDpkFile: string; IDEName: TIDEName; DeleteExisting: Boolean);
var
  EmptyDpkFile: TBytes;
  Writer: TDpkWriter;
begin
  var PackageName := TPath.GetFileNameWithoutExtension(TargetDpkFile);
  var TargetFolder := TPath.GetDirectoryName(TargetDpkFile);

  // Load default empty files
  begin
    var Res := TResourceStream.Create(HInstance, 'EMPTY_PACKAGES_ZIP', RT_RCDATA);
    try
      var Zip := TZipFile.Create;
      try
        Zip.Open(Res, zmRead);
        Zip.Read(Format('%s/Empty.dpk', [IDEId[IDEName]]), EmptyDpkFile);
      finally
        Zip.Free;
      end;
    finally
      Res.Free;
    end;
  end;

  if TFile.Exists(TargetDpkFile) then
    if not DeleteExisting then
      raise Exception.CreateFmt('Cannot create package file "%s.dpk" in folder "%s": File already exists', [PackageName, TargetFolder]);

  TDirectory_CreateDirectory(TargetFolder);

  // Create based on an empty package
  TFile.WriteAllBytes(TargetDpkFile, EmptyDpkFile);

  // Now replace initial info in the project that
  Writer := TDpkWriter.Create(TargetDpkFile, IDEName);
  try
    Writer.UpdatePackageName(PackageName);
    Writer.Flush;
  finally
    Writer.Free;
  end;
end;

function TDpkWriter.NextDirectiveIndex: Integer;
begin
  var InDirective := False;
  for var I := 0 to Lines.Count - 1 do
  begin
    if Lines[I].StartsWith('{$') then
      InDirective := True
    else
    if InDirective then
      Exit(I);
  end;

  var Index: Integer;
  Index := IndexOf('requires') - 1;
  if Index > 0 then Exit(Index);

  Index := IndexOf('contains') - 1;
  if Index > 0 then Exit(Index);

  Index := IndexOf('end.') - 1;
  if Index > 0 then Exit(Index);

  raise Exception.CreateFmt('Could not find a place to add a new directive (%s)', [FileName]);
end;

procedure TDpkWriter.RemoveDirective(const Directive: string);
begin
  var Dummy: string;
  var Index := FindDirective(Directive, Dummy);
  if Index >= 0 then
     Lines.Delete(Index);
end;

procedure TDpkWriter.UpdateDcrFiles(DcrFiles: TIncludeFiles);
begin
  // Delete all .dcr includes (and $R directive that includes a .dcr file)
  var Index := 0;
  while Index < Lines.Count - 1 do
  begin
    var Value: string;
    if IsDirective(Lines[Index], 'R', Value) and (ExtractFileExt(Value.DeQuotedString('''')).ToLower = '.dcr') then
      Lines.Delete(Index)
    else
      Inc(Index);
  end;

  // Find the next dcr index
  Index := IndexOf('{$R *.res}') + 1;
  if Index = 0 then
  begin
    Index := IndexOfStart('{$');
    if Index = -1 then
      Index := 2; // 3rd line of the file
  end;

  // Add all DCR at the position
  for var DcrFile in DcrFiles do
  begin
    Lines.Insert(Index, Format('{$R ''%s''}', [DcrFile.FileName]));
    Inc(Index);
  end;
end;

procedure TDpkWriter.UpdateDescription(const Description: string);
begin
  if Description <> '' then
    UpdateDirective('DESCRIPTION', Description.QuotedString(''''))
  else
    RemoveDirective('DESCRIPTION');
end;

procedure TDpkWriter.UpdateDirective(const Directive, Value: string);
begin
  var Dummy: string;
  var NewLine := '';
  if Value <> '' then
    NewLine := Format('{$%s %s}', [Directive, Value])
  else
    NewLine := Format('{$%s}', [Directive]);

  var Index := FindDirective(Directive, Dummy);
  if Index >= 0 then
     Lines[Index] := NewLine
  else
  begin
    Index := NextDirectiveIndex;
    Lines.Insert(Index, NewLine);
  end;
end;

procedure TDpkWriter.UpdateDllSuffix(const Value: string);
begin
  if Value <> '' then
  begin
    if SameText(Value, 'auto') or SameText(Value, '$(auto)') then
      UpdateDirective('LIBSUFFIX', 'AUTO')
    else
      UpdateDirective('LIBSUFFIX', Value.QuotedString(''''))
  end
  else
    UpdateDirective('LIBSUFFIX');
end;

procedure TDpkWriter.UpdateExplicitRebuild(ExplicitRebuild: boolean);
begin
  if ExplicitRebuild then
    UpdateDirective('IMPLICITBUILD', 'OFF')
  else
    UpdateDirective('IMPLICITBUILD', 'ON')
end;

procedure TDpkWriter.UpdatePackageName(const PackageName: string);
begin
  var Index := IndexOfStart('package ');
  if Index < 0 then
    raise Exception.CreateFmt('Could not find package keyword (%s)', [FileName]);
  Lines[Index] := Format('package %s;', [PackageName]);
end;

procedure TDpkWriter.UpdatePasFiles(PasFiles: TPasIncludeFiles);
begin
  var Index := DeleteSection('contains');
  if Index = -1 then
  begin
    if PasFiles.Count = 0 then Exit;
    Index := CalculateContainsIndex;
  end;

  Lines.Insert(Index, 'contains');
  Inc(Index);

  for var I := 0 to PasFiles.Count - 1 do
  begin
    var PasFile := PasFiles[I];
    var LineContent := Format('%s in ''%s''', [TPath.GetFileNameWithoutExtension(PasFile.FileName), PasFile.FileName]);
    if PasFile.FormName <> '' then
      LineContent := LineContent + ' {' + PasFile.FormName + '}';
    if I < PasFiles.Count - 1 then
      LineContent := LineContent + ','
    else
      LineContent := LineContent + ';';
    Lines.Insert(Index, '  ' + LineContent);
    Inc(Index);
  end;
  Lines.Insert(Index, '');
end;

procedure TDpkWriter.UpdateRequires(Requires: TStrings);
begin
  var Index := DeleteSection('requires');
  if Index = -1 then
    Index := CalculateRequiresIndex;

  Lines.Insert(Index, 'requires');
  Inc(Index);

  for var I := 0 to Requires.Count - 1 do
  begin
    var LineContent := Requires[I];
    if I < Requires.Count - 1 then
      LineContent := LineContent + ','
    else
      LineContent := LineContent + ';';
    Lines.Insert(Index, '  ' + LineContent);
    Inc(Index);
  end;
  Lines.Insert(Index, '');
end;

procedure TDpkWriter.UpdateUsage(const Usage: TPackageUsage);
begin
  case Usage of
    TPackageUsage.Runtime:
      begin
        RemoveDirective('DESIGNONLY');
        UpdateDirective('RUNONLY');
      end;
    TPackageUsage.DesignTime:
      begin
        UpdateDirective('DESIGNONLY');
        RemoveDirective('RUNONLY');
      end;
  else
    RemoveDirective('DESIGNONLY');
    RemoveDirective('RUNONLY');
  end;
end;

end.

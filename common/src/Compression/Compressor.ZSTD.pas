unit Compressor.ZSTD;
{$i ../tmscommon.inc}

interface
uses Classes, SysUtils, Generics.Collections;

type
  TTarFile = record
    NameOnDisk: string;
    NameOnTar: string;
    constructor Create(const aNameOnDisk, aNameOnTar: string);
  end;

  TZSTDCompressor = class
  private
  public
    class procedure Tar(const Files: TList<TTarFile>; const Dest: TStream; const Feedback: TProc<string> = nil); overload; static;
    class procedure Tar(const SourceFolder: String; const Dest: TStream; const Feedback: TProc<string> = nil); overload; static;
    class procedure Compress(const Source, Dest: TStream; const Level: integer; const MultiThread: boolean = false); static;
    class procedure TarAndCompress(const SourceFolder: String; const Dest: string; const Level: integer; const MultiThread: boolean = false; const Feedback: TProc<string> = nil); overload; static;
    class procedure TarAndCompress(const Files: TList<TTarFile>; const Dest: string; const Level: integer; const MultiThread: boolean = false; const Feedback: TProc<string> = nil); overload; static;
  end;

implementation
uses ZSTDLib, ZSTD,  IOUtils, Generics.Defaults, StrUtils, TarCommon, TarWriter, Threading;

type
  TTarSourceAndDestination = record
  public
    Source: TTarFileInfo;
    Destination: string;
    constructor Create(const aSource: TTarFileInfo; const aDestination: string);
  end;

function CreateCompressStream(const Dest: TStream; const Level: integer; const MultiThread: boolean): TStream;
begin
  var Options: TZSTDCompressOptions;
  Options.Init;
  Options.CompressionLevel := Level;
  //Options.Strategy := Integer(ZSTD_btultra2);
  if MultiThread then Options.Workers := TThread.ProcessorCount else Options.Workers := 0;
 // Options.EnableLongDistanceMatching := true;
  Result := TZSTDCompressStream.Create(Dest, Options);

end;

procedure ProvideFeedback(const Feedback: TProc<string>; const WorkDone: integer; const WorkTotal: integer);
begin
  if not Assigned(Feedback) then exit;
  Feedback(IntToStr(Round(Double(WorkDone) / Double(WorkTotal) * 100.0)) + '%');
end;


class procedure TZSTDCompressor.Compress(const Source, Dest: TStream; const Level: integer; const MultiThread: boolean = false);
begin
  var Compressor := CreateCompressStream(Dest, Level, MultiThread);
  try
    Compressor.CopyFrom(Source);
  finally
    Compressor.Free;
  end;
end;

procedure ReadFilesAndFolders(const SourceFolder, RelDestFolder: string; const Files: TList<TTarSourceAndDestination>);
var
  F: SysUtils.TSearchRec;
begin
  if SysUtils.FindFirst(TPath.Combine(SourceFolder, '*'), faAnyFile, F) = 0 then
  begin
    try
      repeat
        var FileName := TPath.Combine(SourceFolder, F.Name);
        var NameOnTar := TPath.Combine(RelDestFolder, FileName);
        if (F.Attr and faDirectory <> 0) then
        begin
          if (F.Name <> '.') and (F.Name <> '..') then
          begin
            Files.Add(TTarSourceAndDestination.Create(TTarFileInfo.Create(FileName, F), NameOnTar));
            ReadFilesAndFolders(FileName, TPath.Combine(RelDestFolder, F.Name), Files);
          end;
        end else
        begin
          Files.Add(TTarSourceAndDestination.Create(TTarFileInfo.Create(FileName, F), NameOnTar));
        end;
      until SysUtils.FindNext(F) <> 0;
    finally
      SysUtils.FindClose(F);
    end;
  end;
end;

//Improve sorting by putting all related-filenames together. 
//This reduces the size of webcore from 200 to 193mb in my tests, but of course might change depending in the particular files.
//In powershell, to get the extensions of a folder:  Get-Childitem -Recurse -file | Select-Object Extension -Unique
function GetSortFileName(const FileName: string): string;
begin
  //note that this will be reversed. 111. (reversed .111) would sort before .900 (reversed .009)
  var ext := TPath.GetExtension(FileName).ToLowerInvariant;
  //binaries
  if (ext = '.exe')
    or (ext = '.bpl')
    or (ext = '.dll')
    or (ext = '')
    or (ext = '.dylib')
    or (ext = '.so')
    then exit (FileName + '.100');

 //those seem to make it worse.
 { if (ext = '.o')
    or (ext = '.obj')
    or (ext = '.lib') //libs can be mostly text (import libraries) or be binary. Sadly we can't know.
    or (ext = '.a') //same for .a
    then exit (FileName + '.200');

  //mostly text
   if (ext = '.bpi')
    or (ext = '.dcp')
    then exit (FileName + '.300');  }

  //text
  if (ext = '.pas')
    or (ext = '.inc')
    or (ext = '.h')
    or (ext = '.cpp')
    or (ext = '.txt')
    or (ext = '.json')
    or (ext = '.xml')
    or (ext = '.xsd')
    or (ext = '.yaml')
    or (ext = '.dfm')
    or (ext = '.lfm')
    or (ext = '.dpr')
    or (ext = '.dproj')
    or (ext = '.cbproj')
    or (ext = '.lpr')
    or (ext = '.js')
    or (ext = '.html')
    or (ext = '.htm')
    or (ext = '.css')
    or (ext = '.rtf')
    or (ext = '.groupproj')
    then exit (FileName + '.400');

  //zip
  if (ext = '.png')
    or (ext = '.zip')
    or (ext = '.zlib')
    or (ext = '.pdf')
    or (ext = '.chm')
    or (ext = '.xlsx')
    then exit (FileName + '.899');

  //non-compressible
  if (ext = '.jpg')
    or (ext = '.jpeg')
    or (ext = '.gif')
    or (ext = '.ico')
    or (ext = '.7z')
    then exit (FileName + '.999');

  //non-compressible files at the end.
  exit (FileName + '.500');


end;

procedure SortFilesAndFolders(var Items: TList<TTarSourceAndDestination>);
begin
  Items.Sort(TDelegatedComparer<TTarSourceAndDestination>.Construct(
    function(const Left, Right: TTarSourceAndDestination): integer
    begin
      if Left.Source.IsFolder and not Right.Source.IsFolder then exit(-1);
      if Right.Source.IsFolder and not Left.Source.IsFolder then exit(1);
      if Left.Source.IsFolder then exit(IComparer<string>(TIStringComparer.Ordinal).Compare(Left.Source.FileName, Right.Source.FileName)); //we want c:\a is before c:\a\b.

      //reversed filename order in files, so they are sorted by extension.
      var tfeL := ReverseString(GetSortFileName(Left.Source.FileName));
      var thgiR := ReverseString(GetSortFileName(Right.Source.FileName));
      Result := IComparer<string>(TIStringComparer.Ordinal).Compare(tfeL, thgiR);
    end
  ));

end;

function GetSortedFilesAndFolders(const SourceFolder: string): TArray<TTarSourceAndDestination>;
begin
  var Items := TList<TTarSourceAndDestination>.Create;
  try
    ReadFilesAndFolders(SourceFolder, '', Items);
    SortFilesAndFolders(Items);
    Result := Items.ToArray;
  finally
    Items.Free;
  end;
end;

class procedure TZSTDCompressor.Tar(const SourceFolder: String; const Dest: TStream; const Feedback: TProc<string> = nil);
begin
  var Tar := TTarWriter.Create(Dest, true);
  try
    var Files := GetSortedFilesAndFolders(SourceFolder);

    var WorkDone := 0;
    for var F in Files do
    begin
      ProvideFeedback(Feedback, WorkDone, Length(Files));
      Tar.AddItem(F.Source, F.Destination);
      Inc(WorkDone);
    end;

  finally
    Tar.Free;
  end;

end;

function GetTarFileInfo(const FileName: string): TTarFileInfo;
begin
  var Rec: TSearchRec;
  if SysUtils.FindFirst(FileName, faNormal or faDirectory, Rec) = 0 then
  begin
    Result := TTarFileInfo.Create(FileName, Rec);
    FindClose(Rec);
  end;
end;

class procedure TZSTDCompressor.Tar(const Files: TList<TTarFile>; const Dest: TStream; const Feedback: TProc<string> = nil);
begin
  var SortedFiles := TList<TTarSourceAndDestination>.Create;
  try
    SortedFiles.Capacity := Files.Count * 2;
    var ExistingFolders := THashSet<string>.Create;
    try
      var ExistingFiles := THashSet<string>.Create;
      try
        for var F in Files do
        begin
          if ExistingFiles.Contains(F.NameOnTar) then continue;

          SortedFiles.Add(TTarSourceAndDestination.Create(GetTarFileInfo(F.NameOnDisk), F.NameOnTar));
          ExistingFiles.Add(F.NameOnTar);

          var FolderOnTar := TPath.GetDirectoryName(F.NameOnTar);
          if FolderOnTar = '' then continue;

          var Folder := TPath.GetDirectoryName(F.NameOnDisk);
          if not ExistingFolders.Contains(FolderOnTar) then
          begin
            SortedFiles.Add(TTarSourceAndDestination.Create(GetTarFileInfo(Folder), FolderOnTar));
            ExistingFolders.Add(FolderOnTar);
          end;
        end;
      finally
        ExistingFiles.Free;
      end;
    finally
      ExistingFolders.Free;
    end;

    SortFilesAndFolders(SortedFiles);
    var Tar := TTarWriter.Create(Dest, true);
    try
      var WorkDone := 0;
      for var F in SortedFiles do
      begin
        ProvideFeedback(Feedback, WorkDone, SortedFiles.Count);
        Tar.AddItem(F.Source, F.Destination);
        inc(WorkDone);
      end;
    finally
      Tar.Free;
    end;
  finally
    SortedFiles.Free;
  end;
end;

class procedure TZSTDCompressor.TarAndCompress(const SourceFolder: String; const Dest: string; const Level: integer; const MultiThread: boolean = false; const Feedback: TProc<string> = nil);
begin
  var FinalStream := TFileStream.Create(Dest, fmCreate);
  try
    var CmpStream := CreateCompressStream(FinalStream, Level, MultiThread);
    try
      Tar(SourceFolder, CmpStream, Feedback);
    finally
      CmpStream.Free;
    end;
  finally
    FinalStream.Free;
    end;
end;

class procedure TZSTDCompressor.TarAndCompress(const Files: TList<TTarFile>;
  const Dest: string; const Level: integer; const MultiThread: boolean = false; const Feedback: TProc<string> = nil);
begin
  var FinalStream := TFileStream.Create(Dest, fmCreate);
  try
    var CmpStream := CreateCompressStream(FinalStream, Level, MultiThread);
    try
      Tar(Files, CmpStream, Feedback);
    finally
      CmpStream.Free;
    end;
  finally
    FinalStream.Free;
    end;

end;


{ TTarFile }

constructor TTarFile.Create(const aNameOnDisk, aNameOnTar: string);
begin
  NameOnDisk := aNameOnDisk;
  NameOnTar := aNameOnTar;
end;

{ TTarSourceAndDestination }

constructor TTarSourceAndDestination.Create(const aSource: TTarFileInfo;
  const aDestination: string);
begin
  Source := aSource;
  Destination := aDestination;
end;

end.

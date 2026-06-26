unit UGenericDecompressor;

interface
uses Classes, SysUtils, Decompressor.ZSTD, Zip;
type
  TDownloadFormat = (Unknown, Zip, Zstd);
 const
  DownloadFormatExtension: Array[TDownloadFormat] of string =('', '.zip', '.tar.zst');
 type

  TBundleDecompressor = class
  private
    class procedure ExtractZipFile(const ZipFileName, ExtractFolder: string;
      const Skip: TFunc<string, boolean>; const Renamer: TFunc<string, string>); static;
    class function IsZipByLocalHeader(const FileName: string): Boolean; overload; static;
    class function IsZipByLocalHeader(Stream: TStream): Boolean; overload; static;
  public
    class function GetFileFormat(const FileName: string): TDownloadFormat; static;
    class procedure ExtractCompressedFile(const FileName, ExtractFolder: string;
      DownloadFormat: TDownloadFormat = TDownloadformat.Unknown; const Skip: TFunc<string, boolean> = nil; const Renamer: TFunc<string, string> = nil); static;
  end;

implementation
uses IOUtils, UTmsBuildSystemUtils, Commands.GlobalConfig;

class function TBundleDecompressor.IsZipByLocalHeader(Stream: TStream): Boolean;
var
  Pos0: Int64;
  B: array[0..3] of Byte;
begin
  Result := False;
  if Stream.Size - Stream.Position < 4 then Exit;

  Pos0 := Stream.Position;
  Stream.ReadBuffer(B, SizeOf(B));
  Result :=
    (B[0] = $50) and  // 'P'
    (B[1] = $4B) and  // 'K'
    (B[2] = $03) and
    (B[3] = $04);
  Stream.Position := Pos0;
end;

class function TBundleDecompressor.IsZipByLocalHeader(const FileName: string): Boolean;
var
  FS: TFileStream;
begin
  Result := False;
  if not FileExists(FileName) then Exit;

  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := IsZipByLocalHeader(FS);
  finally
    FS.Free;
  end;
end;

class function TBundleDecompressor.GetFileFormat(const FileName: string): TDownloadFormat;
begin
  if TZSTDDecompressor.IsValid(FileName) then exit(TDownloadFormat.Zstd);
  try
    if IsZipByLocalHeader(FileName) and TZipFile.IsValid(FileName) then exit(TDownloadFormat.Zip);
  except
    //It is not a zipfile. TZipFile.IsValid should have returned false, but sometimes it can raise an exception...
  end;
  Result := TDownloadFormat.Unknown;
end;

class procedure TBundleDecompressor.ExtractZipFile(const ZipFileName, ExtractFolder: string; const Skip: TFunc<string, boolean>; const Renamer: TFunc<string, string>);
begin
    var Zip := TZipFile.Create;
    try
      Zip.Open(ZipFileName, TZipMode.zmRead);
      for var i := 0 to Zip.FileCount - 1 do
      begin
        var EntryName := Zip.FileNames[i];
        if Assigned(Skip) and Skip(EntryName) then continue;
        if Assigned(Renamer) then EntryName := Renamer(EntryName);

        var FileName := TPath.Combine(ExtractFolder, EntryName);
        TZSTDDecompressor.CheckIsInside(FileName, ExtractFolder);

        if EntryName.EndsWith('/') or EntryName.EndsWith('\') then
        begin
          TDirectory_CreateDirectory(FileName);
          continue;
        end;

        DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, FileName);

        if not Assigned(Renamer) then
        begin
          Zip.Extract(i, ExtractFolder);
        end
        else
        begin
          var DestFolderName := TPath.GetDirectoryName(FileName);
          TDirectory_CreateDirectory(DestFolderName);
          Zip.Extract(i, DestFolderName, false);
        end;
      end;
    finally
      Zip.Free;
    end;


end;

class procedure TBundleDecompressor.ExtractCompressedFile(const FileName, ExtractFolder: string; DownloadFormat: TDownloadFormat; const Skip: TFunc<string, boolean>; const Renamer: TFunc<string, string>);
begin
  if DownloadFormat = TDownloadFormat.Unknown then DownloadFormat := GetFileFormat(FileName);

  case DownloadFormat of
    TDownloadFormat.Zip: ExtractZipFile(FileName, ExtractFolder, Skip, Renamer);
    TDownloadFormat.Zstd: TZSTDDecompressor.Decompress(FileName, ExtractFolder, Skip, Renamer, Config.Folders.LockedFilesFolder);
    else raise Exception.Create('Unknown bundle format in file: "' + FileName + '"');
  end;
end;


end.

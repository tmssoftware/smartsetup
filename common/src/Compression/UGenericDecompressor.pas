unit UGenericDecompressor;

interface
uses Classes, SysUtils, Decompressor.ZSTD, Zip;
type
  TDownloadFormat = (Unknown, Zip, Zstd);

  TBundleDecompressor = class
  private
    class procedure ExtractZipFile(const ZipFileName, ExtractFolder: string;
      const Skip: TFunc<string, boolean>); static;
  public
    class function GetFileFormat(const FileName: string): TDownloadFormat; static;
    class procedure ExtractCompressedFile(const FileName, ExtractFolder: string;
      DownloadFormat: TDownloadFormat = TDownloadformat.Unknown; const Skip: TFunc<string, boolean> = nil); static;
  end;

implementation
class function TBundleDecompressor.GetFileFormat(const FileName: string): TDownloadFormat;
begin
  if TZipFile.IsValid(FileName) then exit(TDownloadFormat.Zip);
  if TZSTDDecompressor.IsValid(FileName) then exit(TDownloadFormat.Zstd);
  Result := TDownloadFormat.Unknown;
end;

class procedure TBundleDecompressor.ExtractZipFile(const ZipFileName, ExtractFolder: string; const Skip: TFunc<string, boolean>);
begin
    var Zip := TZipFile.Create;
    try
      Zip.Open(ZipFileName, TZipMode.zmRead);
      for var i := 0 to Zip.FileCount - 1 do
      begin
        if Assigned(Skip) and Skip(Zip.FileNames[i]) then continue;
        Zip.Extract(Zip.FileNames[i], ExtractFolder);
      end;
    finally
      Zip.Free;
    end;


end;

class procedure TBundleDecompressor.ExtractCompressedFile(const FileName, ExtractFolder: string; DownloadFormat: TDownloadFormat; const Skip: TFunc<string, boolean>);
begin
  if DownloadFormat = TDownloadFormat.Unknown then DownloadFormat := GetFileFormat(FileName);

  case DownloadFormat of
    TDownloadFormat.Zip: ExtractZipFile(FileName, ExtractFolder, Skip);
    TDownloadFormat.Zstd: TZSTDDecompressor.Decompress(FileName, ExtractFolder, Skip);
    else raise Exception.Create('Unknown bundle format in file: "' + FileName + '"');
  end;
end;


end.

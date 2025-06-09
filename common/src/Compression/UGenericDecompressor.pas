unit UGenericDecompressor;

interface
uses Classes, SysUtils, Decompressor.ZSTD, Zip;
type
  TDownloadFormat = (Unknown, Zip, Zstd);

  TBundleDecompressor = class
  public
    class function GetFileFormat(const FileName: string): TDownloadFormat; static;
    class procedure ExtractCompressedFile(const FileName, ExtractFolder: string;
      DownloadFormat: TDownloadFormat = TDownloadformat.Unknown); static;
  end;

implementation
class function TBundleDecompressor.GetFileFormat(const FileName: string): TDownloadFormat;
begin
  if TZipFile.IsValid(FileName) then exit(TDownloadFormat.Zip);
  if TZSTDDecompressor.IsValid(FileName) then exit(TDownloadFormat.Zstd);
  Result := TDownloadFormat.Unknown;
end;

class procedure TBundleDecompressor.ExtractCompressedFile(const FileName, ExtractFolder: string; DownloadFormat: TDownloadFormat);
begin
  if DownloadFormat = TDownloadFormat.Unknown then DownloadFormat := GetFileFormat(FileName);

  case DownloadFormat of
    TDownloadFormat.Zip: TZipFile.ExtractZipFile(FileName, ExtractFolder);
    TDownloadFormat.Zstd: TZSTDDecompressor.Decompress(FileName, ExtractFolder);
    else raise Exception.Create('Unknown bundle format in file: "' + FileName + '"');
  end;
end;


end.

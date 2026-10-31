unit Downloads.FileNameManager;
{$i ../../tmssetup.inc}

interface
uses Classes, SysUtils, Deget.Version;

type
  TDownloadFileName = record
  private
  const
    Separator = '_production_';
    FreeFormSeparator = '@';

    class function TryGetFileNameWithoutExtension(const FileName: string; const Extension: string; out FileNameWithoutExtension: string): boolean; overload; static;
    class function GetFileNameWithoutExtension(const FileName: string): string; static;
  public
    class function ExtractProduct(const FileName: string): string; static;
    class function ExtractVersion(const FileName: string): TLenientVersion; static;
    class function GenerateRootFileName(const ProductId: string): string; static;
    class function GenerateFileName(const ProductId: string; const Version: TLenientVersion): string; static;
    class function TryGetFileNameWithoutExtension(const FileName: string; out FileNameWithoutExtension: string): boolean; overload; static;
    class function TryParse(const FileName: string; out Product: string; out Version: TLenientVersion): boolean; static;
  end;

implementation
uses IOUtils, System.NetEncoding;

{ TDownloadFileName }

class function TDownloadFileName.GenerateRootFileName(const ProductId: string): string;
begin
//   Channel is hard-coded (production), as there is no channel information about latest version
  Result := ProductId + Separator;
end;

class function TDownloadFileName.GenerateFileName(const ProductId: string;
  const Version: TLenientVersion): string;
begin
  case Version.VersionType of
    TVersionType.Semantic:
       Result := GenerateRootFileName(ProductId) + Version.Normalized;
    else
       Result := GenerateRootFileName(ProductId) + FreeFormSeparator + TNetEncoding.Base64String.Encode(Version.ToString).Replace('/', '#'); //We could use Base64Url.Encode, which replaces '/' with '_', but then '_' can create a fake '_production_' separator.
  end;
end;

class function TDownloadFileName.TryGetFileNameWithoutExtension(const FileName,
  Extension: string; out FileNameWithoutExtension: string): boolean;
begin
  FileNameWithoutExtension := '';
  Result := false;

  if FileName.EndsWith(Extension, true) then
  begin
    FileNameWithoutExtension := TPath.GetFileName(FileName.Substring(0, Length(FileName) - Extension.Length));
    exit(true);
  end;
end;

class function TDownloadFileName.TryGetFileNameWithoutExtension(
  const FileName: string; out FileNameWithoutExtension: string): boolean;
begin
  //TPath.GetFileNameWithoutExtension can't manage '.tar.zst' files.
  if TryGetFileNameWithoutExtension(FileName, '.zip', FileNameWithoutExtension) then exit(true);
  if TryGetFileNameWithoutExtension(FileName, '.tar.zst', FileNameWithoutExtension) then exit(true);
  FileNameWithoutExtension := '';
  Result := false;
end;

class function TDownloadFileName.GetFileNameWithoutExtension(
  const FileName: string): string;
begin
  if not TryGetFileNameWithoutExtension(FileName, Result)
    then raise Exception.Create('Only .tar.zst and .zip extensions are allowed.');
end;

class function TDownloadFileName.TryParse(const FileName: string; out Product: string; out Version: TLenientVersion): boolean;
begin
  if FileName.IndexOf(Separator) <= 0 then exit(false);
  try
    Version := ExtractVersion(FileName);
    Product := ExtractProduct(FileName);
  except
    exit(false);
  end;
  Result := true;
end;

class function TDownloadFileName.ExtractProduct(
  const FileName: string): string;
begin
  var OnlyFileName := GetFileNameWithoutExtension(FileName);
  var SeparatorIndex := OnlyFileName.LastIndexOf(Separator);
  if (SeparatorIndex <= 0) then raise Exception.Create('Invalid filename: "' + FileName + '". It must include "' + Separator + '".');

  Result := OnlyFileName.Substring(0, SeparatorIndex);

end;

class function TDownloadFileName.ExtractVersion(const FileName: string): TLenientVersion;
begin
  var OnlyFileName := GetFileNameWithoutExtension(FileName);
  var SeparatorIndex := OnlyFileName.LastIndexOf(Separator);
  if (SeparatorIndex <= 0) then raise Exception.Create('Invalid filename: "' + FileName + '". It must include "' + Separator + '".');

  var VerStr := OnlyFileName.Substring(SeparatorIndex + Separator.Length);
  if VerStr.StartsWith(FreeFormSeparator)
    then Result := TLenientVersion.Create(TNetEncoding.Base64String.Decode(VerStr.Substring(Length(FreeFormSeparator)).Replace('#', '/')), TVersionType.FreeForm)
    else Result := TLenientVersion.Create(VerStr, TVersionType.Semantic);


end;

end.

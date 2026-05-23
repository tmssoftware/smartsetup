unit Downloads.FileNameManager;
{$i ../../tmssetup.inc}

interface
uses Classes, SysUtils, Deget.Version;

type
  TDownloadFileName = record
  private
  const
    Separator = '_production_';
    class function TryGetFileNameWithoutExtension(const FileName: string; const Extension: string; out FileNameWithoutExtension: string): boolean; static;
    class function GetFileNameWithoutExtension(const FileName: string): string; static;
  public
    class function ExtractProduct(const FileName: string): string; static;
    class function ExtractVersion(const FileName: string): string; static;
    class function GenerateRootFileName(const ProductId: string): string; static;
    class function GenerateFileName(const ProductId: string; const Version: TVersion): string; static;
  end;

implementation
uses IOUtils;

{ TDownloadFileName }

class function TDownloadFileName.GenerateRootFileName(const ProductId: string): string;
begin
//   Channel is hard-coded (production), as there is no channel information about latest version
  Result := ProductId + Separator;
end;

class function TDownloadFileName.GenerateFileName(const ProductId: string;
  const Version: TVersion): string;
begin
  Result := GenerateRootFileName(ProductId) + Version.Normalized;
end;

class function TDownloadFileName.TryGetFileNameWithoutExtension(const FileName,
  Extension: string; out FileNameWithoutExtension: string): boolean;
begin
  FileNameWithoutExtension := '';
  Result := false;

  if FileName.EndsWith(Extension, true) then
  begin
    FileNameWithoutExtension := FileName.Substring(0, Length(FileName) - Extension.Length);
    exit(true);
  end;
end;

class function TDownloadFileName.GetFileNameWithoutExtension(
  const FileName: string): string;
begin
  //TPath.GetFileNameWithoutExtension can't manage '.tar.zst' files.
  if TryGetFileNameWithoutExtension(FileName, '.zip', Result) then exit;
  if TryGetFileNameWithoutExtension(FileName, '.tar.zst', Result) then exit;
  raise Exception.Create('Only .tar.zst and .zip extensions are allowed.');
end;

class function TDownloadFileName.ExtractProduct(
  const FileName: string): string;
begin
  var OnlyFileName := GetFileNameWithoutExtension(FileName);
  var SeparatorIndex := OnlyFileName.LastIndexOf(Separator);
  if (SeparatorIndex <= 0) then raise Exception.Create('Invalid filename: "' + FileName + '". It must include "' + Separator + '".');

  Result := OnlyFileName.Substring(0, SeparatorIndex);

end;

class function TDownloadFileName.ExtractVersion(const FileName: string): string;
begin
  var OnlyFileName := GetFileNameWithoutExtension(FileName);
  var SeparatorIndex := OnlyFileName.LastIndexOf(Separator);
  if (SeparatorIndex <= 0) then raise Exception.Create('Invalid filename: "' + FileName + '". It must include "' + Separator + '".');
  Result := OnlyFileName.Substring(SeparatorIndex + 1);
end;

end.

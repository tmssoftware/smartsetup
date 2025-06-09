unit Decompressor.ZSTD;
{$i ../tmscommon.inc}

interface
uses Classes, SysUtils;
type
  TZSTDDecompressor = class
  private
    class procedure CheckIsInside(const FileName, Folder: string); static;
  public
    class procedure Decompress(const FileName, DestFolder: string);
    class function IsValid(const FileName: string): boolean; overload;
    class function IsValid(const Stream: TStream): boolean; overload;
  end;

implementation
uses ZSTD, TarCommon, TarReader, IOUtils, UTmsBuildSystemUtils, UMultiLogger, DateUtils;
{ TZSTDDecompressor }

class procedure TZSTDDecompressor.CheckIsInside(const FileName, Folder: string);
begin
 var FullFolder := TPath.GetFullPath(Folder);
 var FullFileName := TPath.GetFullPath(FileName);
 if not FullFileName.StartsWith(FullFolder, false) then raise Exception.Create('File "' + FileName + '" is outside the extract folder "' + FullFolder + '"');
end;

class procedure TZSTDDecompressor.Decompress(const FileName, DestFolder: string);
begin
  var LastUpdate: TDateTime := Now - 1;
  var InStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    var Total := InStream.Size;
    Logger.SetPercentAction(
      function: integer
      begin
        var Processed := InStream.Position;
        if (Total = 0) or (Processed > Total) then
          Exit(-1);
        Result := (Processed * 100) div Total;
      end);
    try
      var DecompressStream := TZSTDDecompressStream.Create(InStream);
      try
        var Tar := TTarReader.Create(DecompressStream);
        try
          var DirRec: TTarFileInfo;
          while Tar.FindNext(DirRec) do
          begin
            var DestFileName := TPath.Combine(DestFolder, String(DirRec.FileName));
            CheckIsInside(DestFileName, DestFolder);

            if (DirRec.Attr and faDirectory) <> 0 then
            begin
              TDirectory_CreateDirectory(DestFileName);
              TDirectory.SetCreationTime(DestFileName, DirRec.TimeStamp);
              TDirectory.SetLastWriteTime(DestFileName, DirRec.TimeStamp);
            end
            else if (DirRec.Attr and faNormal) <> 0 then
            begin
              if MilliSecondsBetween(Now, LastUpdate) > 100 then
              begin
                Logger.Info('Decompressing ' + TPath.GetFileName(DestFileName));
                LastUpdate := now;
              end;
              Tar.ReadFile(DestFileName, DirRec.Size, DirRec.TimeStamp);
            end
            else raise Exception.Create('Internal error reading tar file');
          end;
        finally
          Tar.Free;
        end;
      finally
        DecompressStream.Free;
      end;
    finally
      Logger.ResetPercentAction;
    end;

  finally
    InStream.Free;
  end;
end;

class function TZSTDDecompressor.IsValid(const FileName: string): boolean;
begin
  var Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := IsValid(Stream);
  finally
    Stream.Free;
  end;
end;

class function TZSTDDecompressor.IsValid(const Stream: TStream): boolean;
const
  ZSTDHeader: array[0..3] of byte = ($28, $b5, $2f, $fd);
begin
  var Buffer: TBytes := nil;
  SetLength(Buffer, Length(ZSTDHeader));
  var Read := Stream.Read(Buffer, Length(Buffer));
  if Read < Length(Buffer) then exit(false);
  for var i := Low(ZSTDHeader) to High(ZSTDHeader) do
  begin
    if ZSTDHeader[i] <> Buffer[i] then exit(false);
  end;
  Result := true;
end;

end.

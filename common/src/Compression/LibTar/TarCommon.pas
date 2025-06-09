unit TarCommon;
{$i ../../tmscommon.inc}
interface
uses SysUtils;

type
  TSector = Array[0..511] of byte;

  TTarFileInfo = record
    FileName: string;
    Attr: Integer;
    Size: Int64;
    TimeStamp: TDateTime;

    constructor Create(const aFileName: string; const aSearchRec: TSearchRec);
    function IsFolder: Boolean;
    function TarTime: Integer;
  end;


implementation
uses DateUtils;

{ TTarFileInfo }

constructor TTarFileInfo.Create(const aFileName: string;
  const aSearchRec: TSearchRec);
begin
  FileName := aFileName;
  Attr := aSearchRec.Attr;
  Size := aSearchRec.Size;
  TimeStamp := aSearchRec.TimeStamp;
end;

function TTarFileInfo.IsFolder: Boolean;
begin
  Result := Attr and faDirectory <> 0;
end;

function TTarFileInfo.TarTime: Integer;
begin
  if (TimeStamp < EncodeDate(1980, 1, 1)) then exit(0);

  Result := DateTimeToUnix(TimeStamp, false);
end;

end.

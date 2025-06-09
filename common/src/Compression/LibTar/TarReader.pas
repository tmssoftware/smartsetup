unit TarReader;
{$i ../../tmscommon.inc}
interface
uses Classes, SysUtils, TarCommon;

type
  TTarReader = class
  private
    Tar: TStream;
    Sector: TSector;
    function GetFileAttr: integer;
    function GetFileName: string;
    function GetFileSize: Int64;
    function GetFileModTime: TDateTime;
    function LinkFlags: byte;
    function NextSector: boolean;
    function SeparatePrefix(const s: string): string;
    function ReadOctal(const Position, Length: integer): Int64;
    function GetFileNameFromExtHeader(const MemBytes: TBytes; const MemSize: integer): string;
    procedure CheckCheckSum;
    function ReadString(const Position, Length: integer): string;
    procedure ProcessExtHeader(const MemBytes: TBytes; const MemSize: Integer; var Pos: integer; out Id,
      Value: string);
  public
    constructor Create(const ATar: TStream);
    destructor Destroy; override;

    function FindNext(var FileInfo: TTarFileInfo) : boolean;
    procedure ReadFile(const Stream: TStream; const Size: Int64); overload;
    procedure ReadFile(const FileName: string; const Size: Int64; const CreationDate: TDateTime); overload;
    procedure SkipFile(const Size: Int64);
  end;

implementation
uses DateUtils, IOUtils, ZSTD,
  {$IFDEF MSWINDOWS}
   Windows,
  {$ENDIF}
   Math;

{ TTarWriter }

constructor TTarReader.Create(const ATar: TStream);
begin
  Tar := ATar;
end;

destructor TTarReader.Destroy;
begin

  inherited;
end;

function TTarReader.ReadString(const Position, Length: integer): string;
begin
  //TEncoding lacks a direct  TEncoding.UTF8.GetBytes(array of byte, position, len)
  var Chars := TEncoding.UTF8.GetChars(Sector, Position, Length);
  Result := '';
  SetLength(Result, System.Length(Chars));
  System.Move(Chars[0], Result[1], System.Length(Chars) * SizeOf(Char));
  Result := Result.Trim;
end;

function OctalToInt(const Oct : String; const Len: integer): Int64;
begin
  Result := 0;
  for var i := 1 to Math.Min(Length(Oct), Len) do
  begin
    var Digit := Ord(Oct[i]);
    if (Digit = 0) or (Digit = 32) then Digit := Ord('0');
    if not (Digit in [Ord('0')..Ord('7')]) then raise Exception.Create('Invalid Octal Number: ' + Oct);
    Result := (Result * 8) + (Digit - Ord('0'));
  end;
end;

function TTarReader.ReadOctal(const Position, Length: integer): Int64;
begin
  var Str := ReadString(Position, Length);

  Exit(OctalToInt(Str, Length - 1));
end;

procedure TTarReader.CheckCheckSum;
begin
  var CheckSum: Int64 := 0;
  for var i := Low(Sector) to High(Sector) do
  begin
    if (i >= 148) and (i < 148 + 8) then Inc(CheckSum, 32)
    else Inc(CheckSum, Sector[i]);
  end;
  if CheckSum <> ReadOctal(148, 8) then raise Exception.Create('Tar file has invalid Checksums, might be corrupt.');
end;

function TTarReader.NextSector: boolean;
begin
  var Read := Tar.Read(Sector[0], SizeOf(Sector));
  if Read <= 0 then exit(false);
  if Read < SizeOf(Sector) then Tar.ReadBuffer(Sector[Read], SizeOf(Sector) - Read);
  if (Sector[0] = 0) then exit(false);
  CheckCheckSum;
  Result := true;

end;

function TTarReader.LinkFlags: byte;
begin
  Result := Sector[156];
end;


function TTarReader.SeparatePrefix(const s: string): string;
begin
  if s = '' then exit('');
  exit(s + '/');
end;

procedure TTarReader.ProcessExtHeader(const MemBytes: TBytes; const MemSize: Integer; var Pos: integer; out Id, Value: string);
begin
  var RecordLen := 0;
  var StartPos := Pos;
  while true do
  begin
    // No infinite lengths, to avoid blowing stuff.
    if (RecordLen > 100000000) or (Pos >= MemSize) or not (MemBytes[Pos] in [ord('0')..ord('9'), ord(' ')]) then raise Exception.Create('Invalid Ext header in tar file.');
    if MemBytes[Pos] = ord(' ') then break;
    RecordLen := RecordLen * 10 + (MemBytes[Pos] - Ord('0'));
    inc(Pos);
  end;

  var LengthLen :=  Pos - StartPos + 1;
  var ValueLen := RecordLen - LengthLen - 1; //the 1 is for the #10 at the end.
  if ValueLen < 0 then raise Exception.Create('Invalid Ext header in tar file.');

  var All := TEncoding.UTF8.GetString(MemBytes, Pos + 1, ValueLen);
  inc(Pos, 1 + RecordLen - LengthLen);

  var Eq := All.IndexOf('=');
  if (Eq < 0) then raise Exception.Create('Invalid Ext header in tar file: "' + All + '"');
  Id := All.Substring(0, Eq);
  Value := All.Substring(Eq + 1);
end;

function TTarReader.GetFileNameFromExtHeader(const MemBytes: TBytes; const MemSize: integer): string;
begin
  var Pos := 0;
  while (Pos < MemSize) do
  begin
    var Id, Value: string;
    ProcessExtHeader(MemBytes, MemSize, Pos, Id, Value);
    if Id = 'path' then exit(Value.Trim);
  end;

  Result := '';
end;

function TTarReader.GetFileName: string;
begin
  if LinkFlags = ord('L') then //gnu long name
  begin
    var Mem := TBytesStream.Create;
    try
      ReadFile(Mem, GetFileSize);
      Mem.Position := 0;
      Result := TEncoding.UTF8.GetString(Mem.Bytes, 0, Mem.Size).Trim;
      if not NextSector then raise Exception.Create('Unexpected end of file.');
      exit;
    finally
      Mem.Free;
    end;
  end;

  if LinkFlags = ord('x') then //posix extended header
  begin
    var Mem := TBytesStream.Create;
    try
      ReadFile(Mem, GetFileSize);
      Mem.Position := 0;
      Result := GetFileNameFromExtHeader(Mem.Bytes, Mem.Size);
      if not NextSector then raise Exception.Create('Unexpected end of file.');
      if Result <> '' then exit; //This header might be about something else.
    finally
      Mem.Free;
    end;
  end;

  Result := SeparatePrefix(ReadString(345, 155))
            + ReadString(0, 100);
end;

function TTarReader.GetFileAttr: integer;
begin
  Result := 0;
  if (LinkFlags = 0) or (LinkFlags = ord('0')) then Result := Result or faNormal;
  if (LinkFlags = ord('5')) then Result := Result or faDirectory;
end;

function TTarReader.GetFileSize: Int64;
begin
  Result := ReadOctal(124, 12);
end;

function TTarReader.GetFileModTime: TDateTime;
begin
  var UnixTimeStamp := ReadOctal(136, 12);
  Result := UnixToDateTime(UnixTimeStamp, false);
end;


function TTarReader.FindNext(var FileInfo: TTarFileInfo): boolean;
begin
  Repeat
    if not NextSector then exit(false);

    FileInfo.FileName := GetFileName;
    FileInfo.Attr := GetFileAttr;
    FileInfo.Size := GetFileSize;
    FileInfo.TimeStamp := GetFileModTime;
    if (FileInfo.Attr = 0) then SkipFile(FileInfo.Size);

  Until FileInfo.Attr <> 0;
  Result := true;
end;

procedure TTarReader.ReadFile(const FileName: string; const Size: Int64; const CreationDate: TDateTime);
begin
  var Fs := TFileStream.Create(FileName, fmCreate);
  try
    ReadFile(Fs, Size);
  {$IFDEF MSWINDOWS}
    //Optimization to avoid opening the handle many times.

    var SystemTime: TSystemTime; DateTimeToSystemTime(CreationDate, SystemTime);
    var UtcTime: TSystemTime; if not TzSpecificLocalTimeToSystemTime(nil, SystemTime, UtcTime) then exit;
    var FileTime: TFileTime; if not SystemTimeToFileTime(UtcTime, FileTime) then exit;
    SetFileTime(Fs.Handle, @FileTime, @FileTime, @FileTime);
  {$ENDIF}
  finally
    Fs.Free;
  end;
  {$IFNDEF MSWINDOWS}
  //The slow way
    //SetCreationTime Not needed for posix: On POSIX and Mac, SetCreationTime will only update the last accessed time, because a Creation Time for a file system entry is not defined.
    TFile.SetLastAccessTime(FileName, CreationDate);
    TFile.SetLastWriteTime(FileName, CreationDate);
  {$ENDIF}
end;

procedure TTarReader.ReadFile(const Stream: TStream; const Size: Int64);
begin
  if Size = 0 then exit;
  if Tar is TZSTDDecompressStream then
  begin
    TZSTDDecompressStream(Tar).SaveToStream(Stream, Size);
  end else
  begin
    Stream.CopyFrom(Tar, Size);
  end;

  var WrittenInLastSector := ((Size - 1) mod Sizeof(Sector)) + 1;
  var Pad := SizeOf(Sector) - WrittenInLastSector;
  if Pad = 0 then exit;

  Tar.Seek(Pad, soFromCurrent);
end;

procedure TTarReader.SkipFile(const Size: Int64);
begin
  if Size = 0 then exit;

  var WrittenInLastSector := ((Size - 1) mod Sizeof(Sector)) + 1;
  var Pad := SizeOf(Sector) - WrittenInLastSector;

  Tar.Seek(Size + Pad, soFromCurrent);
end;

end.

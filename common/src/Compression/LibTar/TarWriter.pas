unit TarWriter;
{$i ../../tmscommon.inc}

// This class writes PAX files, as GNU is supposed to be switching to it.
// The reader can read both PAX and GNU
interface
uses Classes, SysUtils, TarCommon;

type
  TTarWriter = class
  private
  const
    Magic : Array[0..5] of byte = (ord('u'), ord('s'), ord('t'), ord('a'), ord('r'), 0);
    EmptyChecksum: Array[0..7] of byte = (32, 32, 32, 32, 32, 32, 32, 32);
  private
    Tar: TStream;
    WriteFileTimes: boolean;
    Sector: TSector;
    SectorForLongName: TSector;
    PaxHeader: TArray<Byte>;
    class procedure WriteOctal(var ASector: TSector; const Position, Len: Integer; const Number: Int64); static;
    procedure WriteOctalName(const Position, Len: Integer; const Number: Int64);
    procedure WriteOctalHeader(const Position, Len: Integer; const Number: Int64);
    class function GetChecksum(const ASector: TSector): integer; static;
    function GetMode(const Item: TTarFileInfo): integer;
    function GetMTime(const Item: TTarFileInfo): integer;
    procedure WriteFileData(const NameOnDisk: string);
    procedure EndFile;
    procedure WriteNameSector(const NameOnTarBytes: TArray<Byte>);
    class procedure PadFileData(const ATar: TStream; const StSize: Int64; var ASector: TSector); static;
    procedure WriteNameSectorData(const NameOnTarBytes: TArray<Byte>; const FullDataLength: Integer);
  public
    constructor Create(const ATar: TStream; const AWriteFileTimes: boolean = true);
    destructor Destroy; override;
    procedure AddItem(const Item: TTarFileInfo; const NameOnTar: string);
  end;

implementation
uses Math, IOUtils, ZLib;

{ TTarWriter }

function CalcCRC32(const bytes: TArray<Byte>): string;
begin
  if Length(Bytes) = 0 then exit('0');

  Result := IntToStr(crc32(0, @(bytes[0]), Length(bytes)));
end;

function IntToOctal(Value : Int64): string;
const
  OctDigits  : array[0..7] of Char = '01234567';
begin
  if Value = 0 then
    Result := '0'
  else begin
    Result := '';
    while Value > 0 do begin
      Result := OctDigits[Value and 7] + Result;
      Value := Value shr 3;
    end;
  end;
end;

class procedure TTarWriter.WriteOctal(var ASector: TSector; const Position, Len: Integer; const Number: Int64);
begin
  var Str := IntToOctal(Number) + #0;
  if (Length(Str) > Len) then raise Exception.Create('Number is too big to store: ' + Str);
  var k := Length(Str) - Len + 1;
  for var i := 0 to Len - 1 do
  begin
    if (k <= 0) then ASector[Position + i] := ord('0') else ASector[Position + i] := ord(Str[k]);
    inc(k);
  end;

end;

procedure TTarWriter.WriteOctalHeader(const Position, Len: Integer;
  const Number: Int64);
begin
  WriteOctal(Sector, Position, Len, Number);
end;

procedure TTarWriter.WriteOctalName(const Position, Len: Integer;
  const Number: Int64);
begin
  WriteOctal(SectorForLongName, Position, Len, Number);
end;

class function TTarWriter.GetChecksum(const ASector: TSector): integer;
begin
  Result := 0;
  for var i := Low(ASector) to High(ASector) do
  begin
    Result := Result + ASector[i];
  end;
end;

function TTarWriter.GetMode(const Item: TTarFileInfo): integer;
begin
  Result := $1FF; //octal 777
end;

function TTarWriter.GetMTime(const Item: TTarFileInfo): integer;
begin
  if not WriteFileTimes then exit(0);
  Result := Item.TarTime;
end;

procedure TTarWriter.EndFile;
begin
  FillChar(Sector[0], SizeOf(Sector), 0);
  Tar.WriteBuffer(Sector[0], SizeOf(Sector));
  Tar.WriteBuffer(Sector[0], SizeOf(Sector));
end;

class procedure TTarWriter.PadFileData(const ATar: TStream; const StSize: Int64; var ASector: TSector);
begin
    if StSize = 0 then exit;

    var Remainder := (StSize - 1) mod SizeOf(ASector) + 1;
    var Pad := SizeOf(ASector) - Remainder;
    if Pad > 0 then
    begin
      FillChar(ASector[0], Pad, 0);
      ATar.WriteBuffer(ASector[0], Pad);
    end;

end;

procedure TTarWriter.WriteFileData(const NameOnDisk: string);
begin
  var Stream := TFileStream.Create(NameOnDisk, fmOpenRead or fmShareDenyNone);
  try
    Tar.CopyFrom(Stream);
    PadFileData(Tar, Stream.Size, Sector);
  finally
    Stream.Free;
  end;
end;

function CalcFullLength(const DLen: integer; const Id: string; const Data: TArray<byte>): integer;
begin
  Result := DLen + Length(' ') + Length(Id) + Length('=')  + Length(Data) + Length(#10);
end;


function FullLength(const Id: string; const Data: TArray<byte>): integer;
begin
  var DLen := 1; //We won't have smaller sizes than 3 for filename size. This is called only if len > 100 bytes. But just in case we use this for another property.

  var i := 0;
  while i < 4 do //This should converge after most 3 iterations
  begin
    Result := CalcFullLength(DLen, Id, Data);
    var NewDLen := Length(IntToStr(Result));
    if NewDLen = DLen then exit;
    DLen := NewDLen;
    inc(i);
  end;
  raise Exception.Create('Can''t calculate internal length of string.');

end;

procedure TTarWriter.WriteNameSectorData(const NameOnTarBytes: TArray<Byte>; const FullDataLength: Integer);
const
  NewLine: Array[0..0] of byte = (10);
begin
  var Header := TEncoding.UTF8.GetBytes(IntToStr(FullDataLength) + ' path=');
  Tar.WriteBuffer(Header, Length(Header));
  Tar.WriteBuffer(NameOnTarBytes[0], Length(NameOnTarBytes));
  Tar.WriteBuffer(NewLine[0], 1);
  PadFileData(Tar,Length(Header) + Length(NameOnTarBytes) + 1, SectorForLongName);
end;

procedure TTarWriter.WriteNameSector(const NameOnTarBytes: TArray<Byte>);
begin
  FillChar(SectorForLongName[0], Length(SectorForLongName), 0);

  System.Move(PaxHeader[0], SectorForLongName[0], Min(100, Length(PaxHeader)));

  WriteOctalName(100, 8, $1A4);
  WriteOctalName(108, 8, 0); //User ID
  WriteOctalName(116, 8, 0); //Group ID.

  var DataLength := FullLength('path', NameOnTarBytes);
  WriteOctalName(124, 12, DataLength);
  WriteOctalName(136, 12, 0);

  System.Move(EmptyChecksum[0], SectorForLongName[148], 8);

  SectorForLongName[156] := ord('x'); //typeflag
  //No Links
  System.Move(Magic[0], SectorForLongName[257], 6);

  SectorForLongName[263] := ord('0'); //version
  SectorForLongName[264] := ord('0');

  //gname and uname are #0
  WriteOctalName(329, 8, 0); //devmajor
  WriteOctalName(337, 8, 0); //devminor

  //At the end, when the header is filled.
  WriteOctalName(148, 8, GetChecksum(SectorForLongName));
  Tar.WriteBuffer(SectorForLongName, Length(SectorForLongName));

  WriteNameSectorData(NameOnTarBytes, DataLength);

end;

procedure TTarWriter.AddItem(const Item: TTarFileInfo; const NameOnTar: string);
begin
  if NameOnTar.Trim = '' then raise Exception.Create('Can''t add a file with an empty name.');

  // https://pubs.opengroup.org/onlinepubs/9699919799/utilities/pax.html#tag_20_92_13_03
  FillChar(Sector[0], Length(Sector), 0);
  var NameOnTarBytes := TEncoding.UTF8.GetBytes(NameOnTar.Replace('\','/'));

  //Technically we could write also 100 bytes and not have the string 0-terminated.
  //To avoid corner cases, a string with length 100 goes to long names. We will not use the prefix either.
  if Length(NameOnTarBytes) >= 100 then
  begin
    WriteNameSector(NameOnTarBytes);
  end;
  System.Move(NameOnTarBytes[0], Sector[0], Min(100, Length(NameOnTarBytes)));

  WriteOctalHeader(100, 8, GetMode(Item));
  WriteOctalHeader(108, 8, 0); //User ID
  WriteOctalHeader(116, 8, 0); //Group ID.

  var FileSize := 0;
  if not Item.IsFolder then FileSize := Item.Size;
  WriteOctalHeader(124, 12, FileSize);
  WriteOctalHeader(136, 12, GetMTime(Item));

  System.Move(EmptyChecksum[0], Sector[148], 8);

  if Item.IsFolder then Sector[156] := ord('5') //typeflag: directory.
  else Sector[156] := ord('0'); //typeflag: file.
  //No Links
  System.Move(Magic[0], Sector[257], 6);

  Sector[263] := ord('0'); //version
  Sector[264] := ord('0');

  //gname and uname are #0
  WriteOctalHeader(329, 8, 0); //devmajor
  WriteOctalHeader(337, 8, 0); //devminor

  if Length(NameOnTarBytes) >= 100 then
  begin
    var PathCutBytes := TEncoding.UTF8.GetBytes('@PathCut/_pc_crc32/' + CalcCRC32(NameOnTarBytes));
    System.Move(PathCutBytes[0], Sector[345], Length(PathCutBytes));
  end;

  //At the end, when the header is filled.
  WriteOctalHeader(148, 8, GetChecksum(Sector));
  Tar.WriteBuffer(Sector, Length(Sector));

  if not Item.IsFolder then WriteFileData(Item.FileName);

end;

constructor TTarWriter.Create(const ATar: TStream; const AWriteFileTimes: boolean);
begin
  Tar := ATar;
  PaxHeader := TEncoding.UTF8.GetBytes('PaxHeader/@PaxHeader');
  WriteFileTimes := AWriteFileTimes;
end;

destructor TTarWriter.Destroy;
begin
  EndFile;
  inherited;
end;

end.

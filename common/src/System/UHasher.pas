unit UHasher;

interface
function HashFile(const FileName: string): String;

implementation

uses Classes, SysUtils, Generics.Collections, Hash;

function HashFile(const FileName: string): String;
const
  BUFFERSIZE = 4096;
var
  Buffer: TBytes;
  Hasher: THashSHA2;
  BytesRead: Longint;
  Stream: TFileStream;
begin
  Hasher := THashSHA2.Create;
  SetLength(Buffer, BUFFERSIZE);

  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    while True do
    begin
      BytesRead := Stream.ReadData(Buffer, BUFFERSIZE);
      if BytesRead = 0 then
        Break;
      Hasher.Update(Buffer, BytesRead);
    end;
  finally
    Stream.Free;
  end;

  Result := Hasher.HashAsString;
end;

end.

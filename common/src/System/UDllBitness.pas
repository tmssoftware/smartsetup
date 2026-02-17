unit UDllBitness;
{$i ../tmscommon.inc}

{$IFDEF MSWINDOWS}
interface
type
  TBitness = (
    Unknown,
    CantOpen,
    Intel32,
    Intel64
  );

  function GetDllBitness(const FileName: string): TBitness;
implementation
uses Classes, SysUtils, Windows, IOUtils;

function GetDllBitness(const FileName: string): TBitness;
const
  IMAGE_FILE_MACHINE_I386     = $014c;
  IMAGE_FILE_MACHINE_AMD64    = $8664;
var
  Header: TImageDosHeader;
  ImageNtHeaders: TImageNtHeaders;
begin
  Result := TBitness.Unknown;
  try
    if not TFile.Exists(FileName, true)
      then exit(TBitness.CantOpen);

    var Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try 
      Stream.ReadBuffer(Header, SizeOf(Header));
      if (Header.e_magic <> IMAGE_DOS_SIGNATURE) or
         (Header._lfanew = 0) then exit;

      Stream.Position := Header._lfanew;
      Stream.ReadBuffer(ImageNtHeaders, SizeOf(ImageNtHeaders));
      if ImageNtHeaders.Signature <> IMAGE_NT_SIGNATURE then exit;

      case (ImageNtHeaders.FileHeader.Machine) of
        IMAGE_FILE_MACHINE_I386: exit(TBitness.Intel32);
        IMAGE_FILE_MACHINE_AMD64: exit(TBitness.Intel64);
      end;

    finally
      Stream.Free;
    end;
  except
    //ignore corrupt files.
    exit (TBitness.CantOpen);
  end;
end;


{$ELSE}
interface
implementation
{$ENDIF}
end.

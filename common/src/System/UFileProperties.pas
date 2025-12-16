unit UFileProperties;

interface
{$IFDEF MSWINDOWS}
uses SysUtils, Windows;
function GetFileCompanyName(const FileName: string): string;
{$ENDIF}
implementation
{$IFDEF MSWINDOWS}
function GetFileCompanyName(const FileName: string): string;
type
  PLandCodepage = ^TLandCodepage;
  TLandCodepage = record
    wLanguage,
    wCodePage: word;
  end;
var
  dummy,
  len: cardinal;
  buf, pntr: pointer;
  lang: string;
begin
  Result := '';
  len := GetFileVersionInfoSize(PChar(FileName), dummy);
  if len = 0 then exit;
  GetMem(buf, len);
  try
    if not GetFileVersionInfo(PChar(FileName), 0, len, buf) then exit;

    if not VerQueryValue(buf, '\VarFileInfo\Translation\', pntr, len) then exit;

    lang := Format('%.4x%.4x', [PLandCodepage(pntr)^.wLanguage, PLandCodepage(pntr)^.wCodePage]);

    // Get Company's name, if available...
    if VerQueryValue(buf, PChar('\StringFileInfo\' + lang + '\CompanyName'), pntr, len){ and (@len <> nil)} then
      exit(PChar(pntr));
  finally
    FreeMem(buf);
  end;
end;

{$ENDIF}
end.

unit MainMacroUnit;

interface
{$IFDEF WIN32}
{$LINK zstd_ddict}
{$LINK zstd_common}
{$ENDIF}
{$IFDEF WIN64}
{$LINK zstd_ddict.o}
{$LINK zstd_common.o}
{$ENDIF}

implementation
uses
  System.Win.Crtl; //resolves malloc, free, memcpy...

//The linked objects are only here to test that we can find them.
//They reference zstd functions that live in other zstd objects, so we stub them.
//They are never called.

function ZSTD_loadDEntropy(entropy: Pointer; dict: Pointer; dictSize: NativeUInt): NativeUInt; cdecl;
begin
  Result := NativeUInt(-1);
end;

function ERR_getErrorString(code: Integer): PAnsiChar; cdecl;
begin
  Result := nil;
end;

end.

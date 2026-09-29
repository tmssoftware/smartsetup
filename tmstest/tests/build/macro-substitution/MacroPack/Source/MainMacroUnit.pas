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

end.

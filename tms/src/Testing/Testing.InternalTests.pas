unit Testing.InternalTests;

interface
{$IFDEF DEBUG}
procedure RunInternalTests;
{$ENDIF}

implementation

{$IFDEF DEBUG}
uses Commands.Logging, BBStrings, BBFlow, Deget.Version;
procedure RunInternalTests;
begin
  InitFolderBasedCommand;
  BBFlow_InternalTests;
  DegetVersion_InternalTests;
  BBStrings_InternalTests;
end;
{$ENDIF}
end.

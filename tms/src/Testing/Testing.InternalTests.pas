unit Testing.InternalTests;

interface
{$IFDEF DEBUG}
procedure RunInternalTests;
{$ENDIF}

implementation

{$IFDEF DEBUG}
uses Commands.Logging, BBStrings, BBClasses, Deget.Version;
procedure RunInternalTests;
begin
  InitFolderBasedCommand;
  BBClasses_InternalTests;
  DegetVersion_InternalTests;
  BBStrings_InternalTests;
{$ENDIF}
end;
end.

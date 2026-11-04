unit Commands.InternalTesting;

interface
{$IFDEF DEBUG}
procedure RegisterInternalTesting;
{$ENDIF}

implementation
{$IFDEF DEBUG}
uses UCommandLine, Commands.CommonOptions, Testing.InternalTests;

procedure RegisterInternalTesting;
begin
  var cmd := TOptionsRegistry.RegisterCommand('internal-test', '', 'runs internal tests to ensure basic functionality is ok.',
    '',
    '', false);

  AddCommand(cmd.Name, CommandGroups.Status, RunInternalTests);
end;
{$ENDIF}

end.

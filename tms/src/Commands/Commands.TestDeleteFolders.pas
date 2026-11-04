unit Commands.TestDeleteFolders;
interface
{$IFDEF DEBUG}
procedure RegisterTestDeleteFolders;
{$ENDIF}

implementation
{$IFDEF DEBUG}
uses UCommandLine, Commands.CommonOptions, Testing.TestDeleteFolders;

procedure RegisterTestDeleteFolders;
begin
  var cmd := TOptionsRegistry.RegisterCommand('test-delete-folder', '', 'deletes a folder using our built-in methods.',
    '',
    '', false);

  var option := cmd.RegisterOption<string>('mode',
    procedure(const Value: string)
    begin
      TestDeleteFolderMode := Value;
    end);
  option.Required := True;

  option := cmd.RegisterOption<string>('folder',
    procedure(const Value: string)
    begin
      TestDeleteFolderFolder := Value;
    end);
  option.Required := True;


  AddCommand(cmd.Name, CommandGroups.Status, RunTestDeleteFolders);
end;
{$ENDIF}
end.

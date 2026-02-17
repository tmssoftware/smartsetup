unit Commands.Snapshot;
//Conceptually, right now this is just a tms list -json.
//But we have it in a different place because it might evolve differently.

interface
uses
  System.Generics.Collections, System.SysUtils, UCommandLine;

procedure RegisterSnapshotCommand;
procedure TakeSnapshot(const FileName: string);

implementation
uses
  Commands.CommonOptions, Commands.Logging, Commands.GlobalConfig, Status.Manager, Deget.CoreTypes,
  Deget.Version, Snapshot.Writer;

procedure TakeSnapshot(const FileName: string);
begin
  var Manager := TStatusManager.Create(Config);
  try
    Manager.Update;
    WriteSnapshot(FileName, Manager.Products);
  finally
    Manager.Free;
  end;
end;


var OptionFileName: string;

procedure RunSnapshotCommand;
begin
  InitFolderBasedCommand(false);
  TakeSnapshot(OptionFileName);
end;


procedure RegisterSnapshotCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('snapshot', '', 'save a list of the current products and versions to a file, so they can be recovered with tms restore.',
    '',
    'snapshot <filename>');

  cmd.Examples.Add('snapshot c:\test\tms.snapshot.yaml');

  var option := cmd.RegisterUnnamedOption<string>('File where to save the snapshot', 'filename',
    procedure(const Value : string)
    begin
      OptionFileName := Value;
    end);
  option.Required := True;

  AddCommand(cmd.Name, CommandGroups.Status, RunSnapshotCommand);
end;

end.

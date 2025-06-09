unit Commands.LogView;

interface

uses
  System.SysUtils, UCommandLine, VSoft.CommandLine.Options, Commands.CommonOptions;

procedure RegisterLogViewCommand;

implementation

uses
  UTmsBuildSystemUtils, Commands.GlobalConfig, System.IOUtils, System.Classes, System.Types;

var
  AsText,
  Print: Boolean;

procedure RunLogViewCommand;
begin
  var LogFileName := Config.Folders.LogFile;
  if not AsText then LogFileName := LogFileName + '.html';

  if Print then
    WriteLn(LogFileName)
  else
    LaunchFile(LogFileName);
end;

procedure RegisterLogViewCommand;
begin
  Print := False;
  AsText := False;
  var cmd := TOptionsRegistry.RegisterCommand('log-view', '', 'Opens the log file in the default viewer.',
    'Opens the log file in the default viewer. Depending on the options it can the the html log or the text log.',
    'log-view [<options>]');

  var optionPrint := cmd.RegisterOption<Boolean>('print', '', 'Writes the log filename to the screen and do not open the log file',
    procedure(const Value : Boolean)
    begin
      Print := Value;
    end);
  optionPrint.HasValue := False;

  var optionAsText := cmd.RegisterOption<Boolean>('text', 't', 'Displays the latest text log, not the html log.',
    procedure(const Value : Boolean)
    begin
      AsText := Value;
    end);
  optionAsText.HasValue := False;

  AddCommand(cmd.Name, CommandGroups.Status, RunLogViewCommand);
end;

end.

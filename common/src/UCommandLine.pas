unit UCommandLine;

interface

uses
  System.Generics.Collections, System.SysUtils, System.IOUtils, VSoft.CommandLine.Options, VSoft.CommandLine.Utils;

/// <summary>
///   Parses the command-line and executes the registered command.
/// </summary>
/// <returns>
///   If the parsed command was registered and is execute, returns empty string.
///   Otherwise, returns the command to be manualy executed.
/// </returns>
function ParseCommandLine(InitProc: TProc = nil): string;

/// <summary>
///   Registers a command to be executed.
/// </summary>
procedure AddCommand(const Command: string; Proc: TProc); overload;
procedure AddCommand(const Command, GroupName: string; Proc: TProc); overload;

/// <summary>
///   Register the common commands, like "help".
/// </summary>
procedure RegisterCommonOptions;

/// <summary>
///   Print helps for specified command
/// </summary>
procedure PrintUsage(const command: string);

procedure PrintGroupedCommands;

// aliases to avoid having to use VSoft.CommandLine.Options unit all the time.
type
  TOptionsRegistry = VSoft.CommandLine.Options.TOptionsRegistry;

type
  TCommandGroup = class
  private
    FName: string;
    FCommands: TList<string>;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read FName write FName;
    property Commands: TList<string> read FCommands write FCommands;
  end;

  TCommandGroups = class(TObjectList<TCommandGroup>)
  public
    function Find(const AName: string): TCommandGroup;
  end;

implementation

var
  CommandForHelp: string;
  _Commands: TDictionary<string, TProc>;
  _Groups: TCommandGroups;

procedure AddCommand(const Command: string; Proc: TProc);
begin
  _Commands.AddOrSetValue(Command, Proc);
end;

procedure AddCommand(const Command, GroupName: string; Proc: TProc); overload;
begin
  AddCommand(Command, Proc);

  // Find or add group
  var Group := _Groups.Find(GroupName);
  if Group = nil then
  begin
    Group := TCommandGroup.Create;
    _Groups.Add(Group);
    Group.Name := GroupName;
  end;

  // Find or add command
  if not Group.Commands.Contains(Command) then
    Group.Commands.Add(Command);
end;

procedure RegisterCommonOptions;
var
  cmd: TCommandDefinition;
  option: IOptionDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('help', 'h', 'get help for command',
    '', 'help <command>');
  option := cmd.RegisterUnNamedOption<string>('The command you need help for',
    'command',
    procedure(const Value: string)
    begin
      CommandForHelp := Value;
    end);
end;

procedure PrintUsage(const command: string);
begin
  if command = 'help' then
  begin
    if CommandForHelp = '' then
      CommandForHelp := 'help';
  end
  else
    CommandForHelp := command;
  TOptionsRegistry.PrintUsage(CommandForHelp,
    procedure(const Value: string)
    begin
      WriteLn(Value);
    end);
end;

function PadRight(const s: string; TotalWidth: Integer; PaddingChar: Char = ' '): string;
begin
  TotalWidth := TotalWidth - Length(s);
  if TotalWidth > 0 then
    Result := s + StringOfChar(PaddingChar, TotalWidth)
  else
    Result := s;
end;

procedure PrintGroupedCommands;
var
  Width: Integer;
  Tab: Integer;

  procedure PrintCommand(Cmd: ICommandDefinition);
  begin
    var s := WrapText(cmd.Description, Width);
    var descStrings := TStringUtils.Split(s, sLineBreak);
    WriteLn('  ' + PadRight(Cmd.Name, Tab -1) + descStrings[0]);
    var numDescStrings := Length(descStrings);
    if numDescStrings > 1 then
    begin
      for var i := 1 to numDescStrings -1 do
        WriteLn(PadRight('', Tab) + descStrings[i]);
    end;
  end;

begin
  Width := GetConsoleWidth;
  Tab := TOptionsRegistry.DescriptionTab;

  var RemainingCommands := TList<string>.Create(TOptionsRegistry.RegisteredCommands.Keys);
  try
    for var Group in _Groups do
    begin
      WriteLn(Group.Name);
      for var CommandName in Group.Commands do
      begin
        var Cmd := TOptionsRegistry.RegisteredCommands[CommandName];
        if not Cmd.Visible then
          continue;
        RemainingCommands.Remove(Cmd.Name);
        PrintCommand(Cmd);
      end;
      WriteLn('');
    end;

    if RemainingCommands.Count > 0 then
    begin
      WriteLn('Other commands:');
      for var CommandName in RemainingCommands do
      begin
        var Cmd := TOptionsRegistry.RegisteredCommands[CommandName];
        if not Cmd.Visible then
          continue;
        PrintCommand(Cmd);
      end;
      WriteLn('');
    end;
  finally
    RemainingCommands.Free;
  end;
end;

function ParseCommandLine(InitProc: TProc = nil): string;
var
  parseresult: ICommandLineParseResult;
  Proc: TProc;
begin
  Result := '';
  TOptionsRegistry.DescriptionTab := 20;
  parseresult := TOptionsRegistry.Parse;
  if parseresult.HasErrors then
  begin
    WriteLn;
    WriteLn(parseresult.ErrorText);
    ExitCode := 2;
    Abort;
  end
  else
  begin
    if _Commands.TryGetValue(parseresult.Command, Proc) then
    begin
      if Assigned(InitProc) then
        InitProc();
      Proc();
    end
    else
    if (parseresult.command = '') then
    begin
      PrintUsage(parseresult.command);
      ExitCode := 2;
      Abort;
    end
    else if (parseresult.command = 'help') then
      PrintUsage(parseresult.command)
    else
      Result := parseresult.Command;
  end;
end;

{ TCommandGroup }

constructor TCommandGroup.Create;
begin
  FCommands := TList<string>.Create;
end;

destructor TCommandGroup.Destroy;
begin
  FCommands.Free;
  inherited;
end;

{ TCommandGroups }

function TCommandGroups.Find(const AName: string): TCommandGroup;
begin
  for var Group in Self do
    if Group.Name = AName then
      Exit(Group);
  Result := nil;
end;

initialization
  _Commands := TDictionary<string, TProc>.Create;
  _Groups := TCommandGroups.Create;

finalization
  _Commands.Free;
  _Groups.Free;
end.

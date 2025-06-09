unit Doctor.Run;

interface
uses
  Classes,
  SysUtils,
  Generics.Collections,
  Deget.CoreTypes,
  Doctor.Check,
  Character;

type
TDoctor = class
  private
    Checks: TObjectList<TCheck>;
    procedure Run(const Check: TCheck; const FixErrors, Confirm: boolean; var FixesToApply: integer);
  public
    constructor Create(const IDEName: TIDEName; const AlternateRegistryKey: string);
    destructor Destroy; override;
    procedure RunAllChecks(const FixErrors, Confirm: boolean);
end;

implementation
uses
  ULogger, UMultiLogger,
  UFileSystemPersistence,
  UConsoleUtil,
  Commands.GlobalConfig,
  Doctor.MultiIDECheck,
  Doctor.WindowsPathCheck,
  Doctor.DelphiEnvironmentCheck,
  Doctor.LibraryPathCheck;


{ TDoctor }

constructor TDoctor.Create(const IDEName: TIDEName; const AlternateRegistryKey: string);
begin
  inherited Create;
  Checks := TObjectList<TCheck>.Create;
 {$IFDEF MSWINDOWS}
  Checks.Add(TWindowsUserPathCheck.Create);
  Checks.Add(TWindowsLocalMachinePathCheck.Create);
  Checks.Add(TMultiIDECheck.Create(AlternateRegistryKey, TDelphiEnvironmentPathCheck));
  Checks.Add(TMultiIDECheck.Create(AlternateRegistryKey, TLibraryPathMultiSlashCheck));
  Checks.Add(TMultiIDECheck.Create(AlternateRegistryKey, TLibraryPathNotExistingFoldersCheck));
  {$ENDIF}

end;

destructor TDoctor.Destroy;
begin
  Checks.Free;

  inherited;
end;

procedure TDoctor.Run(const Check: TCheck; const FixErrors, Confirm: boolean; var FixesToApply: integer);
begin
  Logger.StartSection(TMessageType.Check, Check.Name);
  try
  Logger.Message(TLogMessageKind.Text, '');
  Logger.Message(TLogMessageKind.Caption, 'Checking ' + Check.Name);

  Check.Check;
  if Check.FixesCount = 0 then
  begin
    Logger.Message(TLogMessageKind.Caption, 'OK');
    exit;
  end;

  Inc(FixesToApply, Check.FixesCount);

  Logger.Message(TLogMessageKind.Comment, Check.Description);
  Logger.Message(TLogMessageKind.Text, '');

  for var i := 0 to Check.FixesCount - 1 do
  begin
    var Fix := Check.GetFix(i);
    Logger.Message(TLogMessageKind.Text, Fix.Message);
    if FixErrors then
    begin
      if Confirm then
      begin
        Logger.Message(TLogMessageKind.Question, Fix.Action + ' (Y/N)');
        while(True) do
        begin
          var Key := ' ';
          ReadLn(Key);
          Logger.Message(TLogMessageKind.Question, 'Answer: ' + key);
          var KeyLower := Key.ToLower;
          if (KeyLower = 'y') then
          begin
            Fix.Apply := true;
            break;
          end;
          if (KeyLower = 'n') then
          begin
            break;
          end;
          Logger.Message(TLogMessageKind.Question, 'Please write "y" or "n"');
        end;
      end else
      begin
        Fix.Apply := true;
      end;
    end;
  end;

  if FixErrors then
  begin
    try
      var UndoInfo := TUndoInfo.Create(Check.Name);
      try
        Check.Fix(UndoInfo);
        var Persist := TFileSystemPersistence.Create(Config.Folders.DoctorUndoFolder, DoctorUndoExtension);
        try
          Persist.List('test');
          Persist.Store(UndoInfo.Value.ToString, 'test');
        finally
          Persist.Free;
        end;
      finally
        UndoInfo.Free;
      end;
    except on ex: Exception do
      begin
        Logger.Error(ex.Message);
      end;
    end;
  end;

  finally
    Logger.FinishSection(TMessageType.Check);
  end;

end;



procedure TDoctor.RunAllChecks(const FixErrors, Confirm: boolean);
begin
  var FixesToApply := 0;
  var FixesApplied:= 0;
  for var Check in Checks do
  begin
    try
      Run(Check, FixErrors, Confirm, FixesToApply);

    except on ex: Exception do
    begin
      Logger.Error(ex.Message);
    end;
    end;
    Inc(FixesApplied, Check.FixesApplied);

  end;

  Logger.Message(TLogMessageKind.Text, '');

  if (FixesToApply = 0) then
  begin
    Logger.Message(TLogMessageKind.Conclusion, 'No issues were found.');
  end
  else
   if FixErrors then Logger.Message(TLogMessageKind.Conclusion, IntToStr(FixesApplied) + ' of ' + IntToStr(FixesToApply) + ' fixes were applied.')
   else Logger.Message(TLogMessageKind.Conclusion, 'No fix was applied out of ' + IntToStr(FixesToApply) + ' available. To automatically apply fixes, run "tms doctor -fix"');

end;

end.

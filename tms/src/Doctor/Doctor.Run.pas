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
    procedure Run(const Check: TCheck; const FixErrors, Confirm: boolean; var FixesToApply, FixesManual: integer);
    function GetYesNoAnswer(const Key: string; const Fix: TFix): boolean;
    function GetNumericAnswer(const Key: string; const Fix: TFix): boolean;
    procedure WriteQuestion(const Fix: TFix);
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
  Doctor.LibraryPathCheck,
  Doctor.DuplicatedBplCheck;


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
  Checks.Add(TDuplicatedBplCheck.Create);
  {$ENDIF}

end;

destructor TDoctor.Destroy;
begin
  Checks.Free;

  inherited;
end;

function TDoctor.GetYesNoAnswer(const Key: string; const Fix: TFix): boolean;
begin
  Result := false;
  Logger.Message(TLogMessageKind.Question, 'Answer: ' + Key);
  var KeyLower := Key.ToLower;
  if (KeyLower = 'y') then
  begin
    Fix.Apply := 1;
    exit(true);
  end;
  if (KeyLower = 'n') then
  begin
    exit(true);
  end;
  Logger.Message(TLogMessageKind.Question, 'Please write "y" or "n"');
end;

function TDoctor.GetNumericAnswer(const Key: string; const Fix: TFix): boolean;
begin
  Logger.Message(TLogMessageKind.Question, 'Answer: ' + Key);
  var KeyValue: integer;
  if not TryStrToInt(key.Trim, KeyValue) then
  begin
    Logger.Message(TLogMessageKind.Question, 'Please enter a number.');
    exit(false);
  end;

  if (KeyValue > Length(Fix.NumericQuestions)) then
  begin
    Logger.Message(TLogMessageKind.Question, 'Please enter a number less or equal to ' + IntToStr(Length(Fix.NumericQuestions)) + '.');
    exit(false);
  end;

  if (KeyValue < 0) then
  begin
    Logger.Message(TLogMessageKind.Question, 'Please enter a number bigger or equal to 0.');
    exit(false);
  end;

  if KeyValue = 0 then exit(true);

  Fix.Apply := KeyValue;
  exit(true);
end;

procedure TDoctor.WriteQuestion(const Fix: TFix);
begin
  case Fix.FixType of
    TFixType.YesNo: Logger.Message(TLogMessageKind.Question, Fix.Action + ' (Y/N)');
    TFixType.Numeric:
    begin
      Logger.Message(TLogMessageKind.Question, Fix.Action);
      Logger.Message(TLogMessageKind.Text, '  - 0. SKIP FIX');
      var i := 0;
      for var Question in Fix.NumericQuestions do
      begin
        inc(i);
        Logger.Message(TLogMessageKind.Text, '  - ' + IntToStr(i) + '. ' + Question);
      end;
    end;
  end;
end;

procedure TDoctor.Run(const Check: TCheck; const FixErrors, Confirm: boolean; var FixesToApply, FixesManual: integer);
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
        WriteQuestion(Fix);
        while(True) do
        begin
          var Key:string := ' ';
          ReadLn(Key);
          case Fix.FixType of
            YesNo: if GetYesNoAnswer(Key, Fix) then break;
            Numeric: if GetNumericAnswer(Key, Fix) then break;
          end;
        end;
      end else
      begin
        case Fix.FixType of
          YesNo: Fix.Apply := 1;
          Numeric:
            begin
              Fix.Apply := 0; //Can't be applied automatically. You need to select a number.
              inc(FixesManual);
            end;
        end;
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
  var FixesManual := 0;
  for var Check in Checks do
  begin
    try
      Run(Check, FixErrors, Confirm, FixesToApply, FixesManual);

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
   if FixErrors then
   begin
     Logger.Message(TLogMessageKind.Conclusion, IntToStr(FixesApplied) + ' of ' + IntToStr(FixesToApply) + ' fixes were applied.');
     if FixesManual > 0 then Logger.Message(TLogMessageKind.Conclusion, IntToStr(FixesManual) + ' fixes couldn''t be applied automatically because they require manual intervention. Run doctor without "-do-not-confirm" flag to fix those.')

   end
   else Logger.Message(TLogMessageKind.Conclusion, 'No fix was applied out of ' + IntToStr(FixesToApply) + ' available. To automatically apply fixes, run "tms doctor -fix"');

end;

end.

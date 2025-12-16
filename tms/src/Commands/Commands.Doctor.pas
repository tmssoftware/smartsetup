unit Commands.Doctor;

interface

uses
  Classes, System.SysUtils, UCommandLine, VSoft.CommandLine.Options, Commands.CommonOptions;

procedure RegisterDoctorCommand;

implementation
uses
  UTmsBuildSystemUtils, Commands.GlobalConfig, System.IOUtils,
  ULogger,
  UMultiLogger,
  Generics.Collections,
  System.Types,
  Deget.CoreTypes,
  Commands.Logging,
{$IFDEF DEBUG}
  Testing.Globals,
  Doctor.DuplicatedBplCheck,
{$ENDIF}
  Doctor.Run;
var
  FixParam: boolean;
  ConfirmParam: boolean;

procedure RunDoctorCommand;
begin
  InitFolderBasedCommand;
{$IFDEF DEBUG}
  if TestParameters.FolderForCompanyName <> '' then
  begin
    TDuplicatedBplCheck.TestCompanyName(TestParameters.FolderForCompanyName);
    exit;
  end;

{$ENDIF}
  var Doctor := TDoctor.Create(TIDEName.delphi12, Config.AlternateRegistryKey);
  try
    Doctor.RunAllChecks(FixParam, ConfirmParam);
  finally
    Doctor.Free;
  end;
end;

procedure RegisterDoctorCommand;
begin
  FixParam := False;
  ConfirmParam := true;
  var cmd := TOptionsRegistry.RegisterCommand('doctor', '', 'Checks and optionally fixes common problems in your Delphi installation.',
    'Checks for common problems in your Delphi installation. ' +
    'By default this command will not modify anything in your system, unless you pass a -fix parameter. If you pass a -fix parameter, it will ' +
    'still ask for confirmation for every change, unless you pass a -do-not-confirm parameter',
    'doctor [<options>]');

  var optionFix := cmd.RegisterOption<Boolean>('fix', '', 'Tries to fix the issues identified by the checks. It will ask you for confirmation before applying any check, unless you also specify -do-not-confirm.',
    procedure(const Value : Boolean)
    begin
      FixParam := Value;
    end);
  optionFix.HasValue := False;

  var optionDoNotConfirm := cmd.RegisterOption<Boolean>('do-not-confirm', '', '',
    procedure(const Value : Boolean)
    begin
      ConfirmParam := not Value;
    end);
  optionDoNotConfirm.HasValue := False;

  AddCommand(cmd.Name, CommandGroups.Diagnosis, RunDoctorCommand);
end;
end.

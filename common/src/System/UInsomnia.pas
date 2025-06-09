unit UInsomnia;
{$i ../tmscommon.inc}
interface
procedure PreventSleep;
procedure AllowSleep;
function SleepIsAllowedByPolicy: boolean;

implementation
{$IFDEF MSWINDOWS}
uses
  Windows, Registry;
// REF: http://msdn.microsoft.com/en-us/library/aa373208.aspx
type
  EXECUTION_STATE = DWORD;

const
  ES_SYSTEM_REQUIRED = $00000001;
  ES_DISPLAY_REQUIRED = $00000002;
  ES_USER_PRESENT = $00000004;
  ES_AWAYMODE_REQUIRED = $00000040;
  ES_CONTINUOUS = $80000000;

  KernelDLL = 'kernel32.dll';

{
  SetThreadExecutionState Function
  Enables an application to inform the system that it is in use,
  thereby preventing the system from entering sleep or turning off the
  display while the application is running.
}
procedure SetThreadExecutionState(ESFlags: EXECUTION_STATE);
  stdcall; external kernel32 name 'SetThreadExecutionState';

procedure PreventSleep;
begin
  SetThreadExecutionState(ES_SYSTEM_REQUIRED or ES_CONTINUOUS);
end;

procedure AllowSleep;
begin
  // Clear EXECUTION_STATE flags to disable away mode and allow the
  // system to idle to sleep normally.
  SetThreadExecutionState(ES_CONTINUOUS);
end;

function SleepIsAllowedByPolicy: boolean;
begin
  //see https://admx.help/?Category=Windows_10_2016&Policy=Microsoft.Policies.PowerManagement::AllowSystemPowerRequestAC
  //or see https://www.microsoft.com/en-us/download/details.aspx?id=25250 (Group Policy Settings Reference for Windows and Windows Server)
  var Reg:= TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    try
      if not (Reg.OpenKeyReadOnly('Software\Policies\Microsoft\Power\PowerSettings\A4B195F5-8225-47D8-8012-9D41369786E2')) then exit(False);
      if not Reg.ValueExists('ACSettingIndex') then exit (false);

      Result := Reg.ReadInteger('ACSettingIndex') = 1;
    except
      Result := false;
    end;
  finally
    Reg.Free;
  end;

end;

{$ELSE}
procedure PreventSleep;
begin
end;

procedure AllowSleep;
begin
end;

function SleepIsAllowedByPolicy: boolean;
begin
  Result := true;
end;


{$ENDIF}

end.

unit Doctor.WindowsPathCheck;

interface
{$IFDEF MSWINDOWS}
uses Doctor.Check, Doctor.CorePathCheck, SysUtils, Classes;

type
TWindowsPathCheck = class(TCorePathCheck)
protected
  function IsPathValid(const Path: string; out Reason, Action: string): boolean; override;
end;

TWindowsUserPathCheck = class(TWindowsPathCheck)
public
  constructor Create;
  function Name: string; override;
  function Description: string; override;
end;

TWindowsLocalMachinePathCheck = class(TWindowsPathCheck)
public
  constructor Create;
  function Name: string; override;
  function Description: string; override;
end;

{$ENDIF}

implementation
{$IFDEF MSWINDOWS}
uses UWindowsPath, IOUtils, Windows;

{ TWindowsPathCheck }

function TWindowsPathCheck.IsPathValid(const Path: string; out Reason,
  Action: string): boolean;
begin
   if not TDirectory.Exists(ExpandWindowsPath(Path.Trim)) then
   begin
      Reason := 'The path "' + Path.Trim + '" in the Windows Path doesn''t seem to point to an existing folder.';
      Action := 'Remove?';
      exit(False);
   end;
   Reason := '';
   Action := '';
   exit(True);

end;

{ TWindowsUserPathCheck }

constructor TWindowsUserPathCheck.Create;
begin
  inherited;
  GetPath := GetUserWindowsPath;
  SetPath := SetUserWindowsPath;
end;

function TWindowsUserPathCheck.Description: string;
begin
  Result := 'Windows has a maximum length in environment variables of 32760 characters. If ' +
       'your Windows search PATH is too big, you can reach that limit. This check looks for unused entries ' +
       'in your Windows search PATH for the current user, so you can reduce its size.'

end;

function TWindowsUserPathCheck.Name: string;
begin
  Result := 'Windows User Path';
end;

{ TWindowsLocalMachinePathCheck }

constructor TWindowsLocalMachinePathCheck.Create;
begin
  inherited;
  GetPath := GetLocalMachineWindowsPath;
  SetPath := SetLocalMachineWindowsPath;

end;

function TWindowsLocalMachinePathCheck.Description: string;
begin
  Result := 'Windows has a maximum length in environment variables of 32760 characters. If ' +
       'your Windows search PATH is too big, you can reach that limit. This check looks for unused entries ' +
       'in your Windows search PATH for the local machine, so you can reduce its size. Note that ' +
       'to apply the fixes of this check, you will need to run as an Administrator';
end;

function TWindowsLocalMachinePathCheck.Name: string;
begin
  Result := 'Windows Local Machine Path';
end;
{$ENDIF}
end.

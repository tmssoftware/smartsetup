unit Deget.CommandLine;

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Base,
  Posix.Fcntl,
{$ENDIF}
  System.SysUtils, Threading, Generics.Defaults, Generics.Collections;

type
  TStringProc = reference to procedure(const S: string);

  TCommandLine = class
  private
    class function InternalExecute(const CommandLine: string; const WorkingDirectory: string;
      CallbackProc: TStringProc; const Env: TArray<string> = nil; IsCanceledFunc: TFunc<Boolean> = nil): Integer;
  public
    class procedure Execute(const CommandLine: string); overload;
    class procedure Execute(const CommandLine: string; const WorkingDirectory: string); overload;
    class procedure Execute(const CommandLine: string; const WorkingDirectory: string; OutputCallback: TStringProc;
      const Env: TArray<string> = nil); overload;
    class function ExecuteEx(const CommandLine: string; const WorkingDirectory: string; OutputCallback: TStringProc;
     const Env: TArray<string> = nil; IsCanceledFunc: TFunc<Boolean> = nil): Integer; overload;
  end;

function ExecuteCommand(const CommandLine, WorkingDirectory: string; var Output: string; const Env: TArray<string> = nil; const ErrorsAreOk: Boolean = false): Boolean; overload;
function ExecuteCommand(const CommandLine, WorkingDirectory: string): Boolean; overload;
function ExecuteCommand(const CommandLine: string): Boolean; overload;

implementation

uses
  ULogger, UMultiLogger, UAppTerminated;

function ExecuteCommand(const CommandLine, WorkingDirectory: string): Boolean;
var
  DummyOutput: string;
begin
  Result := ExecuteCommand(CommandLine, WorkingDirectory, DummyOutput);
end;

function ExecuteCommand(const CommandLine, WorkingDirectory: string; var Output: string; const Env: TArray<string> = nil; const ErrorsAreOk: Boolean = false): Boolean; overload;
var
  LOutput: string;
begin
  Result := false;
  Logger.StartSection(TMessageType.Command, CommandLine);
  try
    var ErrorMessage := '';
    Output := '';
    LOutput := '';
    Logger.Trace('Execute command: ' + CommandLine);
    Logger.Trace('Working directory: ' + WorkingDirectory);
    try
      TCommandLine.Execute(CommandLine, WorkingDirectory,
        procedure(const S: string)
        begin
          LOutput := LOutput + S;
        end,
        Env
      );
      Result := True;
    except
      on E: Exception do
      begin
        Result := False;
        ErrorMessage := 'Execution failed: ' + E.Message;
      end;
    end;

    Logger.Trace('--- Output start ---');
    Logger.Trace(LOutput);
    Logger.Trace('--- Output end ---');
    Output := LOutput;

    if ErrorMessage <> '' then Logger.Trace(ErrorMessage);
  finally
    Logger.FinishSection(TMessageType.Command, (not ErrorsAreOk) and (not Result));
  end;

end;

function ExecuteCommand(const CommandLine: string): Boolean;
begin
  Result := ExecuteCommand(CommandLine, '');
end;

{ TCommandLine }

class procedure TCommandLine.Execute(const CommandLine, WorkingDirectory: string; OutputCallback: TStringProc; const Env: TArray<string> = nil);
var
  ExitCode: Integer;
begin
  ExitCode := InternalExecute(CommandLine, WorkingDirectory, OutputCallback, Env);
  if ExitCode <> 0 then
    raise Exception.Create('Could not execute command line. Exit code: ' + IntToStr(ExitCode));
end;

class function TCommandLine.ExecuteEx(const CommandLine, WorkingDirectory: string; OutputCallback: TStringProc;
  const Env: TArray<string> = nil; IsCanceledFunc: TFunc<Boolean> = nil): Integer;
begin
  Result := InternalExecute(CommandLine, WorkingDirectory, OutputCallback, Env, IsCanceledFunc);
end;

class procedure TCommandLine.Execute(const CommandLine: string; const WorkingDirectory: string);
begin
  Execute(CommandLine, WorkingDirectory, nil);
end;

class procedure TCommandLine.Execute(const CommandLine: string);
begin
  Execute(CommandLine, '', nil);
end;

{$IF Defined(MSWINDOWS)}

function GetEnvironment(const Env: TArray<string>): string;
begin
   //Each process has an environment block associated with it. The environment block consists of a null-terminated block of null-terminated strings (meaning there are two null bytes at the end of the block)
   //All strings in the environment block must be sorted alphabetically by name. The sort is case-insensitive, Unicode order, without regard to locale
   Result := '';

   var RemovePath := true;

   var TmpEnv: TList<string> := TList<string>.Create;
   try
     for var item in Env do TmpEnv.Add(item.Trim);

     var HasALLUSERSPROFILE := false;
     var HasAPPDATA := false;
     var HasLOCALAPPDATA := false;
     var HasSYSTEMROOT := false;
     var HasSYSTEMDRIVE := false;
     var HasTEMP := false;
     var HasTMP := false;
     for var e in Env do
     begin
       var idx := e.IndexOf('=');
       if (idx > 0) then
       begin
         var VariableName := e.Substring(0,idx).Trim;
         if (SameText(VariableName, 'ALLUSERSPROFILE')) then HasALLUSERSPROFILE := true;
         if (SameText(VariableName, 'APPDATA')) then HasAPPDATA := true;
         if (SameText(VariableName, 'LOCALAPPDATA')) then HasLOCALAPPDATA := true;
         if (SameText(VariableName, 'SYSTEMROOT')) then HasSYSTEMROOT := true;
         if (SameText(VariableName, 'SYSTEMDRIVE')) then HasSYSTEMDRIVE := true;
         if (SameText(VariableName, 'TEMP')) then HasTEMP := true;
         if (SameText(VariableName, 'TMP')) then HasTMP := true;
         if (SameText(VariableName, 'PATH')) then RemovePath := false; //do not remove it if it was the one we want to set. Used in CE.
       end;

     end;

     if not HasALLUSERSPROFILE then TmpEnv.Add('ALLUSERSPROFILE='  + GetEnvironmentVariable('ALLUSERSPROFILE')); //used for tmp
     if not HasAPPDATA then TmpEnv.Add('APPDATA='  + GetEnvironmentVariable('APPDATA'));
     if not HasLOCALAPPDATA then TmpEnv.Add('LOCALAPPDATA='  + GetEnvironmentVariable('LOCALAPPDATA'));
     if not HasSYSTEMROOT then TmpEnv.Add('SYSTEMROOT='  + GetEnvironmentVariable('SYSTEMROOT'));
     if not HasSYSTEMDRIVE then TmpEnv.Add('SYSTEMDRIVE='  + GetEnvironmentVariable('SYSTEMDRIVE'));
     if not HasTEMP then TmpEnv.Add('TEMP='  + GetEnvironmentVariable('TEMP'));
     if not HasTMP then TmpEnv.Add('TMP='  + GetEnvironmentVariable('TMP'));


     TmpEnv.Sort(TComparer<string>.Construct(
      function(const Left, Right: string): Integer
      begin
        Result := String.Compare(Left, Right, true, TLanguages.GetLocaleIDFromLocaleName('Unicode'));
      end)
     );

     Result := '';
     for var EnvStr in TmpEnv do
     begin
       if EnvStr = '' then continue;
       if RemovePath and EnvStr.StartsWith('PATH', true) then continue;

       Result := Result + EnvStr + #0;
     end;
   finally
     TmpEnv.Free;
   end;

   Result := Result + #0;
end;

function SetKillOnJobClose(const Job: THandle; const Value: Boolean): Boolean;
var
  Info: TJobObjectExtendedLimitInformation;
begin
  FillChar(Info, SizeOf(Info), 0);
  if Value then Info.BasicLimitInformation.LimitFlags := JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
  Result := SetInformationJobObject(Job, JobObjectExtendedLimitInformation, @Info, SizeOf(Info));
end;

function CreateKillOnCloseJob: THandle;
begin
  Result := CreateJobObject(nil, nil);
  if (Result <> 0) and not SetKillOnJobClose(Result, true) then
  begin
    CloseHandle(Result);
    Result := 0;
  end;
end;

function KillProcessTree(const Job, Process: THandle): Boolean;
begin
  Result := (Job <> 0) and TerminateJobObject(Job, ERROR_CANCELLED);
  if not Result then Result := TerminateProcess(Process, ERROR_CANCELLED);
end;

class function TCommandLine.InternalExecute(const CommandLine: string;
  const WorkingDirectory: string; CallbackProc: TStringProc; const Env: TArray<string> = nil; IsCanceledFunc: TFunc<Boolean> = nil): Integer;
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  Handle: Boolean;
  ExitCode: Cardinal;
  LocalCommandLine: string;
  LocalWorkingDir: PChar;
  Environment: string;
  PEnvironment: PChar;
  Job: THandle;
  Canceled, FinishedNormally: Boolean;

  //Read one chunk so continuous output cannot prevent cancellation checks.
  function PumpOutput: Boolean;
  var
    Buffer: array[0..4095] of AnsiChar;
    BytesRead, TotalBytesAvail: Cardinal;
  begin
    Result := false;
    if not PeekNamedPipe(StdOutPipeRead, nil, 0, nil, @TotalBytesAvail, nil) then exit;
    if TotalBytesAvail = 0 then exit;
    if not ReadFile(StdOutPipeRead, Buffer, SizeOf(Buffer) - 1, BytesRead, nil) then exit;
    if BytesRead = 0 then exit;
    if Assigned(CallbackProc) then
    begin
      Buffer[BytesRead] := #0;
      OemToCharBuffA(Buffer, Buffer, BytesRead);
      CallbackProc(String(Buffer));
    end;
    Result := true;
  end;

begin
   if Env <> nil then
   begin
     Environment := GetEnvironment(Env);
	 //Might be enabled in debug cases. It can contain some confidential info, so we won't have it by default.
     //Logger.Trace('Environment Variables: ');
     //Logger.Trace(Environment.Replace(#0, #10));
   end else
   begin
     Environment := '';
   end;
   if Environment <> '' then PEnvironment := PChar(Environment) else PEnvironment := nil;


  LocalCommandLine := CommandLine;
  UniqueString(LocalCommandLine);
  if WorkingDirectory <> '' then
    LocalWorkingDir := PChar(WorkingDirectory)
  else
    LocalWorkingDir := nil;
  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;
  SA.lpSecurityDescriptor := nil;
  if not CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0) then
    RaiseLastOSError;
  try
    FillChar(SI, SizeOf(SI), 0);
    FillChar(PI, SizeOf(PI), 0);

    SI.cb := SizeOf(SI);
    SI.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    SI.wShowWindow := SW_HIDE;
    SI.hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
    SI.hStdOutput := StdOutPipeWrite;
    SI.hStdError := StdOutPipeWrite;

    //Everything the child launches goes into this job, so a cancel can kill the whole tree and not only the direct child.
    //If we die without cleaning up, closing the job kills the tree too.
    Job := CreateKillOnCloseJob;
    FinishedNormally := false;
    try
      //No CREATE_NEW_PROCESS_GROUP: in the cli, the child gets the user's Ctrl-C too, and can start stopping by itself before we kill it.
      //CREATE_SUSPENDED: the child can't launch anything before it is in the job.
      Handle := CreateProcess(nil, PChar(LocalCommandLine), nil, nil, True,
        NORMAL_PRIORITY_CLASS or CREATE_UNICODE_ENVIRONMENT or CREATE_SUSPENDED,
        PEnvironment, PChar(LocalWorkingDir), SI, PI);
      CloseHandle(StdOutPipeWrite);
      if not Handle then
        RaiseLastOSError;

      try
        if (Job <> 0) and not AssignProcessToJobObject(Job, PI.hProcess) then
        begin
          //Can happen on Windows 7 if we are already inside a job. We can still kill the direct child.
          CloseHandle(Job);
          Job := 0;
        end;
        ResumeThread(PI.hThread);

        repeat
          Canceled := AppIsTerminated or (Assigned(IsCanceledFunc) and IsCanceledFunc());
          if Canceled then break;
          if not PumpOutput then
            if WaitForSingleObject(PI.hProcess, 50) = WAIT_OBJECT_0 then break;
        until false;

        //Kill the whole tree, not only the direct child. If even that fails, don't wait for a process that may never exit.
        if Canceled and KillProcessTree(Job, PI.hProcess) then
          WaitForSingleObject(PI.hProcess, 5000);

        //Before draining: if the output callback raises, we still keep what the child detached on purpose.
        FinishedNormally := not Canceled;
        while PumpOutput do begin end;

        GetExitCodeProcess(PI.hProcess, ExitCode);
        //The child might have exited with 0 just before we killed it. Not 0, or a canceled run reads as a success.
        if Canceled and (ExitCode = 0) then ExitCode := ERROR_CANCELLED;
        {$R-}
        Result := ExitCode;
        {$R+}
        CheckAppTerminated;
      finally
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
      end;
    finally
      if Job <> 0 then
      begin
        //After a normal run, keep anything the child detached on purpose (like git's background gc), same as before we used a job.
        //Canceled or failed: closing the job kills whatever is still running in it.
        if FinishedNormally then SetKillOnJobClose(Job, false);
        CloseHandle(Job);
      end;
    end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
end;
{$ELSEIF Defined(POSIX)}
// Adapted from https://stackoverflow.com/questions/43020737/delphi-capture-osx-console-output
type
  PIOFile = Pointer;

//Create a new stream connected to a pipe running the given command.
function popen(const Command: PAnsiChar; Modes: PAnsiChar): PIOFile; cdecl; external libc name _PU + 'popen';

//Close a stream opened by popen and return the status of its child.
function pclose(Stream: PIOFile): Integer; cdecl; external libc name _PU + 'pclose';

//Return the EOF indicator for STREAM.
function feof(Stream: PIOFile): Integer; cdecl; external libc name _PU + 'feof';

//Read chunks of generic data from STREAM.
function fread(Ptr: Pointer; Size: LongWord; N: LongWord;
  Stream: PIOFile): LongWord; cdecl; external libc name _PU + 'fread';

//Wait for a child to die.  When one does, put its status in *STAT_LOC
//and return its process ID.  For errors, return (pid_t) -1.
function wait(__stat_loc: PInteger): Integer; cdecl;
  external libc name _PU + 'wait';

class function TCommandLine.InternalExecute(const CommandLine: string;
  const WorkingDirectory: string; CallbackProc: TStringProc; const Env: TArray<string> = nil;
  IsCanceledFunc: TFunc<Boolean> = nil): Integer;

var
  Output: PIOFile;
  Buffer: TBytes;
  BytesRead: Integer;
const
  BufferSize: Integer = 1024;
begin
  Buffer := nil;
  SetLength(Buffer, BufferSize + 1);
  var FullCommandLine := CommandLine;
  if WorkingDirectory <> '' then FullCommandLine := 'cd "' + WorkingDirectory + '";' + FullCommandLine;
  
  Output := popen(PAnsiChar(Ansistring(FullCommandLine)), 'r');
  if Assigned(Output) then
  begin
    try
      while feof(Output) = 0 do
      begin
        BytesRead := fread(Buffer, 1, BufferSize, Output);
        CallbackProc(TEncoding.UTF8.GetString(Buffer, 0, BytesRead));
      end;
    finally
      //see https://man7.org/linux/man-pages/man3/popen.3.html
      Result := pclose(output);
      wait(nil);
    end;
  end
  else Result := -1
end;
{$ELSE}
begin
  raise Exception.Create('Command line execution not implemented in this platform.');
end;
{$IFEND}

end.


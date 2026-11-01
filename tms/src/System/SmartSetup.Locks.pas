unit SmartSetup.Locks;
{$i ../../tmssetup.inc}
interface
uses SysUtils, Classes, SyncObjs;

type
  TAppProtector = class
  private
{$IFDEF MSWINDOWS}
    Mutex: TMutex;
{$ELSE}
    LockFile: Integer;
{$ENDIF}
  public
    constructor Create(const aFolder, aGUID: string);
    destructor Destroy; override;
  end;

const
  ConfigProtector = 'A9BA222B-54F8-4100-AAA8-240C1D6AC5A6';
  CredentialsProtector = '0A55F773-A396-4F72-89B2-5E28F2711312';
  FolderInitProtector = '3A6A634A-C79D-4F00-B40B-D6DE46E0C930';
  RegistryProtector = '189CE014-5E80-4D85-B7BA-B1094798D5F2';

function AppIsAlreadyRunning(const Folder: string): boolean;
function IsAppOnlyInstance(const Folder: string): boolean;

//Locks shared by more than one place. Use these so all places compute the same key.
function LockConfig(const ConfigFile: string): TAppProtector;
function LockCredentials: TAppProtector;

implementation
uses
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
  Posix.Base, Posix.Fcntl, Posix.Unistd, Posix.Errno,
{$ENDIF}
   IOUtils, Hash;

function LockConfig(const ConfigFile: string): TAppProtector;
begin
  Result := TAppProtector.Create(TPath.GetFullPath(ConfigFile), ConfigProtector);
end;

function LockCredentials: TAppProtector;
begin
  //Credential manager is per user, not per folder. So lock per user.
  //Using a Local\ mutex wouldn't work because that's per session, not per user.
  Result := TAppProtector.Create(TPath.GetHomePath, CredentialsProtector);
end;

const
  AppIsRunningGUID = '7E07C55C-D558-40B8-96F7-364B036CAE74';
  AppInstanceGUID = 'D5AA926D-2B7D-480B-938F-869AB185BB1D';

// Name of the mutex must be at most 260 characters. So we use a hash, not the folder as name.
// In Windows we also use Folder.ToUpper so c:\test is the same as C:\TEST. For linux/mac we shouldn't do it.
function LockName(const GUID, Folder: string): string;
begin
{$IFDEF MSWINDOWS}
  var FolderHash := THashSHA2.GetHashString(Folder.ToUpperInvariant);
{$ELSE}
  var FolderHash := THashSHA2.GetHashString(Folder);
{$ENDIF}
  Result := 'tms-smart-setup-' + GUID + '-' + FolderHash;
end;

{$IFDEF MSWINDOWS}
function MutexName(const GUID, Folder: string): string;
begin
  //Global\ is to look into all users.
  Result := 'Global\' + LockName(GUID, Folder);
end;

{$ELSE}
// In posix, named mutexes don't exist (TMutex raises if you give it a name), so we use flock on files in /tmp.
// flock locks are released by the OS when the process dies, so like Windows mutexes, they can't get stale.
// We use /tmp and not TPath.GetTempPath, because in macOS TMPDIR is per user, and we want to see all users, like Global\ in Windows.
const
  LOCK_SH = 1;
  LOCK_EX = 2;
  LOCK_NB = 4;

function flock_(fd: Integer; operation: Integer): Integer; cdecl; external libc name _PU + 'flock';

function OpenLockFile(const GUID, Folder: string): Integer;
begin
  var FileName := TPath.Combine('/tmp', LockName(GUID, Folder) + '.lock');
  var M: TMarshaller;
  var FileNamePtr := MarshaledAString(M.AsUtf8(FileName).ToPointer);
  // We open the file read-only: flock doesn't need write access, and so we can lock a file created by another user.
  // We first try without O_CREAT, because with fs.protected_regular (default in many distros),
  // opening with O_CREAT an existing file owned by another user in /tmp fails even if it is readable.
  Result := open(FileNamePtr, O_RDONLY);
  if (Result = -1) and (errno = ENOENT) then
    Result := open(FileNamePtr, O_RDONLY or O_CREAT, $1B6); //0666, so other users can open it.
  if Result = -1 then
    RaiseLastOSError;

  // Don't let child processes (compilers, git...) inherit the handle, or they would keep the lock alive after we exit.
  fcntl(Result, F_SETFD, FD_CLOEXEC);
end;

// Returns false only if Operation has LOCK_NB and somebody else has the lock. Other errors raise.
function Lock(const fd, Operation: Integer): boolean;
begin
  repeat
    if flock_(fd, Operation) = 0 then exit(true);
  until errno <> EINTR;
  if ((Operation and LOCK_NB) <> 0) and (errno = EWOULDBLOCK) then exit(false);
  RaiseLastOSError;
  Result := false; //not reached
end;
{$ENDIF}

function IsAppOnlyInstance(const Folder: string): boolean;
begin
{$IFDEF MSWINDOWS}
  // The handle is never closed, so the mutex exists while this process is alive.
  if CreateMutex(nil, False, PChar(MutexName(AppInstanceGUID, Folder))) = 0 then
    RaiseLastOSError;

  var LastError := GetLastError;
  if LastError = ERROR_ALREADY_EXISTS then
    Exit(false);
  Result := true;
{$ELSE}
  // Every instance holds a shared lock for its whole life (the file is never closed).
  // If we can get an exclusive lock, nobody else holds the shared one. Then we downgrade it to shared.
  // This must be called inside a TAppProtector, so no other instance is checking at the same time.
  var fd := OpenLockFile(AppInstanceGUID, Folder);
  Result := Lock(fd, LOCK_EX or LOCK_NB);
  Lock(fd, LOCK_SH);
{$ENDIF}
end;

function AppIsAlreadyRunning(const Folder: string): boolean;
begin
{$IFDEF MSWINDOWS}
  // The handle is never closed, so the mutex exists while this process is alive.
  if CreateMutex(nil, True, PChar(MutexName(AppIsRunningGUID, Folder))) = 0 then
    RaiseLastOSError;

  var LastError := GetLastError;
  Result := LastError = ERROR_ALREADY_EXISTS;
{$ELSE}
  // If we get the lock we never close the file, so the lock is held while this process is alive.
  var fd := OpenLockFile(AppIsRunningGUID, Folder);
  Result := not Lock(fd, LOCK_EX or LOCK_NB);
  if Result then __close(fd);
{$ENDIF}
end;


{ TAppProtector }

constructor TAppProtector.Create(const aFolder, aGUID: string);
begin
  inherited Create;
{$IFDEF MSWINDOWS}
  Mutex := TMutex.Create(nil, false, MutexName(aGUID, aFolder));
  Mutex.Acquire;
{$ELSE}
  LockFile := -1; //if the constructor fails, the destructor runs, and fields start at 0, which is stdin.
  var fd := OpenLockFile(aGUID, aFolder);
  try
    Lock(fd, LOCK_EX);
  except
    __close(fd);
    raise;
  end;
  LockFile := fd;
{$ENDIF}
end;

destructor TAppProtector.Destroy;
begin
{$IFDEF MSWINDOWS}
  if Mutex <> nil then
  begin
    Mutex.Release;
    Mutex.Free;
  end;
{$ELSE}
  if LockFile <> -1 then __close(LockFile); //closing the file releases the lock.
{$ENDIF}
  inherited;
end;

end.

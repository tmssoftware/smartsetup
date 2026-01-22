unit UTmsBuildSystemUtils;
{$i ../tmscommon.inc}
interface
uses SysUtils, Classes,
{$IFDEF POSIX}
    Posix.UniStd, Posix.Stdio,
{$ENDIF}
    Generics.Collections;

const
  TempExtension = '.~tmp'; //make it unique so we don't remove it by mistake. We will search for *.TempExtension and delete files.

procedure TryDeleteFileAndRemoveParentFolderIfEmpty(const LockFolder, FileName: string);
procedure TryToDeleteAllFilesInFolderIgnoringLocked(const Folder: string; const Recursive: boolean = false; const RemoveFolders: boolean = false; const Pattern: string = '*');
procedure DeleteFolderMovingToLocked(const LockFolder, Folder: string; const Recursive: boolean; const KeepRootFolder: boolean = false);
procedure ScanFiles(const FilePath: string; const WildCardIncludeFolders, WildCardExcludeFolders,
    WildCardIncludeFiles, WildCardExcludeFiles: TArray<string>; const  OnFile: TProc<string, string>;
    const Recursive: boolean; const ExcludeDefault: boolean = true);

function CombinePath(const RootPath, RelPath: string): string;
procedure FindProjects(const FilePath, FileExt: string; const  Files: TList<string>; const AllowMany: boolean);
procedure LaunchFile(const FileName: string);
function FindProcessUsing(const FileName: string): string;
function GuidToStringN(const Guid: TGuid): String;
procedure TDirectory_CreateDirectory(const Path: string);
procedure DeleteFileOrMoveToLocked(const LockFolder, FileName: string; const DryRun: boolean; const Log: TProc<string>; var NeedsToRestartIDE: boolean); overload;
procedure DeleteFileOrMoveToLocked(const LockFolder, FileName: string); overload;

function AppIsAlreadyRunning(const Folder: string): boolean;

procedure AddPathsWithWildcards(const Result: TList<string>; const Folder: string; const ErrorInfo: string; const OnAdd: TProc<string, string, string>);

procedure RenameAndCheck(const SourceFile, DestFile: string);
procedure RenameAndCheckFolder(const SourceFolder, DestFolder: string);

function FolderIsOutside(const Folder: string; const RootFolders: TArray<string>): boolean;

function CreateUUIDv5(const Namespace: TGUID; const Name: string): TGUID;

function TPath_IsPathRooted(const s: string): boolean;


type
  TUTF8NoBOMEncoding = class(TUTF8Encoding)
  private
    class var
      FInstance: TUTF8NoBOMEncoding;
  public
    class constructor Create;
    class destructor Destroy;
    function GetPreamble: TBytes; override;
    class property Instance: TUTF8NoBOMEncoding read FInstance;
  end;

implementation
uses IOUtils, System.Hash, StrUtils, Masks, System.Types,
    {$IFDEF MSWINDOWS}
      Winapi.ShellAPI, Winapi.Windows, ActiveX, ComObj, Winapi.ShlObj;
    {$ENDIF MSWINDOWS}
    {$IFDEF POSIX}
      Posix.Stdlib;
    {$ENDIF POSIX}

class constructor TUTF8NoBOMEncoding.Create;
begin
  FInstance := TUTF8NoBOMEncoding.Create;
end;

class destructor TUTF8NoBOMEncoding.Destroy;
begin
  FInstance.Free;
end;

function TUTF8NoBOMEncoding.GetPreamble: TBytes;
begin
  Result := nil;
end;

function RemoveWildcards(const s: string): string;
begin
  Result := s.Replace('*', '_').Replace('?', '_');
end;

function TPath_IsPathRooted(const s: string): boolean;
begin
  //IsPathRooted complains if there are wildcards.
  Result := TPath.IsPathRooted(RemoveWildcards(s));
end;

function IsLinkedFolder(const Folder: string): boolean;
begin
  Result := TFile.Exists(Folder, false);
end;

procedure DeleteFolderWithoutFollowingSymlinks(const LockFolder, Folder: string);
begin
  if IsLinkedFolder(Folder) then
  begin
    DeleteFileOrMoveToLocked(LockFolder, Folder);
  end
  else
  begin
    SysUtils.RemoveDir(Folder);
  end;
  if TDirectory.Exists(Folder) then raise Exception.Create('Cannot delete folder "' + Folder + '". ' + SysErrorMessage(GetLastError));

end;

procedure TryDeleteFileAndRemoveParentFolderIfEmpty(const LockFolder, FileName: string);
begin
  // This is not atomic, so someone could delete the file
  // after TFile.Exists returns true, but anyway it is a good
  // normal case to avoid exceptions. If DeleteFile fails because
  // the file is locked or some disk error, then things are fishy
  // and we better throw an exception.
  DeleteFileOrMoveToLocked(LockFolder, FileName);

  if TDirectory.Exists(TPath.GetDirectoryName(FileName)) then
  begin
    if TDirectory.IsEmpty(TPath.GetDirectoryName(FileName))
    then TDirectory.Delete(TPath.GetDirectoryName(FileName))
  end;
end;

procedure DeleteFolderMovingToLockedInternal(const LockFolder, Folder: string; const Recursive: boolean; const RootFolder: string);
begin
var
  F: SysUtils.TSearchRec;
begin
  if SysUtils.FindFirst(TPath.Combine(Folder, '*'), faAnyFile, F) = 0 then
  begin
    try
      repeat
        if (F.Attr and faDirectory <> 0) and not IsLinkedFolder(TPath.Combine(Folder, F.Name)) then
        begin
          if Recursive and (F.Name <> '.') and (F.Name <> '..') then
          begin
            DeleteFolderMovingToLocked(LockFolder, TPath.Combine(Folder, F.Name), Recursive);
          end;
        end else
        begin //we don't care about errors here. If the file is locked, we leave it.
          var FileName := TPath.Combine(Folder, F.Name);
          DeleteFileOrMoveToLocked(LockFolder, FileName);
        end;
      until SysUtils.FindNext(F) <> 0;
    finally
      SysUtils.FindClose(F);
    end;
  end;
  end;
  if Folder <> RootFolder then
  begin
    DeleteFolderWithoutFollowingSymlinks(LockFolder, Folder);
  end;
end;

procedure DeleteFolderMovingToLocked(const LockFolder, Folder: string; const Recursive: boolean; const KeepRootFolder: boolean);
begin
  if KeepRootFolder then DeleteFolderMovingToLockedInternal(LockFolder, Folder, Recursive, Folder)
  else DeleteFolderMovingToLockedInternal(LockFolder, Folder, Recursive, '')
end;


procedure TryToDeleteAllFilesInFolderIgnoringLocked(const Folder: string; const Recursive: boolean = false; const RemoveFolders: boolean = false; const Pattern: string = '*');
var
  F: SysUtils.TSearchRec;
begin
  if SysUtils.FindFirst(TPath.Combine(Folder, '*'), faAnyFile, F) = 0 then
  begin
    try
      repeat
        if (F.Attr and faDirectory <> 0) then
        begin
          if Recursive and (F.Name <> '.') and (F.Name <> '..') then
          begin
            TryToDeleteAllFilesInFolderIgnoringLocked(TPath.Combine(Folder, F.Name), Recursive, RemoveFolders, Pattern);
          end;
        end else
        begin //we don't care about errors here. If the file is locked, we leave it.
          if (Pattern = '*') or (TPath.MatchesPattern(F.Name, Pattern, true)) then
          begin
            var FileName := TPath.Combine(Folder, F.Name);
            try
              if not SysUtils.DeleteFile(FileName) then
              begin
              {$IFDEF MSWINDOWS}
              {$WARN SYMBOL_PLATFORM OFF}
                TFile.SetAttributes(FileName, [TFileAttribute.faNormal]);
              {$WARN SYMBOL_PLATFORM ON}
                SysUtils.DeleteFile(FileName);
              {$ENDIF}
              end;
            except
              //ignore it.
            end;
          end;
        end;
      until SysUtils.FindNext(F) <> 0;
    finally
      SysUtils.FindClose(F);
    end;
  end;
  if (RemoveFolders) then SysUtils.RemoveDir(Folder);
end;

function CombinePath(const RootPath, RelPath: string): string;
begin
  var FullRootPath := TPath.GetFullPath(RootPath);
  var PathFixed := RelPath.Replace('\', PathDelim).Replace('/', PathDelim);

  Result := TPath.GetFullPath(TPath.Combine(FullRootPath, PathFixed));

  //we need some care here to avoid some malicious config file writing anywhere in the disk.
  //we could be passed something like folder/../../../something.pas and rewriting stuff they shouldn't.
  //that's the reason also why we don't try to create folders at all. the folder must exist.

  if not (Result.StartsWith(FullRootPath)) then
  begin
    raise Exception.Create('The path for the file: "' + Result + '" is not valid. It must point to a file inside the project folder. (' + FullRootPath +')');
  end;

end;

procedure FindProjects(const FilePath, FileExt: string; const  Files: TList<string>; const AllowMany: boolean);
var
  F: SysUtils.TSearchRec;
begin
  // TDirectory.GetFiles(Filepath, FileExt, TSearchOption.soAllDirectories);// would be too slow. It would
  // search in all folders below the one with the yaml, and also in the .tmssetup folder
  var ChildFolders := TList<string>.Create;
  try
    var FilesInFolder := 0;
    if SysUtils.FindFirst(TPath.Combine(FilePath, '*'), faAnyFile, F) = 0 then
    begin
      try
        repeat
          if (F.Attr and faDirectory <> 0) then
          begin
            if StartsStr('.', F.Name) then continue;
            if StartsStr('__', F.Name) then continue;
            if SameText('RELEASE', F.Name) then continue;   //Avoid searching in the binary folders.
            if SameText('DEBUG', F.Name) then continue;
            if SameText('Temp', F.Name) then continue;
            if AllowMany then FindProjects(TPath.Combine(FilePath, F.Name), FileExt, Files, AllowMany)
            else ChildFolders.Add(F.Name);
          end else
          begin
            if not EndsText(FileExt, F.Name) then continue;

            Files.Add(TPath.Combine(FilePath, F.Name));
            inc(FilesInFolder);
            if not AllowMany and (FilesInFolder > 1) then raise Exception.Create('The folder "' + FilePath
              + '" contains more than one "' + FileExt
              + '" files. Only one project definition is allowed in a folder.');

          end;
        until SysUtils.FindNext(F) <> 0;
      finally
        SysUtils.FindClose(F);
      end;
    end;

    if (FilesInFolder = 0) then //ChildFolders only has data when AllowMany is false. In that case, we don't keep searching in other folders if we found the file.
    begin
      for var Folder in ChildFolders do FindProjects(TPath.Combine(FilePath, Folder), FileExt, Files, AllowMany);
    end;

  finally
    ChildFolders.Free;
  end;
end;


function CreateMask(const Masks: TArray<string>): TArray<TMask>;
begin
  Result := nil;
  SetLength(Result, Length(Masks));
  for var i := Low(Masks) to High(Masks) do
  begin
    Result[i] := TMask.Create(Masks[i]);
  end;
end;

function MatchesMask(const s: string; const Masks: TArray<TMask>): boolean;
begin
  Result := false;
  for var i := Low(Masks) to High(Masks) do
  begin
    if (Masks[i].Matches(s)) then exit(true);
  end;
end;

procedure FreeMasks(const Masks: TArray<TMask>);
begin
  for var i := Low(Masks) to High(Masks) do
  begin
    Masks[i].Free;
  end;
end;

procedure ScanFilesInternal(const FilePath, RelFilePath: string; const IncludeMaskFolders, ExcludeMaskFolders,
    IncludeMaskFiles, ExcludeMaskFiles: TArray<TMask>; const  OnFile: TProc<string, string>;
    const Recursive: boolean; const ExcludeDefault: boolean);
var
  F: SysUtils.TSearchRec;
begin
  if SysUtils.FindFirst(TPath.Combine(FilePath, '*'), faAnyFile, F) = 0 then
  begin
    try
      repeat
        if (F.Attr and faDirectory <> 0) then
        begin
          if ExcludeDefault then
          begin
            if StartsStr('.', F.Name) then continue;
            if StartsStr('__', F.Name) then continue;
          end else
          begin
            if (F.Name = '.') or (F.Name = '..') then continue;
          end;
          if MatchesMask(F.Name, ExcludeMaskFolders) then continue;
          if (IncludeMaskFolders = nil) or MatchesMask(F.Name, IncludeMaskFolders) then
          begin
            if Recursive then ScanFilesInternal(TPath.Combine(FilePath, F.Name), TPath.Combine(RelFilePath, F.Name), IncludeMaskFolders, ExcludeMaskFolders, IncludeMaskFiles, ExcludeMaskFiles, OnFile, Recursive, ExcludeDefault);
          end;
        end else
        begin
          if ExcludeDefault and (F.Name = '.gitignore') then continue;
          if MatchesMask(F.Name, ExcludeMaskFiles) then continue;
          if (IncludeMaskFiles = nil) or MatchesMask(F.Name, IncludeMaskFiles) then
          begin
            OnFile(TPath.Combine(FilePath, F.Name), TPath.Combine(RelFilePath, F.Name));
          end;
        end;
      until SysUtils.FindNext(F) <> 0;
    finally
      SysUtils.FindClose(F);
    end;
  end;
end;

procedure ScanFiles(const FilePath: string; const WildCardIncludeFolders, WildCardExcludeFolders,
    WildCardIncludeFiles, WildCardExcludeFiles: TArray<string>; const  OnFile: TProc<string, string>;
    const Recursive: boolean; const ExcludeDefault: boolean = true);
begin
  // TDirectory.GetFiles(Filepath, FileExt, TSearchOption.soAllDirectories, predicate); won't allow to exclude full folders, only files.

  var IncludeMaskFolders := CreateMask(WildCardIncludeFolders);
  try
  var ExcludeMaskFolders := CreateMask(WildcardExcludeFolders);
  try
  var IncludeMaskFiles := CreateMask(WildCardIncludeFiles);
  try
  var ExcludeMaskFiles := CreateMask(WildCardExcludeFiles);
  try
    ScanFilesInternal(FilePath, '', IncludeMaskFolders, ExcludeMaskFolders, IncludeMaskFiles, ExcludeMaskFiles, OnFile, Recursive, ExcludeDefault);
  finally
    FreeMasks(ExcludeMaskFiles);
  end;
  finally
    FreeMasks(IncludeMaskFiles);
  end;
  finally
    FreeMasks(ExcludeMaskFolders);
  end;
  finally
    FreeMasks(IncludeMaskFolders);
  end;
end;



procedure LaunchFile(const FileName: string);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(FileName), '', '', SW_SHOWNORMAL);
{$ENDIF}
{$IFDEF POSIX}
  _system(PAnsiChar('open ' + UTF8Encode(FileName)));
{$ENDIF POSIX}
end;

function FindProcessUsingImpl(const FileName: string): string;
{$IFDEF MSWINDOWS}
var
  ROT : IRunningObjectTable;
  mFile, enumIndex, Prefix : IMoniker;
  enumMoniker : IEnumMoniker;
  MonikerType : LongInt;
  unkInt  : IInterface;
  pAppName: PWidechar;
begin
  Result := '';
  try
    OleCheck(GetRunningObjectTable(0, ROT));
    OleCheck(CreateFileMoniker(PWideChar(FileName), mFile));

    OleCheck(ROT.EnumRunning(enumMoniker));

    while (enumMoniker.Next(1, enumIndex, nil) = S_OK) do
    begin
      OleCheck(enumIndex.IsSystemMoniker(MonikerType));
      if MonikerType = MKSYS_FILEMONIKER then
      begin
        if Succeeded(mFile.CommonPrefixWith(enumIndex, Prefix)) and
           (mFile.IsEqual(Prefix) = S_OK) then
        begin
         if Succeeded(ROT.GetObject(enumIndex, unkInt)) then
          begin
            if Succeeded(unkInt.QueryInterface(IID_IFileIsInUse, result)) then
            begin
              var FileInUse := unkInt as IFileIsInUse;
              if Assigned(FileInUse) then
              begin
                OleCheck(FileInUse.GetAppName(pAppName));
                Result := pAppName;
                CoTaskMemFree(pAppName);
                exit;
              end;

              exit;
            end;
          end;
        end;
      end;
    end;
  except
    //Nothing, we just can't get the app name.
  end;

{$ELSE}
begin
  Result := '';
{$ENDIF}
end;

function FindProcessUsing(const FileName: string): string;
{$IFDEF MSWINDOWS}
begin
  try
    CoInitialize(nil);
    try
      Result := FindProcessUsingImpl(FileName);
    finally
      CoUninitialize; //We must call FindProcessUsingImpl in a diff method, so all Interfaces have been released when we call CoUninitialize.
    end;
  except
    //Nothing, we just can't get the app name.
  end;

{$ELSE}
begin
  Result := '';
{$ENDIF}
end;


function GuidToStringN(const Guid: TGuid): String;
begin
  SetLength(Result, 32);
  StrLFmt(PChar(Result), 32,'%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x',   // do not localize
    [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
    Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
end;

var
  CreateDirLock: TObject;

// TDirectory doesn't raise exceptions in all cases when it fails. See https://quality.embarcadero.com/browse/RSP-42432
// Until that's fixed, we will use a fixed version here.
// Also, ForceDirectories is not thread-safe, let's wrap it around a critical section.
procedure TDirectory_CreateDirectory(const Path: string);
begin
  TMonitor.Enter(CreateDirLock);
  try
    var FullPath := TPath.GetFullPath(Path);
    if not ForceDirectories(FullPath) then
    begin
      raise Exception.Create('Can''t create folder "' + Path + '".' + SysErrorMessage(GetLastError));
    end;
  finally
    TMonitor.Exit(CreateDirLock);
  end;
end;

  function _FindLockingApp(const FileName: string): string;
  begin
    var LockingApp := FindProcessUsing(FileName);
    Result := '';
    if LockingApp <> '' then Result := Format(' Locked by "%s".', [LockingApp]);
  end;

  procedure DeleteFileOrMoveToLocked(const LockFolder, FileName:      string);
  begin
    var DummyNeedsToRestartIDE := false;
    DeleteFileOrMoveToLocked(LockFolder, FileName, false, nil, DummyNeedsToRestartIDE);

  end;

  procedure RenameAndCheck(const SourceFile, DestFile: string);
  begin
    //do not use TFile.Move. It thinks R:\ <> r:\ and decides are different drives, trying to copy instead, and causing crashes here.
    //FileName and TempFileName should be in the same drive always, so there is never a need to copy.

    TDirectory_CreateDirectory(TPath.GetDirectoryName(DestFile));
    if not SysUtils.RenameFile(SourceFile, DestFile) then
      raise EInOutError.Create('Error trying to move "' + SourceFile + '" to "' + DestFile +'". ' + SysErrorMessage(GetLastError), SourceFile);
  end;

procedure RenameFilesOneByOne(SourceFolder, DestFolder: string);
var
  F: SysUtils.TSearchRec;
begin
  TDirectory_CreateDirectory(DestFolder);

  if SysUtils.FindFirst(TPath.Combine(SourceFolder, '*'), faAnyFile, F) = 0 then
  begin
    try
      repeat
        if (F.Attr and faDirectory <> 0) then
        begin
          if (F.Name <> '.') and (F.Name <> '..') then
          begin
            RenameFilesOneByOne(TPath.Combine(SourceFolder, F.Name), TPath.Combine(DestFolder, F.Name));
          end;
        end else
        begin
          var SourceFileName := TPath.Combine(SourceFolder, F.Name);
          var DestFileName := TPath.Combine(DestFolder, F.Name);
          if not SysUtils.RenameFile(SourceFileName, DestFileName) then
            raise EInOutError.Create('Error trying to move "' + SourceFileName + '" to "' + DestFileName +'". ' + SysErrorMessage(GetLastError), SourceFileName);
        end;
      until SysUtils.FindNext(F) <> 0;
    finally
      SysUtils.FindClose(F);
    end;
  end;
  SysUtils.RemoveDir(SourceFolder);
end;

procedure RenameAndCheckFolder(const SourceFolder, DestFolder: string);
begin
  //SysUtils.RenameFile(file) almost never fails. Doesn't matter if the file is locked and in use, whatever.
  //SysUtils.RenameFile(folder) is a different story. If any of the files inside the folder is locked, it will fail.
  //So we try first to rename all, but if it doesn't work, we rename the files individually.

  TDirectory_CreateDirectory(TPath.GetDirectoryName(DestFolder));
  //First try the simplest way. This will work unless something is locked.
  if SysUtils.RenameFile(SourceFolder, DestFolder) then exit;

  RenameFilesOneByOne(SourceFolder, DestFolder);

end;

procedure DeleteFileOrMoveToLocked(const LockFolder, FileName: string; const DryRun: boolean; const Log: TProc<string>; var NeedsToRestartIDE: boolean);
begin
  if FileName.Trim = '' then Exit;
  if TFile.Exists(FileName, false) then
  begin
    if not DryRun then
    begin
      if SysUtils.DeleteFile(FileName) then
      begin
        if (Assigned(Log)) then Log(Format('File "%s" deleted.', [FileName]));
      end
      else
      begin
        if LockFolder = '' then
        begin
          raise Exception.Create(Format('File "%s" seems to be locked but can''t be moved as we don''t have a valid LockFolder.', [FileName]));
        end
        else
        begin
          NeedsToRestartIDE := true;
          var TempFileName := TPath.Combine(LockFolder, TPath.GetFileName(FileName)) + '.' + GuidToStringN(TGUID.NewGuid) + TempExtension;
          var LockingApp := _FindLockingApp(FileName); // find the handle before moving it.
          RenameAndCheck(FileName, TempFileName);
          if (Assigned(Log)) then Log(Format('File "%s" seems to be locked. Moved to "%s"%s.', [FileName, TempFileName, LockingApp]));
        end;
      end;
    end;

  end;
end;

function AppIsAlreadyRunning(const Folder: string): boolean;
begin
  Result := false;

  {$IFDEF MSWINDOWS}
  // Name of the mutext must be at most 260 characters. So we use a hash, not the folder as name.
  // We also use Folder.ToUpper so c:\test is the same as C:\TEST. We are inside a IFDEF MSWINDOWS. For linux/mac we shouldn't do it.
  var FolderHash := THashSHA2.GetHashString(Folder.ToUpper);

  //We'll add a guid to the hash so we identify even better our app...
  //Global\ is to look into all users.
  if CreateMutex(nil, True, PChar('Global\tms-smart-setup-7E07C55C-D558-40B8-96F7-364B036CAE74-' + FolderHash)) = 0 then
    RaiseLastOSError;

  var LastError := GetLastError;
  if LastError = ERROR_ALREADY_EXISTS then
    Exit(true);
  {$ENDIF}
end;

procedure AddOnePath(const Result: TList<string>; const Folder: string; const ErrorInfo: string; const OnAdd: TProc<string, string, string>);
begin
  Result.Add(Folder);
  OnAdd(Result[0], Folder, ErrorInfo);
end;

procedure SplitPath(const Position: integer; const Folder: string; out Root, Pattern, Rest: string);
begin
  var p1 := Position;
  while (p1 > 0) and (Folder.Chars[p1] <> '/') and (Folder.Chars[p1] <> '\') do dec(p1);
  Root := Folder.Substring(0, p1);
  var p2 := Position;
  while (p2 < Folder.Length) and (Folder.Chars[p2] <> '/') and (Folder.Chars[p2] <> '\') do inc(p2);
  Pattern := Folder.Substring(p1 + 1, p2 - p1 - 1);
  Rest := Folder.Substring(p2 + 1);
end;

procedure AddPathsWithWildcards(const Result: TList<string>; const Folder: string; const ErrorInfo: string; const OnAdd: TProc<string, string, string>);
begin
  var p := Folder.IndexOfAny(['*', '?']); // Only supporting masks with * or ?
  if p < 0 then
  begin
    AddOnePath(Result, Folder, ErrorInfo, OnAdd);
    exit;
  end;

  var Root, Pattern, Rest: string;
  SplitPath(p, Folder, Root, Pattern, Rest);

  var MatchingFolders: TArray<string>;
  try
    MatchingFolders := TDirectory.GetDirectories(Root, Pattern, TSearchOption.soTopDirectoryOnly);
  except on ex: Exception do
    raise Exception.Create('Error finding paths: ' + ex.Message + '. ' + ErrorInfo);
  end;
  for var mf in MatchingFolders do
  begin
    AddPathsWithWildcards(Result, TPath.Combine(mf, Rest), ErrorInfo, OnAdd);
  end;

end;

function FolderIsOutside(const Folder: string; const RootFolders: TArray<string>): boolean;
begin
  Result := true;
  var FullFolder := IncludeTrailingPathDelimiter(TPath.GetFullPath(Folder));
  for var RootFolder in RootFolders do
  begin
    var FullRootFolder := IncludeTrailingPathDelimiter(TPath.GetFullPath(RootFolder));
    {$IFDEF MSWINDOWS}
    var IgnoreCase := true;
    {$ELSE}
    var IgnoreCase := false;
    {$ENDIF}
    if (FullFolder.StartsWith(FullRootFolder, IgnoreCase)) then exit(false);
  end;
end;

function CreateUUIDv5(const Namespace: TGUID; const Name: string): TGUID;
begin
  // UTF-8 encode name
  var NameBytes := TEncoding.UTF8.GetBytes(Name);

  // Concatenate and hash using SHA-1
  var LSHA1 := THashSHA1.Create;
  LSHA1.Update(Namespace.ToByteArray(TEndian.Big) + NameBytes);
  var Hash := LSHA1.HashAsBytes;

  // Truncate to 16 bytes
  SetLength(Hash, 16);

  // Set version to 5 (UUIDv5)
  Hash[6] := (Hash[6] and $0F) or $50;

  // Set variant to RFC 4122
  Hash[8] := (Hash[8] and $3F) or $80;

  // Convert hash to TGUID
  Result.D1 := Swap(PCardinal(@Hash[0])^);
  Result.D2 := Swap(PWord(@Hash[4])^);
  Result.D3 := Swap(PWord(@Hash[6])^);
  Move(Hash[8], Result.D4[0], 8);
end;


initialization
  CreateDirLock := TObject.Create;

finalization
  CreateDirLock.Free;

end.

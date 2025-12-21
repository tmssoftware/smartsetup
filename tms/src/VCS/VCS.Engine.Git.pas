unit VCS.Engine.Git;
{$I ../../tmssetup.inc}

interface
uses SysUtils, Classes, VCS.Engine.Virtual, Generics.Collections, Generics.Defaults;

type
  TGitEngine = class(TInterfacedObject, IVCSEngine)
  private
    const GitDefaultBranchFile = 'tms.defaultbranch.txt';
  private
    FGitCommandLine: string;
    FCloneCommand: string;
    FPullCommand: string;

    function GetEnvCommandLine: string;
    procedure CheckoutVersion(const aCloneFolder, aVersion: string; const Detach: boolean);
    function GetCurrentBranch(const aFolder: string): string;
    procedure SaveCurrentBranch(const aRootFolder, aRepoFolder: string);
    function LoadCurrentBranch(const aRootFolder: string): string;
    procedure AttachHead(const aRootFolder, aGitFolder: string);
  public
    constructor Create(const aGitCommandLine, aCloneCommand, aPullCommand: string);

    property GitCommandLine: string read FGitCommandLine;
    property CloneCommand: string read FCloneCommand;
    property PullCommand: string read FPullCommand;

    procedure Clone(const aCloneFolder, aURL, aVersion: string);
    procedure AfterClone(const aRootFolder, aCloneFolder: string);

    procedure Pull(const aRootFolder, aGitFolder, aVersion: string);
    function GetVersionNames(const aExistingRepoFolder, aTempFolder, aLockedFolder: string; const aURL: string): TArray<TVersionAndDate>;
    function GetProduct(const aDestFolderRoot, aDestFolder, aURL, aServer, aProductId, aVersion: string): boolean;
    function FileIsVersioned(const aFileName, aWorkingFolder: string): boolean;

    function GetCommitId(const aWorkingFolder: string): string;
    function IsRootVCSFolder(const Folder: string): boolean;

  end;

implementation
uses UWindowsPath, Deget.CommandLine, UMultiLogger, UTmsBuildSystemUtils, IOUtils,
     DateUtils, Testing.Globals;

{ TGitEngine }

constructor TGitEngine.Create(const aGitCommandLine, aCloneCommand, aPullCommand: string);
begin
  if aGitCommandLine.Trim = '' then FGitCommandLine := GetEnvCommandLine
  else FGitCommandLine := aGitCommandLine;

  if FGitCommandLine.Trim = '' then raise Exception.Create('Can''t find a valid git.exe specified in tms.config.yaml or the Windows PATH. Make sure you have git installed and configured.');

  if aCloneCommand.Trim = '' then FCloneCommand := 'clone' else FCloneCommand := aCloneCommand;
  if aPullCommand.Trim = '' then FPullCommand := 'pull --all' else FPullCommand := aPullCommand;

end;

function TGitEngine.FileIsVersioned(const aFileName, aWorkingFolder: string): boolean;
begin
  var Output := '';
  var FullCommand := '"' + GitCommandLine + '" ls-files "' + aFileName + '"';
  if not ExecuteCommand(FullCommand, TPath.GetFullPath(aWorkingFolder), Output, ['GIT_TERMINAL_PROMPT=0'])
    then raise Exception.Create('Error in git command: ' + FullCommand);

  Result := Output <> '';
end;

function TGitEngine.GetEnvCommandLine: string;
const
{$IFDEF MSWINDOWS}
  GitExe = 'git.exe';
{$ELSE}
  GitExe = 'git';
{$ENDIF}
begin
  var Path := GetLocalMachineWindowsPath;
  Result := FindExeInPath(Path, GitExe);
  if Result <> '' then exit;

  Path := GetUserWindowsPath;
  Result := FindExeInPath(Path, GitExe);
end;

function TGitEngine.GetProduct(const aDestFolderRoot, aDestFolder, aURL, aServer, aProductId, aVersion: string): boolean;
begin
  Result := false;
end;

function TGitEngine.GetVersionNames(const aExistingRepoFolder, aTempFolder, aLockedFolder: string; const aURL: string): TArray<TVersionAndDate>;
var
  FullCommand: string;
begin
{$IFDEF DEBUG}
  TestParameters.CheckOffline('TGitEngine.GetVersionNames');
{$ENDIF}
  var Output := '';

  var WorkingFolder := '';
  if TDirectory.Exists(aExistingRepoFolder) then
  begin
    FullCommand := '"' + GitCommandLine + '" fetch --all';
    if ExecuteCommand(FullCommand, aExistingRepoFolder, Output, ['GIT_TERMINAL_PROMPT=0'])
      then WorkingFolder := aExistingRepoFolder;
  end;

  if WorkingFolder = '' then
  begin
    WorkingFolder := aTempFolder;
    if TDirectory.Exists(aTempFolder) then DeleteFolderMovingToLocked(aLockedFolder, aTempFolder, true, false);
    TDirectory.CreateDirectory(aTempFolder);

    FullCommand := '"' + GitCommandLine + '" clone --bare --filter=tree:0 ' + ' "' + aURL + '" "' + WorkingFolder + '"';
    if not ExecuteCommand(FullCommand, WorkingFolder, Output, ['GIT_TERMINAL_PROMPT=0'])
      then raise Exception.Create('Error in git command: ' + FullCommand);
  end;

  //See https://stackoverflow.com/a/57901182
  FullCommand := '"' + GitCommandLine + '" tag -l --format="%(refname:short)%09%(if)%(committerdate:iso-strict)%(then)%(committerdate:iso-strict)%(else)%(*committerdate:iso-strict)%(end)"';
  if not ExecuteCommand(FullCommand, WorkingFolder, Output, ['GIT_TERMINAL_PROMPT=0'])
    then raise Exception.Create('Error in git command: ' + FullCommand);

  var Lines := Output.Split([#10]);
  var LineCount := Length(Lines);
  while (LineCount > 0) and (Lines[LineCount - 1].Trim = '') do dec(LineCount);
                            

  SetLength(Result, LineCount);
  for var i := Low(Result) to High(Result) do
  begin
    var idx := Lines[i].IndexOf(#9);
    if idx < 0 then raise Exception.Create('Internal error: Line "' + '" doesn''t have a separator.');

    Result[i].Version := Lines[i].Substring(0, idx).Trim;
    Result[i].Date := ISO8601ToDate(Lines[i].Substring(idx).Trim);
  end;

  var DateComparer: IComparer<TVersionAndDate> := TDelegatedComparer<TVersionAndDate>.Create(
    function(const Left, Right: TVersionAndDate): integer
    begin
      if Left.Date < Right.Date then
        Result := 1
      else if Left.Date > Right.Date then
        Result := -1
      else
        Result := 0;
    end);

  TArray.Sort<TVersionAndDate>(Result, DateComparer);

  try
    if TDirectory.Exists(aTempFolder) then DeleteFolderMovingToLocked(aLockedFolder, aTempFolder, true, false);
  except
    //ignore errors in the exception. This temp folder will be cleaned up next time you run tms anyway.
  end;
end;

function TGitEngine.IsRootVCSFolder(const Folder: string): boolean;
begin
  Result := TDirectory.Exists(TPath.Combine(Folder, '.git'));
end;

function TGitEngine.GetCommitId(const aWorkingFolder: string): string;
begin
  var FullCommand := '"' + GitCommandLine + '" rev-parse HEAD';
  if not ExecuteCommand(FullCommand, aWorkingFolder, Result, ['GIT_TERMINAL_PROMPT=0'])
    then exit('');

  Result := Result.Trim;
end;

function TGitEngine.GetCurrentBranch(const aFolder: string): string;
begin
  Result := '';
  var FullCommand := '"' + GitCommandLine + '" branch --show-current';
  if not ExecuteCommand(FullCommand, aFolder, Result, ['GIT_TERMINAL_PROMPT=0'])
    then raise Exception.Create('Error in git command: ' + FullCommand);
end;

procedure TGitEngine.SaveCurrentBranch(const aRootFolder, aRepoFolder: string);
begin
  TFile.WriteAllText(TPath.Combine(aRootFolder, GitDefaultBranchFile), GetCurrentBranch(aRepoFolder), TUTF8NoBOMEncoding.Instance);
end;

function TGitEngine.LoadCurrentBranch(const aRootFolder: string): string;
begin
  var DefaultBranchFile := TPath.Combine(aRootFolder, GitDefaultBranchFile);
  if not TFile.Exists(DefaultBranchFile) then exit('');

  Result := TFile.ReadAllText(DefaultBranchFile, TUTF8NoBOMEncoding.Instance).Trim;
end;

procedure TGitEngine.AttachHead(const aRootFolder, aGitFolder: string);
begin
  var BranchName := LoadCurrentBranch(aRootFolder);
  if BranchName = '' then exit;
  CheckoutVersion(aGitFolder, BranchName, false);
end;

procedure TGitEngine.CheckoutVersion(const aCloneFolder, aVersion: string; const Detach: boolean);
begin
  if (aVersion.Trim = '') or (aVersion.Trim = '*') then exit;

  //git clean removes untracked files. It removes tmsbuild.yaml too if not in the repo, and tmsfetch.info.
  //git reset --hard removes tracked files, leaves untracked.
  //git switch/checkout - will exit detached head

  var DetachStr := '';
  if Detach then DetachStr :=  ' --detach';
  
  var Output := '';
  var FullCommand := '"' + GitCommandLine + '" switch' + DetachStr + ' "' + aVersion + '"'; //add a --force to wipe local changes if we prefer it that way.
  if not ExecuteCommand(FullCommand, aCloneFolder, Output, ['GIT_TERMINAL_PROMPT=0'])
    then
    begin
      raise Exception.Create('Can''t find the version "' + aVersion + '" in product ' + TPath.GetFileName(aCloneFolder) + '. Verify it is a valid tag, branch name or commit id.');
    end;
end;


procedure TGitEngine.Clone(const aCloneFolder, aURL, aVersion: string);
begin
{$IFDEF DEBUG}
  TestParameters.CheckOffline('TGitEngine.Clone');
{$ENDIF}
  var Output := '';
  var CloneFolder := TPath.GetFullPath(aCloneFolder);
  var FullCommand := '"' + GitCommandLine + '" ' + CloneCommand + ' "' + aURL + '" "' + CloneFolder + '"';
  if DirectoryExists(CloneFolder) then raise Exception.Create('Can''t git clone into an existing folder: "' + CloneFolder + '"');
  TDirectory_CreateDirectory(CloneFolder);
  if not ExecuteCommand(FullCommand, CloneFolder, Output, ['GIT_TERMINAL_PROMPT=0'])
    then raise Exception.Create('Error cloning "' + aUrl + '" into ' + CloneFolder);

  SaveCurrentBranch(aCloneFolder, aCloneFolder);
  CheckoutVersion(CloneFolder, aVersion, true);
end;

procedure TGitEngine.AfterClone(const aRootFolder, aCloneFolder: string);
begin
  DeleteFile(TPath.Combine(aRootFolder, GitDefaultBranchFile));
  RenameAndCheck(TPath.Combine(aCloneFolder, GitDefaultBranchFile), TPath.Combine(aRootFolder, GitDefaultBranchFile));
end;


procedure TGitEngine.Pull(const aRootFolder, aGitFolder, aVersion: string);
begin
{$IFDEF DEBUG}
  TestParameters.CheckOffline('TGitEngine.Pull');
{$ENDIF}
  var Output := '';
  var GitFolder := TPath.GetFullPath(aGitFolder);
  AttachHead(TPath.GetFullPath(aRootFolder), GitFolder);
  var FullCommand := '"' + GitCommandLine + '" ' + PullCommand;
  if not ExecuteCommand(FullCommand, GitFolder, Output, ['GIT_TERMINAL_PROMPT=0'])
    then raise Exception.Create('Error in git pull "' +  GitFolder + '"');
  CheckoutVersion(aGitFolder, aVersion, true);
end;

end.

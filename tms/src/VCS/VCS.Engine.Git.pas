unit VCS.Engine.Git;
{$I ../../tmssetup.inc}

interface
uses SysUtils, Classes, VCS.Engine.Virtual, Generics.Collections, Generics.Defaults;

type
  TGitEngine = class(TInterfacedObject, IVCSEngine)
  private
    const GitDefaultBranchFile = 'tms.defaultbranch.txt';
  private
    type TBranchKind = (NotABranch, LocalBranch, RemoteOnlyBranch);
  private
    FGitCommandLine: string;
    FCloneCommand: string;
    FPullCommand: string;
    FSkipSubModules: boolean;

    function GetEnvCommandLine: string;
    function GetBranchKind(const aCloneFolder, aVersion: string): TBranchKind;
    procedure CleanAndReset(const aCloneFolder: string);
    procedure CheckoutVersion(const aCloneFolder, aVersion: string; const Detach, Clean: boolean);
    function GetCurrentBranch(const aFolder: string): string;
    procedure SaveCurrentBranch(const aRootFolder, aRepoFolder: string);
    function LoadCurrentBranch(const aRootFolder: string): string;
    procedure AttachHead(const aRootFolder, aGitFolder: string);
    function RemoteHasChanges(const aRootFolder, aGitFolder, aVersion: string): boolean;
    function GetBestTag(const Tags: string): string;
    function LooksLikeVersion(Tag: string): boolean;
    function GetGitCommandLine: string;
    function RunGit(const aFolder, Parameters: string; const ExtraErrorMsg: string = ''): string;
    function TryRunGit(const aFolder, Parameters: string; out Message: string; const FailureIsOk: boolean = false): boolean;
    function GetFullCommand(const Parameters: string): string;
  public
    constructor Create(const aGitCommandLine, aCloneCommand, aPullCommand: string; const aSkipSubModules: boolean);

    property GitCommandLine: string read GetGitCommandLine;
    property CloneCommand: string read FCloneCommand;
    property PullCommand: string read FPullCommand;
    property SkipSubModules: boolean read FSkipSubModules;

    procedure Clone(const aCloneFolder, aURL, aVersion: string);
    procedure AfterClone(const aRootFolder, aCloneFolder: string);

    procedure Pull(const aRootFolder, aGitFolder, aVersion: string);
    function GetVersionNames(const aExistingRepoFolder, aTempFolder, aLockedFolder: string; const aURL: string): TArray<TVersionAndDate>;
    function GetProduct(const aDestFolderRoot, aDestFolder, aURL, aServer, aProductId, aVersion: string): boolean;
    function FileIsVersioned(const aFileName, aWorkingFolder: string): boolean;

    function GetCommitId(const aWorkingFolder: string; const allowTags: boolean): string;
    function IsRootVCSFolder(const Folder: string): boolean;

  end;

implementation
uses UWindowsPath, Deget.CommandLine, UMultiLogger, UTmsBuildSystemUtils, IOUtils,
     DateUtils, Testing.Globals, Character, VCS.Sanitizer;

{ TGitEngine }

constructor TGitEngine.Create(const aGitCommandLine, aCloneCommand, aPullCommand: string; const aSkipSubModules: boolean);
begin
  if aGitCommandLine.Trim = '' then FGitCommandLine := GetEnvCommandLine
  else FGitCommandLine := aGitCommandLine;

  if aCloneCommand.Trim = '' then FCloneCommand := 'clone' else FCloneCommand := aCloneCommand;
  if aPullCommand.Trim = '' then FPullCommand := 'pull --all' else FPullCommand := aPullCommand;

  FSkipSubModules := aSkipSubModules;
end;

function TGitEngine.FileIsVersioned(const aFileName, aWorkingFolder: string): boolean;
begin
  ValidateVCSFilePath(aFileName);
  var Output := RunGit(TPath.GetFullPath(aWorkingFolder), 'ls-files -- "' + aFileName + '"');

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
{$IFDEF DEBUG}
  if TestParameters.NoGit then exit('');
{$ENDIF}
  var Path := GetLocalMachineWindowsPath;
  Result := FindExeInPath(Path, GitExe);
  if Result <> '' then exit;

  Path := GetUserWindowsPath;
  Result := FindExeInPath(Path, GitExe);
end;

function TGitEngine.GetGitCommandLine: string;
begin
  if FGitCommandLine.Trim = '' then raise Exception.Create('Can''t find a valid git.exe specified in tms.config.yaml or the Windows PATH. Make sure you have git installed and configured.');
  Result := FGitCommandLine;
end;

function TGitEngine.GetFullCommand(const Parameters: string): string;
begin
  //core.symlinks=false doesn't allow links.
  //We pass core.symlinks as an env variable, but also as a command line param for plats other than windows where env vars are ignored.
  var Cmd := QuoteCmd(GitCommandLine) + ' -c core.symlinks=false ';
  Result := Cmd + Parameters;
end;

function TGitEngine.TryRunGit(const aFolder, Parameters: string; out Message: String; const FailureIsOk: boolean = false): boolean;
begin
  Message := '';
  Result := ExecuteCommand(GetFullCommand(Parameters), aFolder, Message,
      ['GIT_TERMINAL_PROMPT=0','GIT_CONFIG_COUNT=1',
       'GIT_CONFIG_KEY_0=core.symlinks',
       'GIT_CONFIG_VALUE_0=false'],  FailureIsOk);
end;

function TGitEngine.RunGit(const aFolder, Parameters: string; const ExtraErrorMsg: string = ''): string;
begin
  Result := '';
  var ExtraMsg := ExtraErrorMsg;
  if ExtraMsg <> '' then ExtraMsg := '. ' + ExtraMsg;
  if not TryRunGit(aFolder, Parameters, Result)
    then raise Exception.Create('Error in git command: ' + RedactUrlCredentials(GetFullCommand(Parameters)) + ' Working folder: "' + aFolder + '"' + ExtraMsg);
end;

function TGitEngine.GetProduct(const aDestFolderRoot, aDestFolder, aURL, aServer, aProductId, aVersion: string): boolean;
begin
  Result := false;
end;

function TGitEngine.GetVersionNames(const aExistingRepoFolder, aTempFolder, aLockedFolder: string; const aURL: string): TArray<TVersionAndDate>;
begin
{$IFDEF DEBUG}
  TestParameters.CheckOffline('TGitEngine.GetVersionNames');
{$ENDIF}
  var Output := '';

  var WorkingFolder := '';
  if TDirectory.Exists(aExistingRepoFolder) then
  begin
    if TryRunGit(aExistingRepoFolder, 'fetch --all', Output, true)
      then WorkingFolder := aExistingRepoFolder;
  end;

  if WorkingFolder = '' then
  begin
    WorkingFolder := aTempFolder;
    if TDirectory.Exists(aTempFolder) then DeleteFolderMovingToLocked(aLockedFolder, aTempFolder, true, false);
    TDirectory.CreateDirectory(aTempFolder);

    ValidateVCSUrl(aURL);
    //'--' terminates option parsing so that a URL or folder starting with '-' cannot be interpreted as a git option.
    RunGit(WorkingFolder, 'clone --bare --filter=tree:0 -- "' + aURL + '" "' + WorkingFolder + '"');
  end;

  //See https://stackoverflow.com/a/57901182
  Output := RunGit(WorkingFolder, 'tag -l --format="%(refname:short)%09%(if)%(committerdate:iso-strict)%(then)%(committerdate:iso-strict)%(else)%(*committerdate:iso-strict)%(end)"');

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

function TGitEngine.LooksLikeVersion(Tag: string): boolean;
begin
  Result := false;
  var LastIdx := 0;
  while (True) do
  begin
    var Idx := Tag.IndexOf('.', LastIdx);
    LastIdx := Idx + 1;
    if Idx < 0 then exit;
    if (Idx > 0) and (Idx < Length(Tag) - 1) and Char.IsNumber(Tag, Idx - 1) and Char.IsNumber(Tag, Idx + 1) then exit(true);

  end;
end;

function TGitEngine.GetBestTag(const Tags: string): string;
begin
  var ParsedTags := Tags.Split([#10], TStringSplitOptions.ExcludeEmpty);
  for var Tag in ParsedTags do
  begin
    if LooksLikeVersion(Tag) then exit(Tag.Trim);
  end;
  if Length(ParsedTags) > 0 then exit(ParsedTags[0].Trim);
  Result := '';

end;


function TGitEngine.GetCommitId(const aWorkingFolder: string; const allowTags: boolean): string;
begin
  if allowTags then
  begin
    if TryRunGit(aWorkingFolder, 'tag --points-at HEAD', Result, true) then
    begin
      Result := GetBestTag(Result.Trim);
      if Result <> '' then exit;
    end;
  end;

  if not TryRunGit(aWorkingFolder, 'rev-parse HEAD', Result, true)
    then exit('');

  Result := Result.Trim;
end;

function TGitEngine.GetCurrentBranch(const aFolder: string): string;
begin
  Result := RunGit(aFolder, 'branch --show-current');
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
  CheckoutVersion(aGitFolder, BranchName, false, true);
end;

function TGitEngine.GetBranchKind(const aCloneFolder, aVersion: string): TBranchKind;
begin
  var Output := '';
  if TryRunGit(aCloneFolder, 'show-ref --verify --quiet -- "refs/heads/' + aVersion + '"', Output, true)
    then exit(TBranchKind.LocalBranch);

  //We always clone the repos ourselves, so the remote is always named origin.
  if TryRunGit(aCloneFolder, 'show-ref --verify --quiet -- "refs/remotes/origin/' + aVersion + '"', Output, true)
    then exit(TBranchKind.RemoteOnlyBranch);

  Result := TBranchKind.NotABranch;
end;

procedure TGitEngine.CleanAndReset(const aCloneFolder: string);
begin
  //git clean removes untracked files. It removes tmsbuild.yaml too if not in the repo, and tmsfetch.info.
  //git reset --hard removes tracked files, leaves untracked.

  RunGit(aCloneFolder, 'clean -fdx', 'Can''t clean existing repository. You might need to remove the folders and re-install.');
  RunGit(aCloneFolder, 'reset --hard', 'Can''t reset existing repository. You might need to remove the folders and re-install.');
end;

procedure TGitEngine.CheckoutVersion(const aCloneFolder, aVersion: string; const Detach, Clean: boolean);
begin
  if (aVersion.Trim = '') or (aVersion.Trim = '*') then exit;
  ValidateVCSVersion(aVersion);

  if Clean then CleanAndReset(aCloneFolder);

  //git switch/checkout - will exit detached head
  //git switch --detach fails for a branch that only exists in the remote, like "feature" when
  //only origin/feature exists. And for a local branch we want to stay on it, not detach at its commit.
  var BranchKind := GetBranchKind(aCloneFolder, aVersion);
  var DetachStr := '';
  if Detach and (BranchKind = TBranchKind.NotABranch) then DetachStr :=  ' --detach';

  var FullParameters := '';
  if BranchKind = TBranchKind.RemoteOnlyBranch then
    //Create the local branch tracking origin's. ValidateVCSVersion guarantees aVersion doesn't start with '-'.
    FullParameters := 'switch --create "' + aVersion + '" "origin/' + aVersion + '"'
  else
    //'--' terminates option parsing so that a version starting with '-' cannot be interpreted as a git option.
    FullParameters := 'switch' + DetachStr + ' -- "' + aVersion + '"'; //add a --force to wipe local changes if we prefer it that way.

  RunGit(aCloneFolder, FullParameters,
    'Can''t find the version "' + aVersion + '" in product ' + TPath.GetFileName(aCloneFolder) + '. Verify it is a valid tag, branch name or commit id.');
end;


procedure TGitEngine.Clone(const aCloneFolder, aURL, aVersion: string);
begin
{$IFDEF DEBUG}
  TestParameters.CheckOffline('TGitEngine.Clone');
{$ENDIF}
  ValidateVCSUrl(aURL);
  ValidateVCSVersion(aVersion);
  var Output := '';
  var CloneFolder := TPath.GetFullPath(aCloneFolder);
  var SubModules := ' --recurse-submodules';
  if SkipSubModules then SubModules := '';

  //'--' terminates option parsing so that a URL or folder starting with '-' cannot be interpreted as a git option.
  if DirectoryExists(CloneFolder) then raise Exception.Create('Can''t git clone into an existing folder: "' + CloneFolder + '"');
  TDirectory_CreateDirectory(CloneFolder);
  RunGit(CloneFolder, CloneCommand + SubModules + ' -- "' + aURL + '" "' + CloneFolder + '"');

  SaveCurrentBranch(aCloneFolder, aCloneFolder);
  CheckoutVersion(CloneFolder, aVersion, true, false);
end;

procedure TGitEngine.AfterClone(const aRootFolder, aCloneFolder: string);
begin
  DeleteFile(TPath.Combine(aRootFolder, GitDefaultBranchFile));
  RenameAndCheck(TPath.Combine(aCloneFolder, GitDefaultBranchFile), TPath.Combine(aRootFolder, GitDefaultBranchFile));
end;


function TGitEngine.RemoteHasChanges(const aRootFolder, aGitFolder, aVersion: string): boolean;
begin
  //Returns false when the commit we would end up on after pulling and checking out aVersion is the
  //one we are already on. In that case there is nothing to update and we can skip the pull, avoiding
  //the clean/reset that would erase the compiled files (dcus, etc.) and force a needless rebuild.
  Result := true;

  var CurrentCommit := '';
  if not TryRunGit(aGitFolder, 'rev-parse HEAD', CurrentCommit, true) then exit;
  CurrentCommit := CurrentCommit.Trim;
  if CurrentCommit = '' then exit;

  //When no version (or '*') is requested we stay on the default branch, just like AttachHead does.
  var Version := aVersion.Trim;
  if (Version = '') or (Version = '*') then Version := LoadCurrentBranch(aRootFolder);
  if Version = '' then exit;
  ValidateVCSVersion(Version);

  //For a branch the pull would move us to its remote tip (origin/<branch>); a tag or commit is fixed.
  var TargetRef := Version;
  if GetBranchKind(aGitFolder, Version) <> TBranchKind.NotABranch then TargetRef := 'origin/' + Version;

  var TargetCommit := '';
  if not TryRungit(aGitFolder, 'rev-parse "' + TargetRef + '"', TargetCommit, true) then exit;
  TargetCommit := TargetCommit.Trim;
  if TargetCommit = '' then exit;

  Result := not SameText(CurrentCommit, TargetCommit);
end;

procedure TGitEngine.Pull(const aRootFolder, aGitFolder, aVersion: string);
begin
{$IFDEF DEBUG}
  TestParameters.CheckOffline('TGitEngine.Pull');
{$ENDIF}
  var GitFolder := TPath.GetFullPath(aGitFolder);
  var RootFolder := TPath.GetFullPath(aRootFolder);

  //Fetch first so we can tell whether the remote actually has changes. Fetch only updates the
  //remote-tracking refs, it doesn't touch the working tree, so the compiled output is preserved.
  RunGit(GitFolder, 'fetch --all');

  //If there are no updates, skip the attach-head/pull/checkout dance. That dance cleans the working
  //tree (CleanAndReset), which would erase the compiled files and force a rebuild even with no changes.
  if not RemoteHasChanges(RootFolder, GitFolder, aVersion) then exit;

  AttachHead(RootFolder, GitFolder);
  var SubModules := ' --recurse-submodules';
  if SkipSubModules then SubModules := '';

  RunGit(GitFolder, PullCommand + SubModules);
  CheckoutVersion(GitFolder, aVersion, true, false); //already cleaned in AttachHead above.
end;

end.

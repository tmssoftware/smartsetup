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
    function GetVersionNames(const aURL: string): TArray<string>;
    function GetProduct(const aDestFolderRoot, aDestFolder, aURL, aServer, aProductId, aVersion: string): boolean;
  end;

implementation
uses UWindowsPath, Deget.CommandLine, UMultiLogger, UTmsBuildSystemUtils, IOUtils;

{ TGitEngine }

constructor TGitEngine.Create(const aGitCommandLine, aCloneCommand, aPullCommand: string);
begin
  if aGitCommandLine.Trim = '' then FGitCommandLine := GetEnvCommandLine
  else FGitCommandLine := aGitCommandLine;

  if FGitCommandLine.Trim = '' then raise Exception.Create('Can''t find a valid git.exe specified in tms.config.yaml or the Windows PATH. Make sure you have git installed and configured.');

  if aCloneCommand.Trim = '' then FCloneCommand := 'clone' else FCloneCommand := aCloneCommand;
  if aPullCommand.Trim = '' then FPullCommand := 'pull --all' else FPullCommand := aPullCommand;

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

function TGitEngine.GetVersionNames(const aURL: string): TArray<string>;
begin
  var Output := '';
  var FullCommand := '"' + GitCommandLine + '" ls-remote --tags ' + aUrl;
  if not ExecuteCommand(FullCommand, '', Output, ['GIT_TERMINAL_PROMPT=0'])
    then raise Exception.Create('Error in git command: ' + FullCommand);

  // parse result
  var Tags := TStringList.Create;
  try
    var Lines := TStringList.Create;
    try
      Lines.Text := Output;
      for var Line in Lines do
      begin
        var P := Pos('refs/tags/', Line);
        var Tag := Copy(Line, P + Length('refs/tags/'));
        if Tag.EndsWith('^{}') then
          Tag := Copy(Tag, 1, Length(Tag) - 3);
        if Tags.IndexOf(Tag) = -1 then
          Tags.Add(Tag);
      end;
    finally
      Lines.Free;
    end;
    Tags.Sort;
    SetLength(Result, Tags.Count);
    for var I := 0 to Tags.Count - 1 do
      Result[I] := Tags[Tags.Count - 1 - I];
  finally
    Tags.Free;
  end;
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
    then raise Exception.Create('Error in git command: ' + FullCommand);
end;


procedure TGitEngine.Clone(const aCloneFolder, aURL, aVersion: string);
begin
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
  RenameAndCheck(TPath.Combine(aCloneFolder, GitDefaultBranchFile), TPath.Combine(aRootFolder, GitDefaultBranchFile));
end;


procedure TGitEngine.Pull(const aRootFolder, aGitFolder, aVersion: string);
begin
  var Output := '';
  var GitFolder := TPath.GetFullPath(aGitFolder);
  AttachHead(TPath.GetFullPath(aRootFolder), GitFolder);
  var FullCommand := '"' + GitCommandLine + '" ' + PullCommand;
  if not ExecuteCommand(FullCommand, GitFolder, Output, ['GIT_TERMINAL_PROMPT=0'])
    then raise Exception.Create('Error in git pull "' +  GitFolder + '"');
  CheckoutVersion(aGitFolder, aVersion, true);
end;

end.

unit VCS.Engine.Git;
{$I ../../tmssetup.inc}

interface
uses SysUtils, VCS.Engine.Virtual;

type
  TGitEngine = class(TInterfacedObject, IVCSEngine)
  private
    FGitCommandLine: string;
    FCloneCommand: string;
    FPullCommand: string;
    FCheckoutCommand: string;
    FShallowCloneCommand: string;

    function GetEnvCommandLine: string;
  public
    constructor Create(const aGitCommandLine, aCloneCommand, aPullCommand, aCheckoutCommand, aShallowCloneCommand: string);

    property GitCommandLine: string read FGitCommandLine;
    property CloneCommand: string read FCloneCommand;
    property PullCommand: string read FPullCommand;
    property CheckoutCommand: string read FCheckoutCommand;
    property ShallowCloneCommand: string read FShallowCloneCommand;

    procedure Clone(const  aCloneFolder, aURL: string);
    procedure Pull(const aFolder: string);
    procedure GetFile(const aFileName, aDestFolder, aURL, aServer: string);
    function GetProduct(const aDestFolderRoot, aDestFolder, aURL, aServer, aProductId: string): boolean;
  end;

implementation
uses UWindowsPath, Deget.CommandLine, UMultiLogger, UTmsBuildSystemUtils, IOUtils;

{ TGitEngine }

constructor TGitEngine.Create(const aGitCommandLine, aCloneCommand, aPullCommand, aCheckoutCommand, aShallowCloneCommand: string);
begin
  if aGitCommandLine.Trim = '' then FGitCommandLine := GetEnvCommandLine
  else FGitCommandLine := aGitCommandLine;

  if FGitCommandLine.Trim = '' then raise Exception.Create('Can''t find a valid git.exe specified in tms.config.yaml or the Windows PATH. Make sure you have git installed and configured.');

  if aCloneCommand.Trim = '' then FCloneCommand := 'clone' else FCloneCommand := aCloneCommand;
  if aPullCommand.Trim = '' then FPullCommand := 'pull' else FPullCommand := aPullCommand;
  if aCheckoutCommand.Trim = '' then FCheckoutCommand := 'checkout HEAD --' else FCheckoutCommand := aCheckoutCommand;

  //Instead of doing a clone --no-checkout and then a checkout tmsbuild.yaml, we will instead
  //checkout only the root folder (--sparse) and only the current state (--depth 1). This allows us
  //to use --filter=blob:none which makes the checkout way faster in huge repos (see for example castle engine)
  //With our old approach of --no-checkout we couldn't use the --filter=blob:none. This new approach has the problem that
  //it could be slow if the root folder is huge, but we can't have everything.
  if aShallowCloneCommand = '' then FShallowCloneCommand := 'clone --depth 1 --filter=blob:none --sparse';
  
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

procedure TGitEngine.GetFile(const aFileName, aDestFolder, aURL, aServer: string);
begin
  // https://stackoverflow.com/questions/2466735/how-to-sparsely-checkout-only-one-single-file-from-a-git-repository
  var Output := '';
  var CloneFolder := TPath.GetFullPath(aDestFolder);
  var FullCloneCommand := '"' + GitCommandLine + '" ' + ShallowCloneCommand + ' "' + aURL + '" "' + CloneFolder + '"';
  if DirectoryExists(CloneFolder) then raise Exception.Create('Can''t git clone into an existing folder: "' + CloneFolder + '"');
  TDirectory_CreateDirectory(CloneFolder);
  if not ExecuteCommand(FullCloneCommand, CloneFolder, Output, ['GIT_TERMINAL_PROMPT=0'])
    then raise Exception.Create('Error cloning "' + aUrl + '" into ' + CloneFolder);

  if not TFile.Exists(TPath.Combine(aDestFolder, aFileName))
    then raise Exception.Create('Error: The file "' +  aFileName + '" doesn''t exist at the root of "' + aUrl + '". Probably this repository is not enabled for smart setup.');


end;

function TGitEngine.GetProduct(const aDestFolderRoot, aDestFolder, aURL, aServer, aProductId: string): boolean;
begin
  Result := false;
end;

procedure TGitEngine.Clone(const aCloneFolder, aURL: string);
begin
  var Output := '';
  var CloneFolder := TPath.GetFullPath(aCloneFolder);
  var FullCommand := '"' + GitCommandLine + '" ' + CloneCommand + ' "' + aURL + '" "' + CloneFolder + '"';
  if DirectoryExists(CloneFolder) then raise Exception.Create('Can''t git clone into an existing folder: "' + CloneFolder + '"');
  TDirectory_CreateDirectory(CloneFolder);
  if not ExecuteCommand(FullCommand, CloneFolder, Output, ['GIT_TERMINAL_PROMPT=0'])
    then raise Exception.Create('Error cloning "' + aUrl + '" into ' + CloneFolder);
end;

procedure TGitEngine.Pull(const aFolder: string);
begin
  var Output := '';
  var Folder := TPath.GetFullPath(aFolder);
  var FullCommand := '"' + GitCommandLine + '" ' + PullCommand;
  if not ExecuteCommand(FullCommand, Folder, Output, ['GIT_TERMINAL_PROMPT=0'])
    then raise Exception.Create('Error in git pull "' +  Folder + '"');
end;

end.

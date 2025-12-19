unit VCS.Engine.Svn;
{$I ../../tmssetup.inc}

interface
uses SysUtils, VCS.Engine.Virtual;

type
  TSvnEngine = class(TInterfacedObject, IVCSEngine)
  private
    FSvnCommandLine: string;
    FCloneCommand: string;
    FPullCommand: string;

    function GetEnvCommandLine: string;
  public
    constructor Create(const aSvnCommandLine, aCloneCommand, aPullCommand: string);

    property SvnCommandLine: string read FSvnCommandLine;
    property CloneCommand: string read FCloneCommand;
    procedure AfterClone(const aRootFolder, aCloneFolder: string);

    property PullCommand: string read FPullCommand;
    function GetVersionNames(const aExistingRepoFolder, aTempFolder, aLockedFolder: string; const aURL: string): TArray<TVersionAndDate>;

    procedure Clone(const aCloneFolder, aURL, aVersion: string);
    procedure Pull(const aRootFolder, aGitFolder, aVersion: string);
    function GetProduct(const aDestFolderRoot, aDestFolder, aURL, aServer, aProductId, aVersion: string): boolean;
    function FileIsVersioned(const aFileName, aWorkingFolder: string): boolean;

    function GetCommitId(const aWorkingFolder: string): string;
    function IsRootVCSFolder(const Folder: string): boolean;
  end;

implementation
uses UWindowsPath, Deget.CommandLine, UMultiLogger, UTmsBuildSystemUtils, IOUtils, Testing.Globals;

{ TSvnEngine }

constructor TSvnEngine.Create(const aSvnCommandLine, aCloneCommand, aPullCommand: string);
begin
  if aSvnCommandLine.Trim = '' then FSvnCommandLine := GetEnvCommandLine
  else FSvnCommandLine := aSvnCommandLine;

  if FSvnCommandLine.Trim = '' then raise Exception.Create('Can''t find a valid svn.exe specified in tms.config.yaml or the Windows PATH. Make sure you have svn installed and configured.');

  if aCloneCommand.Trim = '' then FCloneCommand := 'checkout' else FCloneCommand := aCloneCommand;
  if aPullCommand.Trim = '' then FPullCommand := 'update' else FPullCommand := aPullCommand;
end;

function TSvnEngine.FileIsVersioned(const aFileName,
  aWorkingFolder: string): boolean;
begin
  if not TFile.Exists(aFileName) then exit(false); //svn status will return empty, not ? if the file doesn't exist.

  var Output := '';
  var Folder := TPath.GetFullPath(aWorkingFolder);
  var FullCommand := '"' + SvnCommandLine + '" --non-interactive status "' + aFileName + '"';
  if not ExecuteCommand(FullCommand, Folder, Output)
    then raise Exception.Create('Error doing svn status in "' + aFileName + '"');
  Result := not Output.Trim.StartsWith('?');
end;

function TSvnEngine.GetCommitId(const aWorkingFolder: string): string;
begin
  Result := '';
end;

function TSvnEngine.GetEnvCommandLine: string;
const
{$IFDEF MSWINDOWS}
  SvnExe = 'svn.exe';
{$ELSE}
  SvnExe = 'svn';
{$ENDIF}
begin
  var Path := GetLocalMachineWindowsPath;
  Result := FindExeInPath(Path, SvnExe);
  if Result <> '' then exit;

  Path := GetUserWindowsPath;
  Result := FindExeInPath(Path, SvnExe);
end;

function TSvnEngine.GetProduct(const aDestFolderRoot, aDestFolder, aURL, aServer, aProductId, aVersion: string): boolean;
begin
  Result := false;
end;

function TSvnEngine.GetVersionNames(const aExistingRepoFolder, aTempFolder, aLockedFolder: string; const aURL: string): TArray<TVersionAndDate>;
begin
  raise Exception.Create('GetVersionNames not supported in SVN protocol.');
end;

function TSvnEngine.IsRootVCSFolder(const Folder: string): boolean;
begin
  Result := TDirectory.Exists(TPath.Combine(Folder, '.svn'));
end;

procedure TSvnEngine.AfterClone(const aRootFolder, aCloneFolder: string);
begin

end;

procedure TSvnEngine.Clone(const aCloneFolder, aURL, aVersion: string);
begin
{$IFDEF DEBUG}
  TestParameters.CheckOffline('TSvnEngine.Clone');
{$ENDIF}

  if aVersion <> '' then raise Exception.Create('Versioning not supported in SVN protocol.');

  var Output := '';
  var CloneFolder := TPath.GetFullPath(aCloneFolder);
  var FullCommand := '"' + SvnCommandLine + '" --non-interactive ' + CloneCommand + ' "' + aURL + '" "' + CloneFolder + '"';
  if DirectoryExists(CloneFolder) then raise Exception.Create('Can''t svn checkout into an existing folder: "' + CloneFolder + '"');
  TDirectory_CreateDirectory(CloneFolder);
  if not ExecuteCommand(FullCommand, CloneFolder, Output)
    then raise Exception.Create('Error doing svn checkout from "' + aUrl + '" into ' + aCloneFolder);
end;

procedure TSvnEngine.Pull(const aRootFolder, aGitFolder, aVersion: string);
begin
  if aVersion <> '' then raise Exception.Create('Versioning not supported in SVN protocol.');
{$IFDEF DEBUG}
  TestParameters.CheckOffline('TSvnEngine.Pull');
{$ENDIF}

  var Output := '';
  var Folder := TPath.GetFullPath(aGitFolder);
  var FullCommand := '"' + SvnCommandLine + '" --non-interactive ' + PullCommand;
  if not ExecuteCommand(FullCommand, Folder, Output)
    then raise Exception.Create('Error doing svn update in "' + aGitFolder + '"');
end;

end.

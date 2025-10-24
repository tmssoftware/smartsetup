unit VCS.Engine.Git;
{$I ../../tmssetup.inc}

interface
uses SysUtils, Classes, VCS.Engine.Virtual, Generics.Collections, Generics.Defaults;

type
  TGitEngine = class(TInterfacedObject, IVCSEngine)
  private
    FGitCommandLine: string;
    FCloneCommand: string;
    FPullCommand: string;

    function GetEnvCommandLine: string;
  public
    constructor Create(const aGitCommandLine, aCloneCommand, aPullCommand: string);

    property GitCommandLine: string read FGitCommandLine;
    property CloneCommand: string read FCloneCommand;
    property PullCommand: string read FPullCommand;

    procedure Clone(const  aCloneFolder, aURL: string);
    procedure Pull(const aFolder: string);
    function GetVersionNames(const aURL: string): TArray<string>;
    function GetProduct(const aDestFolderRoot, aDestFolder, aURL, aServer, aProductId: string): boolean;
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
  if aPullCommand.Trim = '' then FPullCommand := 'pull' else FPullCommand := aPullCommand;

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

function TGitEngine.GetProduct(const aDestFolderRoot, aDestFolder, aURL, aServer, aProductId: string): boolean;
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

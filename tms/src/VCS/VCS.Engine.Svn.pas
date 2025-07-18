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
    FExportCommand: string;

    function GetEnvCommandLine: string;
  public
    constructor Create(const aSvnCommandLine, aCloneCommand, aPullCommand, aExportCommand: string);

    property SvnCommandLine: string read FSvnCommandLine;
    property CloneCommand: string read FCloneCommand;
    property PullCommand: string read FPullCommand;
    property ExportCommand: string read FExportCommand;

    procedure Clone(const  aCloneFolder, aURL: string);
    procedure Pull(const aFolder: string);
    procedure GetFile(const aFileName, aDestFolder, aURL, aServer: string);
    function GetProduct(const aDestFolderRoot, aDestFolder, aURL, aServer, aProductId: string): boolean;
  end;

implementation
uses UWindowsPath, Deget.CommandLine, UMultiLogger, UTmsBuildSystemUtils, IOUtils;

{ TSvnEngine }

constructor TSvnEngine.Create(const aSvnCommandLine, aCloneCommand, aPullCommand, aExportCommand: string);
begin
  if aSvnCommandLine.Trim = '' then FSvnCommandLine := GetEnvCommandLine
  else FSvnCommandLine := aSvnCommandLine;

  if FSvnCommandLine.Trim = '' then raise Exception.Create('Can''t find a valid svn.exe specified in tms.config.yaml or the Windows PATH. Make sure you have svn installed and configured.');

  if aCloneCommand.Trim = '' then FCloneCommand := 'checkout' else FCloneCommand := aCloneCommand;
  if aPullCommand.Trim = '' then FPullCommand := 'update' else FPullCommand := aPullCommand;
  if aExportCommand.Trim = '' then FExportCommand := 'export' else FExportCommand := aExportCommand;

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

procedure TSvnEngine.GetFile(const aFileName, aDestFolder, aURL, aServer: string);
begin
  var Output := '';
  var DestFolder := TPath.GetFullPath(aDestFolder);
  var FullCommand := '"' + SvnCommandLine + '" --non-interactive ' + ExportCommand + ' "' + aURL + '/' + aFileName + '" "' + DestFolder + '"';
  if DirectoryExists(DestFolder) then raise Exception.Create('Can''t svn export into an existing folder: "' + DestFolder + '"');
  TDirectory_CreateDirectory(DestFolder);
  if not ExecuteCommand(FullCommand, DestFolder, Output)
    then raise Exception.Create('Error trying to get file "' +  aFileName + '" from "' + aUrl + '". Verify that the file exists.');

end;

function TSvnEngine.GetProduct(const aDestFolderRoot, aDestFolder, aURL, aServer, aProductId: string): boolean;
begin
  Result := false;
end;

procedure TSvnEngine.Clone(const aCloneFolder, aURL: string);
begin
  var Output := '';
  var CloneFolder := TPath.GetFullPath(aCloneFolder);
  var FullCommand := '"' + SvnCommandLine + '" --non-interactive ' + CloneCommand + ' "' + aURL + '" "' + CloneFolder + '"';
  if DirectoryExists(CloneFolder) then raise Exception.Create('Can''t svn checkout into an existing folder: "' + CloneFolder + '"');
  TDirectory_CreateDirectory(CloneFolder);
  if not ExecuteCommand(FullCommand, CloneFolder, Output)
    then raise Exception.Create('Error doing svn checkout from "' + aUrl + '" into ' + aCloneFolder);
end;

procedure TSvnEngine.Pull(const aFolder: string);
begin
  var Output := '';
  var Folder := TPath.GetFullPath(aFolder);
  var FullCommand := '"' + SvnCommandLine + '" --non-interactive ' + PullCommand;
  if not ExecuteCommand(FullCommand, Folder, Output)
    then raise Exception.Create('Error doing svn update in "' + aFolder + '"');
end;

end.

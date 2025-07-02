unit UConfigFolders;

interface

uses
  System.SysUtils, System.IOUtils;

type
  IBuildFolders = interface
  ['{99359F83-B90B-4977-A4FF-FDD3CB4C71A5}']
    function RootFolder: string;
    function MetaFolder: string;
    function BplFolder: string;
    function LogFile :string;
    function HashesFolder: string;
    function UninstallFolder: string;
    function SelfUninstallFolder: string;
    function DoctorUndoFolder: string;
    function LockedFilesFolder: string;
    function CredentialsFile(const ServerName: string): string;
    function DownloadsFolder: string;
    function OldDownloadsFolder: string;
    function ProductsFolder: string;
    function ProductsTempFolder: string;
    function ParallelFolder: string;
    function CompileTempFolder: string;
    function VCSMetaFolder: string;
    function VCSTempFolder: string;
    function DcuMegafolder: string;
  end;

  TBuildFolders = class(TInterfacedObject, IBuildFolders)
  private
    FRootFolder: string;
    function LogFolder: string;
    function MetaBuildFolder: string;
    function TempBuildFolder: string;
    function MetaFetchFolder: string;
    function TempFetchFolder: string;
    function MetaDoctorFolder: string;
  public
    constructor Create(const ARootFolder: string);
    function MetaFolder: string;
    function TempFolder: string;
    function RootFolder: string;
    function BplFolder: string;
    function LogFile :string;
    function HashesFolder: string;
    function UninstallFolder: string;
    function SelfUninstallFolder: string;
    function DoctorUndoFolder: string;
    function LockedFilesFolder: string;
    function CredentialsFile(const ServerName: string): string;
    function DownloadsFolder: string;
    function OldDownloadsFolder: string;
    function ProductsFolder: string;
    function ProductsTempFolder: string;
    function ParallelFolder: string;
    function CompileTempFolder: string;
    function VCSMetaFolder: string;
    function VCSTempFolder: string;
    function DcuMegafolder: string;
  end;

implementation

{ TBuildFolders }

constructor TBuildFolders.Create(const ARootFolder: string);
begin
  FRootFolder := ARootFolder;
end;

function TBuildFolders.MetaFolder: string;
begin
  Result := TPath.Combine(RootFolder, '.tmssetup');
end;

function TBuildFolders.TempFolder: string;
begin
  //we'll put dots in the subbfolders, except build.
  Result := TPath.Combine(RootFolder, 'Temp'); //do not put a leading dot in this name. See https://github.com/tmssoftware/tms-smartsetup/issues/97
end;

function TBuildFolders.MetaBuildFolder: string;
begin
  Result := TPath.Combine(MetaFolder, 'build');
end;

function TBuildFolders.TempBuildFolder: string;
begin
  Result := TPath.Combine(TempFolder, 'build');
end;

function TBuildFolders.MetaFetchFolder: string;
begin
  Result := TPath.Combine(MetaFolder, 'fetch');
end;

function TBuildFolders.TempFetchFolder: string;
begin
  Result := TPath.Combine(TempFolder, '.fetch');
end;

function TBuildFolders.MetaDoctorFolder: string;
begin
  Result := TPath.Combine(MetaFolder, 'doctor');
end;


function TBuildFolders.CredentialsFile(const ServerName: string): string;
begin
  var Prefix := '';
  if ServerName <> 'tms' then Prefix := ServerName + '.'; //For v1.0 compat, if the source is tms, keep the credentials file named "credentials", not tms.credentials.

  Result := TPath.Combine(MetaFetchFolder, Prefix + 'credentials');
end;

function TBuildFolders.DownloadsFolder: string;
begin
  Result := TPath.Combine(TPath.Combine(RootFolder, 'Downloads'), 'CurrentVersions');
end;

function TBuildFolders.OldDownloadsFolder: string;
begin
  Result := TPath.Combine(TPath.Combine(RootFolder, 'Downloads'), 'OldVersions');
end;

function TBuildFolders.HashesFolder: string;
begin
  Result := TPath.Combine(MetaBuildFolder, 'hashes');
end;

function TBuildFolders.LogFile: string;
begin
  Result := TPath.Combine(LogFolder, TPath.ChangeExtension(TPath.GetFileName(ParamStr(0)), '.log'));
end;

function TBuildFolders.LogFolder: string;
begin
  Result := TPath.Combine(RootFolder, 'Logs');
end;

function TBuildFolders.ParallelFolder: string;
begin
  Result := TPath.Combine(TempFolder, 'p');
end;

function TBuildFolders.ProductsFolder: string;
begin
  Result := TPath.Combine(RootFolder, 'Products');
end;

function TBuildFolders.ProductsTempFolder: string;
begin
  Result := TPath.Combine(TempFetchFolder, 'Products');
 end;

function TBuildFolders.LockedFilesFolder: string;
begin
  Result := TPath.Combine(TempFolder, '.locked');
end;

function TBuildFolders.BplFolder: string;
begin
  Result := TPath.Combine(MetaBuildFolder, 'bpl');
end;

function TBuildFolders.RootFolder: string;
begin
  Result := TPath.GetFullPath(FRootFolder);
end;

function TBuildFolders.UninstallFolder: string;
begin
  Result := TPath.Combine(MetaBuildFolder, 'uninstall');
end;

function TBuildFolders.SelfUninstallFolder: string;
begin
  Result := TPath.Combine(MetaBuildFolder, 'self-uninstall');
end;

function TBuildFolders.VCSMetaFolder: string;
begin
  Result := TPath.Combine(MetaFolder, 'repositories')
end;

function TBuildFolders.VCSTempFolder: string;
begin
  Result := TPath.Combine(TempFolder, '.repositories')
end;

function TBuildFolders.DcuMegafolder: string;
begin
  Result := TPath.Combine(MetaFolder, 'lib')
end;

function TBuildFolders.DoctorUndoFolder: string;
begin
  Result := TPath.Combine(MetaDoctorFolder, 'undo');
end;

function TBuildFolders.CompileTempFolder: string;
begin
  Result := TPath.Combine(TempBuildFolder, 'compile');
end;

end.

unit Deget.FileUtils;

interface

function GetTempFileNameExt(Extension: string; const TempFolder: string = ''): string;
procedure CreateSymLink(const LinkFileName, TargetFileName, LockedFilesFolder: string);
procedure RemoveSymLink(const LinkFileName, LockedFilesFolder: string);
procedure CopyFilesToDirectory(const SourceFileMask, DestDir: string); overload;
procedure CopyFilesToDirectory(const SourceDir, FileMask, DestDir: string;
  Recursive: Boolean = False); overload;
procedure DeleteFilesFromDirectory(const Dir, FileMask, LockedFilesFolder: string);

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ELSE}
  Posix.Unistd,
{$ENDIF}
  System.Types, System.SysUtils, System.IOUtils, System.Classes,
  UMultiLogger, UTmsBuildSystemUtils;

procedure CopyFilesToDirectory(const SourceDir, FileMask, DestDir: string;
  Recursive: Boolean = False); overload;
begin
  Logger.Trace(Format('Copying files "%s\%s" to "%s"', [SourceDir, FileMask, DestDir]));
  TDirectory_CreateDirectory(DestDir);
  if TDirectory.Exists(SourceDir) then
  begin
    TDirectory.GetFiles(SourceDir, FileMask,
      function(const Path: string; const SearchRec: TSearchRec): Boolean
      begin
        Result := true;
        TFile.Copy(TPath.Combine(Path, SearchRec.Name), TPath.Combine(DestDir, SearchRec.Name), true);
//        Logger.Note(Format('"%s" copied', [SearchRec.Name]));
      end
    );

    if Recursive then
      TDirectory.GetDirectories(SourceDir, TSearchOption.soTopDirectoryOnly,
        function(const Path: string; const SearchRec: TSearchRec): Boolean
        begin
          Result := True;
          if (SearchRec.Name = '.') or (SearchRec.Name = '..') then Exit;

          CopyFilesToDirectory(
            TPath.Combine(Path, SearchRec.Name),
            '*.*',
            TPath.Combine(DestDir, SearchRec.Name),
            True);
        end
      );
  end;
end;

procedure CopyFilesToDirectory(const SourceFileMask, DestDir: string);
begin
  CopyFilesToDirectory(TPath.GetDirectoryName(SourceFileMask), TPath.GetFileName(SourceFileMask), DestDir);
end;


procedure DeleteFilesFromDirectory(const Dir, FileMask, LockedFilesFolder: string);
begin
  TDirectory.GetFiles(Dir, FileMask,
    function(const Path: string; const SearchRec: TSearchRec): Boolean
    begin
      Result := true;
      DeleteFileOrMoveToLocked(LockedFilesFolder, TPath.Combine(Path, SearchRec.Name));
//      Logger.Note(Format('"%s" deleted', [SearchRec.Name]));
    end
  );
end;

function GetTempFileNameExt(Extension: string; const TempFolder: string = ''): string;
begin
  var TempPath := TempFolder;
  if TempPath = '' then
    TempPath := TPath.GetTempPath;

  Result := TPath.Combine(TempPath, TPath.ChangeExtension(TPath.GetGUIDFileName, Extension));
end;

procedure CreateSymLink(const LinkFileName, TargetFileName, LockedFilesFolder: string);
begin
  DeleteFileOrMoveToLocked(LockedFilesFolder, LinkFileName);
  TDirectory_CreateDirectory(TPath.GetDirectoryName(LinkFileName));
  if not FileCreateSymLink(LinkFileName, TargetFileName) then
    RaiseLastOSError(GetLastError, sLineBreak + 'Can''t create symlink: ' + TargetFileName + ' => ' + LinkFileName);
end;

procedure RemoveSymLink(const LinkFileName, LockedFilesFolder: string);
begin
  DeleteFileOrMoveToLocked(LockedFilesFolder, LinkFileName);
end;

end.

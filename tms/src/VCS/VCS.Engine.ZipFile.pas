unit VCS.Engine.ZipFile;
{$I ../../tmssetup.inc}

interface
uses SysUtils, Classes, VCS.Engine.Virtual, ULogger;

type
  TZipFileEngine = class(TInterfacedObject, IVCSEngine)
  private
    procedure RemoveTopEmptyFolder(const DestFolder: string);
  public
    procedure Clone(const aCloneFolder, aURL, aVersion: string);
    procedure AfterClone(const aRootFolder, aCloneFolder: string);

    procedure Pull(const aRootFolder, aGitFolder, aVersion: string);
    procedure GetFile(const aFileName, aDestFolder, aURL, aServer: string);
    function GetVersionNames(const aExistingRepoFolder, aTempFolder, aLockedFolder: string; const aURL: string): TArray<TVersionAndDate>;
    function GetProduct(const aDestFolderRoot, aDestFolder, aURL, aServer, aProductId, aVersion: string): boolean;
    function FileIsVersioned(const aFileName, aWorkingFolder: string): boolean;

    function GetCommitId(const aWorkingFolder: string): string;
    function IsRootVCSFolder(const Folder: string): boolean;
  end;


implementation
uses
   ZipFile.Download, UMultiLogger, IOUtils,
   UGenericDecompressor, UTmsBuildSystemUtils, Commands.GlobalConfig;

{ TZipFileEngine }

procedure TZipFileEngine.AfterClone(const aRootFolder, aCloneFolder: string);
begin

end;

procedure TZipFileEngine.Clone(const aCloneFolder, aURL, aVersion: string);
begin
  raise Exception.Create('Clone not supported in ZIPFILE protocol.');
end;

function TZipFileEngine.FileIsVersioned(const aFileName,
  aWorkingFolder: string): boolean;
begin
  Result := TFile.Exists(aFileName);
end;

//This method is to handle a case where the product is all put inside a folder.
//So the actual tmsbuild.yaml is at root\Product\tmsbuild.yaml instead of root\tmsbuild.yaml.
//We accept multiple empty folders, root\Product\other\tmsbuild.yaml is ok *if* Product and other are empty.
//(Product is not completely empty because it has 1 folder, other, but that is the only thing we allow).
function FileIsAtRoot(const FileName, aDestFolder: string): boolean;
begin
  var Folder := TPath.GetFullPath(TPath.GetDirectoryName(FileName));
  while true do
  begin
    if Folder = '' then exit(false);
    if SameText(Folder, TPath.GetFullPath(aDestFolder)) then exit(true);
    Folder := TPath.GetDirectoryName(Folder);
    if TDirectory.GetFiles(Folder, '*', TSearchOption.soTopDirectoryOnly) <> nil then exit(false);
    if Length(TDirectory.GetDirectories(Folder, '*', TSearchOption.soTopDirectoryOnly)) > 1 then exit(false);
  end;

end;

function TZipFileEngine.GetCommitId(const aWorkingFolder: string): string;
begin
  Result := '';
end;

procedure TZipFileEngine.GetFile(const aFileName, aDestFolder, aURL, aServer: string);
begin
  //Sometimes the files might be inside an empty root folder, so we need to skip it.
  var ZipFileName := TPath.Combine(aDestFolder, aFileName + '.download');
  try
    ZipDownloader.GetRepo(aURL, ZipFileName, aServer, Logger.Write, true);
    TBundleDecompressor.ExtractCompressedFile(ZipFileName, aDestFolder, TDownloadFormat.Unknown,
      function(FileName: string): boolean
      begin
        Result := not SameText(TPath.GetFileName(FileName), aFileName);
      end);

  finally
    DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, ZipFileName);
  end;

  var Files := TDirectory.GetFiles(aDestFolder, '*', TSearchOption.soAllDirectories);
  for var FileName in Files do
  begin
    if FileIsAtRoot(FileName, aDestFolder) then
    begin
      RenameAndCheck(FileName, TPath.Combine(aDestFolder, TPath.GetFileName(FileName)));
      exit;
    end;

  end;

end;

procedure TZipFileEngine.RemoveTopEmptyFolder(const DestFolder: string);
begin
  var TopFolders := TDirectory.GetDirectories(DestFolder, '*', TSearchOption.soTopDirectoryOnly);
  if Length(TopFolders) <> 1 then exit;
  var TopFiles := TDirectory.GetFiles(DestFolder, '*', TSearchOption.soTopDirectoryOnly);
  if Length(TopFiles) <> 0 then exit;

  var TopFolder := TPath.Combine(DestFolder, TopFolders[0]);
  var ChildFolders := TDirectory.GetDirectories(TopFolder, '*', TSearchOption.soTopDirectoryOnly);
  var ChildFiles := TDirectory.GetFiles(TopFolder, '*', TSearchOption.soTopDirectoryOnly);

  //If some folder has the same name as root, this will fail. As removing the top empty folder is cosmetic, if there is a possible problem, we just don't remove it.
  for var ChildFolder in ChildFolders do
    if SameText(TPath.GetFileName(ChildFolder), TPath.GetFileName(TopFolder)) then exit;
  for var ChildFile in ChildFiles do
    if SameText(TPath.GetFileName(ChildFile), TPath.GetFileName(TopFolder)) then exit;

  for var ChildFolder in ChildFolders do RenameAndCheckFolder(ChildFolder, TPath.Combine(DestFolder, TPath.GetFileName(ChildFolder)));
  for var ChildFile in ChildFiles do RenameAndCheck(ChildFile, TPath.Combine(DestFolder, TPath.GetFileName(ChildFile)));
  SysUtils.RemoveDir(TopFolder);

end;

function TZipFileEngine.GetProduct(const aDestFolderRoot, aDestFolder, aURL, aServer, aProductId, aVersion: string): boolean;
begin
  if aVersion <> '' then raise Exception.Create('Versioning not supported in ZIPFILE protocol.');

  Result := true;
  var TempGUIDProductFolder := TPath.Combine(Config.Folders.VCSTempFolder, GuidToStringN(TGUID.NewGuid));
  TDirectory_CreateDirectory(TempGUIDProductFolder);
  var ZipFileName := TPath.Combine(TempGUIDProductFolder, aProductId + '.zip');
  var TmpETagFile := ZipFileName + '.etag';
  var DestETagFile := TPath.Combine(aDestFolderRoot, TPath.GetFileName(TmpETagFile));
  if TFile.Exists(DestETagFile) then TFile.Copy(DestETagFile, TmpETagFile);
  try
    ZipDownloader.GetRepo(aURL, ZipFileName, aServer, Logger.Write);
    if TFile.Exists(ZipFileName) then  //might not exist if it wasn't modified
    begin
      DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, DestETagFile);
      try
        TBundleDecompressor.ExtractCompressedFile(ZipFileName, aDestFolder);
        RemoveTopEmptyFolder(aDestFolder);
      except
        DeleteFolderMovingToLocked(Config.Folders.LockedFilesFolder, aDestFolder, true);
        raise;
      end;
      RenameAndCheck(TmpETagFile, DestETagFile);
    end;
  finally
    DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, ZipFileName);
  end;
end;

function TZipFileEngine.GetVersionNames(const aExistingRepoFolder, aTempFolder, aLockedFolder: string; const aURL: string): TArray<TVersionAndDate>;
begin
  raise Exception.Create('GetVersionNames not supported in ZIPFILE protocol.');
end;

function TZipFileEngine.IsRootVCSFolder(const Folder: string): boolean;
begin
  Result := true;
end;

procedure TZipFileEngine.Pull(const aRootFolder, aGitFolder, aVersion: string);
begin
  raise Exception.Create('Pull not supported in ZIPFILE protocol.');
end;

end.

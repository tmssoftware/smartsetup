unit VCS.Engine.ZipFile;
{$I ../../tmssetup.inc}

interface
uses SysUtils, Classes, VCS.Engine.Virtual, ULogger;

type
  TZipFileEngine = class(TInterfacedObject, IVCSEngine)
  public
    procedure Clone(const  aCloneFolder, aURL: string);
    procedure Pull(const aFolder: string);
    procedure GetFile(const aFileName, aDestFolder, aURL, aServer: string);
    function GetProduct(const aDestFolder, aURL, aServer: string): boolean;
  end;


implementation
uses
   ZipFile.Download, UMultiLogger, IOUtils,
   UGenericDecompressor, UTmsBuildSystemUtils, Commands.GlobalConfig;

{ TZipFileEngine }

procedure TZipFileEngine.Clone(const aCloneFolder, aURL: string);
begin
  raise Exception.Create('Pull not supported in ZIPFILE protocol.');
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

function TZipFileEngine.GetProduct(const aDestFolder, aURL, aServer: string): boolean;
begin
  Result := true;
  var ZipFileName := TPath.Combine(aDestFolder, TPath.GetFileName(aDestFolder) + '.download');
  try
    ZipDownloader.GetRepo(aURL, ZipFileName, aServer, Logger.Write);
    if TFile.Exists(ZipFileName) then  //might not exist if it wasn't modified
    begin
      TBundleDecompressor.ExtractCompressedFile(ZipFileName, aDestFolder);
    end;
  finally
    DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, ZipFileName); //This will keep the Etag file, so it won't be fetched again.
  end;
end;

procedure TZipFileEngine.Pull(const aFolder: string);
begin
  raise Exception.Create('Pull not supported in ZIPFILE protocol.');
end;

end.

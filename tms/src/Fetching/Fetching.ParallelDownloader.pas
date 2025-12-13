unit Fetching.ParallelDownloader;

{$I ../../tmssetup.inc}

interface

uses
{$IFDEF MSWINDOWS}
  WinApi.Windows,
{$ENDIF}
  System.SysUtils, System.Threading, System.DateUtils, UAppTerminated, URepositoryManager,
  Fetching.ParallelProjectInfo, Fetching.InfoFile, Fetching.FetchItem, Fetching.InstallInfo,
  UConfigFolders, UGenericDecompressor;

type
  TParallelDownloader = class
  private
    ProcessedSteps: Integer;
    ParallelProjects: TParallelProjectInfoList;
    Items: TFetchItems;
    Repo: TRepositoryManager;
    LastTime: TDateTime;
    Folders: IBuildFolders;
    procedure SetupLogger(TotalSteps: Integer);
    procedure FetchOneProject(const FetchItem: TFetchItem);
    procedure DownloadAndUnzip(const FetchItem: TFetchItem);
    procedure CreateInstallInfoFile(const Folder: string; const FetchItem: TFetchItem);
    function DownloadFile(Info: TDownloadInfo; const FileNameOnDisk: string; out DownloadFormat: TDownloadFormat): string;
    function FindFile(const FileNameWithoutExtension: string; const FetchItemFileHash: string): string;
  public
    constructor Create(AItems: TFetchItems; ARepo: TRepositoryManager; AFolders: IBuildFolders);
    destructor Destroy; override;
    procedure Fetch;
  end;

implementation
uses
  ULogger, UMultiLogger, System.Zip, Fetching.OfflineHTTPClient, System.Generics.Collections, UTmsBuildSystemUtils,
  System.Classes, System.IOUtils, UHasher,
{$IFDEF POSIX}
  Posix.UniStd, Posix.Stdio,
{$ENDIF}
  Commands.GlobalConfig, Decompressor.ZSTD;



{ TParallelDownloader }

constructor TParallelDownloader.Create(AItems: TFetchItems; ARepo: TRepositoryManager; AFolders: IBuildFolders);
begin
  inherited Create;
  Items := AItems;
  Repo := ARepo;
  ParallelProjects := TParallelProjectInfoList.Create;
  Folders := AFolders;
end;

destructor TParallelDownloader.Destroy;
begin
  Logger.ResetPercentAction;
  ParallelProjects.Free;
  inherited;
end;

procedure TParallelDownloader.SetupLogger(TotalSteps: Integer);
begin
  Logger.SetPercentAction(
    function: integer
    begin
      if (TotalSteps = 0) or (ProcessedSteps > TotalSteps) then
        Exit(-1);
      Result := (ProcessedSteps * 100) div TotalSteps;
    end);
end;

procedure TParallelDownloader.Fetch;
begin
  var TotalSteps := 0;
  for var FetchItem in Items do
    if FetchItem.Status = TFetchStatus.Outdated then
    begin
      FetchOneProject(FetchItem);
      Inc(TotalSteps, 100);
    end;

  SetupLogger(TotalSteps);
  try
    LastTime := Now;
    ParallelProjects.Run;
    Logger.Info('Download tasks finished');
  finally
    Logger.ResetPercentAction;
    Logger.Info(Format('Total: %d, Processed: %d', [TotalSteps, ProcessedSteps]));
  end;
end;

procedure TParallelDownloader.FetchOneProject(const FetchItem: TFetchItem);
begin
  ParallelProjects.Add(FetchItem.ProductId, TParallelProjectInfo.Create);
  ParallelProjects.SetRun(FetchItem.ProductId,
    procedure
    begin
      Logger.StartSection(TMessageType.Fetch, FetchItem.UniqueName);
      try
        CheckAppTerminated;
        DownloadAndUnzip(FetchItem);
      except
        on ex: Exception do
        begin
          FetchItem.Status := TFetchStatus.Failed;
          Logger.Error(Format('Error downloading %s: %s', [FetchItem.ProductId, ex.Message]));
        end;
      end;
      Logger.FinishSection(TMessageType.Fetch, FetchItem.Status = TFetchStatus.Failed);
    end);
end;

procedure TParallelDownloader.CreateInstallInfoFile(const Folder: string; const FetchItem: TFetchItem);
begin
  //pinned must be false when downloading new products
  TFetchInfoFile.SaveInFolder(Folder, FetchItem.ProductId, FetchItem.Version, FetchItem.Server, false);
end;

function TParallelDownloader.FindFile(const FileNameWithoutExtension: string; const FetchItemFileHash: string): string;
begin
  Result := '';

  for var DownloadFormat := Low(TDownloadFormat) to High(TDownloadFormat) do
  begin
    var ext := DownloadFormatExtension[DownloadFormat];

    for var Folder in [Folders.DownloadsFolder, Folders.OldDownloadsFolder] do
    begin
      var FileName := CombinePath(Folder, FileNameWithoutExtension + ext);
      if TFile.Exists(FileName) then
      begin
        var ExistingFileHash := HashFile(FileName);
        if (ExistingFileHash = FetchItemFileHash) then exit(FileName);  //If hash doesn't match, corrupt, redownload.
      end;
    end;
  end;
end;

procedure TParallelDownloader.DownloadAndUnzip(const FetchItem: TFetchItem);
begin
  // Check if the file is already locally downloaded, without hitting the server
  TDirectory_CreateDirectory(Folders.DownloadsFolder);
  
  var DownloadInfo: TDownloadInfo;
  var HasDownloadInfo := false;
  if FetchItem.FileHash = '' then
  begin
    DownloadInfo := Repo.GetDownloadInfo(FetchItem.ProductId, FetchItem.Version);
    FetchItem.FileHash := DownloadInfo.FileHash;
    HasDownloadInfo := true;
  end;

  var ExistingFileName := FindFile(FetchItem.UniqueName, FetchItem.FileHash);
  
  var DownloadFormat := TDownloadFormat.Unknown;
  if ExistingFileName = '' then
  begin
    if not HasDownloadInfo then DownloadInfo := Repo.GetDownloadInfo(FetchItem.ProductId, FetchItem.Version);
    ExistingFileName := DownloadFile(DownloadInfo, CombinePath(Folders.DownloadsFolder, FetchItem.UniqueName), DownloadFormat);
  end
  else
  begin
    DownloadFormat := TBundleDecompressor.GetFileFormat(ExistingFileName);
    Logger.Trace(Format('Cached download found for %s, no need to download again', [FetchItem.ProductId]));
    AtomicIncrement(ProcessedSteps, 100);
  end;

  if not FetchItem.SkipExtraction then
  begin
    var ProductsTempFolder := Folders.ProductsTempFolder;
    var ExtractFolder := CombinePath(ProductsTempFolder, FetchItem.UniqueName);
    if TDirectory.Exists(ExtractFolder) then
      TDirectory.Delete(ExtractFolder, True);

    TBundleDecompressor.ExtractCompressedFile(ExistingFileName, ExtractFolder, DownloadFormat);
    CreateInstallInfoFile(ExtractFolder, FetchItem);

    var FinalFolder := CombinePath(Folders.ProductsFolder, FetchItem.ProductId);
    DeleteFolderMovingToLocked(Config.Folders.LockedFilesFolder, FinalFolder, true);
    RenameAndCheckFolder(ExtractFolder, FinalFolder);
    Logger.Info('Extracted ' + TPath.GetFileName(ExistingFileName));
  end;

  FetchItem.Status := TFetchStatus.Downloaded;
end;


function TParallelDownloader.DownloadFile(Info: TDownloadInfo; const FileNameOnDisk: string; out DownloadFormat: TDownloadFormat): string;
begin
  Result := '';
  var Aborted := False;
  var AddedSteps := 0;
  var Client: TOfflineHTTPClient := TOfflineHTTPClient.Create;
  try
    // set receive data callback
    begin
      var LastStep := 0;
      Client.ReceiveDataCallback :=
        procedure(const Receiveer: TObject; AContentLength: Int64; AWriteCount: Int64; var AAbort: Boolean)
        begin
          if AppIsTerminated then
          begin
            AAbort := true;
            Aborted := true;
          end;

          var Step := Trunc((AWriteCount / AContentLength) * 100);
          if Step > LastStep then
          begin
            AtomicIncrement(ProcessedSteps, Step - LastStep);
            AddedSteps := AddedSteps + Step - LastStep;
            LastStep := Step;
          end;

          TMonitor.Enter(Self);
          try
            if SecondsBetween(Now, LastTime) > 1 then
            begin
              LastTime := Now;
              Logger.Info(Format('Downloading %s (%d%%)', [TPath.GetFileName(FileNameOnDisk), Step]));
            end;
          finally
            TMonitor.Exit(Self);
          end;
        end
    end;

    // try download file
    TDirectory_CreateDirectory(TPath.GetDirectoryName(FileNameOnDisk));
    var AResponse: IHTTPResponse;
    var TempFileName := FileNameOnDisk + '.download';
    var fs := TFileStream.Create(TempFileName, fmCreate);
    try
      var Request := Client.GetRequest('GET', Info.DownloadUrl);
      if Info.RequiresToken and (Repo.AccessToken <> '') then
        Request.AddHeader('Authorization', 'Bearer ' + Repo.AccessToken);
      AResponse := Client.Execute(Request, fs);
      AtomicIncrement(ProcessedSteps, 100 - AddedSteps);
    finally
      fs.Free;
    end;

    // Check if file was downloaded ok, if not, delete it (we must do this after fe.Free)
    if AResponse.StatusCode <> 200 then
    begin
      var ErrorMessage: string := '';
      if AResponse.HeaderValue['Content-Type'].StartsWith('text/', True) and TFile.Exists(FileNameOnDisk) then
        ErrorMessage := ': ' + TFile.ReadAllText(FileNameOnDisk);
      DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, TempFileName);

      raise Exception.Create(Format('Download for %s failed with status %d%s',
        [TPath.GetFileName(FileNameOnDisk), AResponse.StatusCode, ErrorMessage]));
    end;

    DownloadFormat := TBundleDecompressor.GetFileFormat(TempFileName);
    var FinalFileNameOnDisk := FileNameOnDisk + DownloadFormatExtension[DownloadFormat];
    Result := FinalFileNameOnDisk; 
    DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, FinalFileNameOnDisk);
    RenameFile(TempFileName, FinalFileNameOnDisk);

    if Aborted then
      raise Exception.Create(TPath.GetFileName(FileNameOnDisk) + ' download aborted')
    else
      Logger.Info('Downloaded ' + TPath.GetFileName(FinalFileNameOnDisk));
  finally
    Client.Free;
  end;
end;

end.

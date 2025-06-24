unit Git.HTTPSDownload;

interface
uses Windows;
type
  GitDownloader = class
  private
    class procedure GetFile(const DownloadUrl, FileNameOnDisk: string); static;

  public
    //Downloads a full githubrepo to a file.
    class procedure GetRepo(const DownloadUrl, FileNameOnDisk: string); static;

  end;

implementation
uses Classes, SysUtils, System.Net.HttpClient, UTmsBuildSystemUtils,
     IOUtils, Commands.GlobalConfig, UAppTerminated, UMultiLogger;

{ GitDownloader }

//Code is from TParallelDownloader.DownloadFile. We could unify it, but for now I prefer to evolve it separately.
class procedure GitDownloader.GetFile(const DownloadUrl, FileNameOnDisk: string);
begin
  var Client: THTTPClient := THTTPClient.Create;
  try
    var Aborted := false;
    TDirectory_CreateDirectory(TPath.GetDirectoryName(FileNameOnDisk));
    var AResponse: IHTTPResponse;
    var TempFileName := FileNameOnDisk + '.download';
    var fs := TFileStream.Create(TempFileName, fmCreate);
    try
      Client.ReceiveDataCallback :=
        procedure(const Receiveer: TObject; AContentLength: Int64; AWriteCount: Int64; var AAbort: Boolean)
        begin
          if AppIsTerminated then
          begin
            AAbort := true;
            Aborted := true;
          end;
        end;

      var Request := Client.GetRequest('GET', DownloadUrl);
      AResponse := Client.Execute(Request, fs);
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

    if Aborted then
      raise Exception.Create(TPath.GetFileName(FileNameOnDisk) + ' download aborted')
    else
      Logger.Trace('Downloaded ' + TPath.GetFileName(FileNameOnDisk));

    DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, FileNameOnDisk);
    RenameFile(TempFileName, FileNameOnDisk);

  finally
    Client.Free;
  end;

end;

class procedure GitDownloader.GetRepo(const DownloadUrl,
  FileNameOnDisk: string);
begin
  GetFile(DownloadUrl, FileNameOnDisk);
end;

end.

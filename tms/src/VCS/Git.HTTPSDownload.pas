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
    var ETagFileName := FileNameOnDisk + '.etag';
    var ETag := 'invalid';
    try
      if TFile.Exists(ETagFileName) then ETag := TFile.ReadAllText(ETagFileName, TEncoding.UTF8);
    except on ex: Exception do //invalid etag file?
      begin
        Logger.Info('Invalid ETag Cache: ' + ex.Message);
        ETag := 'invalid';
      end;
    end;

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

      Client.CustomHeaders['If-None-Match'] := ETag;

      var Request := Client.GetRequest('GET', DownloadUrl);
      AResponse := Client.Execute(Request, fs);
    finally
      fs.Free;
    end;

    case AResponse.StatusCode of
      304:
        begin //File didn't change in server.
          Logger.Trace('Skipped downloading ' + TPath.GetFileName(FileNameOnDisk) + ' because it was up to date.');
          DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, TempFileName);
        end;

      200:
        begin
          if Aborted then
            raise Exception.Create(TPath.GetFileName(FileNameOnDisk) + ' download aborted')
          else
          begin
            Logger.Trace('Downloaded ' + TPath.GetFileName(FileNameOnDisk));
          end;

          DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, FileNameOnDisk);
          RenameFile(TempFileName, FileNameOnDisk);

          var NewETag := AResponse.HeaderValue['ETag'];
          if NewETag = '' then Logger.Error('Server at url "' + DownloadUrl + '" doesn''t support ETags. USE ONLY SERVERS WITH ETAGs to avoid continually downloading the same file.' );
          TFile.WriteAllText(ETagFileName, NewETag, TEncoding.UTF8);
        end


      else
        begin
          // if file was not downloaded ok, delete it (we must do this after fs.Free)
          var ErrorMessage: string := '';
          if AResponse.HeaderValue['Content-Type'].StartsWith('text/', True) and TFile.Exists(FileNameOnDisk) then
            ErrorMessage := ': ' + TFile.ReadAllText(FileNameOnDisk);
          DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, TempFileName);

          raise Exception.Create(Format('Download for %s failed with status %d%s',
            [TPath.GetFileName(FileNameOnDisk), AResponse.StatusCode, ErrorMessage]));

        end;
    end;

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

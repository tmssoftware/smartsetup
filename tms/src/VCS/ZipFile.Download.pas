unit ZipFile.Download;

interface
uses ULogger,
  {$IFDEF MSWINDOWS}
    Windows,
  {$ELSE}
    Posix.Stdio,
  {$ENDIF}
  System.Net.HttpClient;
type
  TDownloadLogger = reference to procedure(const Verbosity: TVerbosity; const msg: string);

  ZipDownloader = class
  private
    class procedure GetFile(const DownloadUrl, FileNameOnDisk, Server: string; DownloadLogger: TDownloadLogger; const ForceDownload: boolean); static;
    class procedure GetHttpsFile(const DownloadUrl, FileNameOnDisk, Server: string; DownloadLogger: TDownloadLogger; const ForceDownload: boolean); static;
    class procedure GetLocalFile(const DownloadUrl, FileNameOnDisk: string; DownloadLogger: TDownloadLogger; const ForceDownload: boolean); static;
    class function ReadETag(const ETagFileName: string;
      const ForceDownload: boolean; const DownloadLogger: TDownloadLogger): string; static;
    class procedure WriteETag(const ETagFileName: string;
      const ForceDownload: boolean; const DownloadLogger: TDownloadLogger;
      const NewETag, DownloadUrl: string); static;
    class procedure AddCustomHeaders(const Request: IHTTPRequest; const Server: string); static;
    class function RemoveFilePrefix(const s: string): string; static;

  public
    //Downloads a full zipped repo to a file.
    class procedure GetRepo(const DownloadUrl, FileNameOnDisk, Server: string; DownloadLogger: TDownloadLogger; const ForceDownload: boolean = false); static;

  end;

implementation
uses Classes, SysUtils, Character, UTmsBuildSystemUtils,
     IOUtils, Commands.GlobalConfig, UAppTerminated, UMultiLogger, System.Hash;

const
  ETagInvalid =  'invalid';
{ ZipDownloader }

class function ZipDownloader.ReadETag(const ETagFileName: string; const ForceDownload: boolean; const DownloadLogger: TDownloadLogger): string;
begin
  Result := ETagInvalid;
  try
    if TFile.Exists(ETagFileName) and not ForceDownload then Result := TFile.ReadAllText(ETagFileName, TEncoding.UTF8);
  except on ex: Exception do //invalid etag file?
    begin
      DownloadLogger(TVerbosity.info, 'Invalid ETag Cache: ' + ex.Message);
      Result := ETagInvalid;
    end;
  end;

end;

class procedure ZipDownloader.WriteETag(const ETagFileName: string; const ForceDownload: boolean; const DownloadLogger: TDownloadLogger; const NewETag, DownloadUrl: string);
begin
  if not ForceDownload then
  begin
    if NewETag = '' then DownloadLogger(TVerbosity.Error, 'Server at url "' + DownloadUrl + '" doesn''t support ETags. USE ONLY SERVERS WITH ETAGs to avoid continually downloading the same file.');
    TFile.WriteAllText(ETagFileName, NewETag, TEncoding.UTF8);
  end;
end;

class procedure ZipDownloader.AddCustomHeaders(const Request: IHTTPRequest; const Server: string);
begin
  var HeadersString := GetEnvironmentVariable('SMARTSETUP_SERVER_' + Server.ToUpper);
  var Headers := HeadersString.Split([';'], TStringSplitOptions.ExcludeEmpty);
  for var Header in Headers do
  begin
    var Idx := Header.IndexOf(':');
    if Idx < 1 then
    begin
      Logger.Info('Invalid Header: ' + Header + '. Missing ":"');
      continue;
    end;

    Request.AddHeader(Header.Substring(0, Idx), Header.Substring(Idx + 1));
  end;

end;

class function ZipDownloader.RemoveFilePrefix(const s: string): string;
const
  FileProtocol = 'file://';
begin
  //Technically, the protocol would be file://c:/temp
  //But, it is standard in many places to write it as file:///c:/temp
  //(with an extra slash to signify an absolute path, which it is anyway because after that we have c:/
  //We need to support both ways.

  if not s.StartsWith(FileProtocol, true) then raise Exception.Create('Url "' + s + '" should start with file://');

  Result := s.Substring(FileProtocol.Length);
  if (Result.Length > 3)
    and (Result[1] = '/') and Character.IsLetter(Result[2]) and (Result[3] = ':') and (Result[4] = '/')
    then Result := Result.Substring(1);

end;

class procedure ZipDownloader.GetFile(const DownloadUrl, FileNameOnDisk, Server: string;
  DownloadLogger: TDownloadLogger; const ForceDownload: boolean);
const
  FileProtocol = 'file://';
begin
  if DownloadUrl.StartsWith(FileProtocol, true) then GetLocalFile(RemoveFilePrefix(DownloadUrl), FileNameOnDisk, DownloadLogger, ForceDownload)
  else GetHttpsFile(DownloadUrl, FileNameOnDisk, Server, DownloadLogger, ForceDownload);
end;

//Code is from TParallelDownloader.DownloadFile. We could unify it, but for now I prefer to evolve it separately.
class procedure ZipDownloader.GetHttpsFile(const DownloadUrl, FileNameOnDisk, Server: string; DownloadLogger: TDownloadLogger; const ForceDownload: boolean);
begin
  var Client: THTTPClient := THTTPClient.Create;
  try
    var Aborted := false;
    TDirectory_CreateDirectory(TPath.GetDirectoryName(FileNameOnDisk));
    var AResponse: IHTTPResponse;
    var TempFileName := TPath.Combine(Config.Folders.ZipFileTempFolder, TPath.GetFileName(FileNameOnDisk) + '.download');
    var ETagFileName := FileNameOnDisk + '.etag';
    var ETag := ReadETag(ETagFileName, ForceDownload, DownloadLogger);

    TDirectory_CreateDirectory(TPath.GetDirectoryName(TempFileName));
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
      AddCustomHeaders(Request, Server);

      AResponse := Client.Execute(Request, fs);
    finally
      fs.Free;
    end;

    case AResponse.StatusCode of
      304:
        begin //File didn't change in server.
          DownloadLogger(TVerbosity.trace, TPath.GetFileName(FileNameOnDisk) + ' is up to date.');
          DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, TempFileName);
        end;

      200:
        begin
          if Aborted then
            raise Exception.Create(TPath.GetFileName(FileNameOnDisk) + ' download aborted')
          else
          begin
            DownloadLogger(TVerbosity.trace, TPath.GetFileName(FileNameOnDisk) + ' downloaded.');
          end;

          DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, FileNameOnDisk);
          RenameFile(TempFileName, FileNameOnDisk);

          WriteETag(ETagFileName, ForceDownload, DownloadLogger, AResponse.HeaderValue['ETag'], DownloadUrl);
        end


      else
        begin
          var ErrorMessage := '';
          try try
            // if file was not downloaded ok, delete it (we must do this after fs.Free)
            var ContextType := AResponse.HeaderValue['Content-Type'];
            if (ContextType.StartsWith('text/', True) or ContextType.StartsWith('application/json', True))
              and TFile.Exists(TempFileName)
              then ErrorMessage := ': ' + TFile.ReadAllText(TempFileName);
          finally
            DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, TempFileName);
          end;
          finally
            DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, ETagFileName);
          end;

          raise Exception.Create(Format('Download for %s from %s failed with status %d%s',
            [TPath.GetFileName(FileNameOnDisk), DownloadUrl, AResponse.StatusCode, ErrorMessage]));

        end;
    end;

  finally
    Client.Free;
  end;

end;

class procedure ZipDownloader.GetLocalFile(const DownloadUrl,
  FileNameOnDisk: string; DownloadLogger: TDownloadLogger;
  const ForceDownload: boolean);
begin
  TDirectory_CreateDirectory(TPath.GetDirectoryName(FileNameOnDisk));
  var ETagFileName := FileNameOnDisk + '.etag';
  var ETag := ReadETag(ETagFileName, ForceDownload, DownloadLogger);
  var FileHash := THashSHA2.GetHashStringFromFile(DownloadUrl);

  if FileHash = ETag then
  begin
    DownloadLogger(TVerbosity.trace, TPath.GetFileName(FileNameOnDisk) + ' is up to date.');
    exit;
  end;

  DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, FileNameOnDisk);
  TFile.Copy(DownloadUrl, FileNameOnDisk);
  DownloadLogger(TVerbosity.trace, TPath.GetFileName(FileNameOnDisk) + ' copied.');
  WriteETag(ETagFileName, ForceDownload, DownloadLogger, FileHash, DownloadUrl);


end;

class procedure ZipDownloader.GetRepo(const DownloadUrl,
  FileNameOnDisk, Server: string; DownloadLogger: TDownloadLogger; const ForceDownload: boolean = false);
begin
  GetFile(DownloadUrl, FileNameOnDisk, Server, DownloadLogger, ForceDownload);
end;

end.

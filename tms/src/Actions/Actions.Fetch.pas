unit Actions.Fetch;

{$I ../../tmssetup.inc}

interface

type
  TFetchMode = (OnlyInstalled, DownloadNew);

procedure ExecuteFetchAction(AProductIds: TArray<string>; FetchMode: TFetchMode);

implementation

uses
  System.SysUtils,
  System.StrUtils,
  ULogger, UMultiLogger,
  Commands.CommonOptions,
  Commands.GlobalConfig,
  URepositoryManager,
  Fetching.Manager,
  Fetching.Summary,
  Commands.Logging,
  UConfigDefinition,
  VCS.Manager;

function FindApiServer: TServerConfig;
begin
  var ServerIndex := -1;
  for var i := 0 to Config.ServerConfig.ServerCount - 1 do
  begin
    var Server := Config.ServerConfig.GetServer(i);
    if not Server.Enabled then continue;
    if Server.Protocol <> TServerProtocol.Api then continue;
    if ServerIndex <> -1 then raise Exception.Create('In this version of Smart Setup, only one API Server is allowed.');
    ServerIndex := i;
  end;

  if ServerIndex = -1 then exit(TServerConfig.Create('', TServerProtocol.Api, '', false)); //not found
  Result := Config.ServerConfig.GetServer(ServerIndex);
end;

procedure ExecuteFetchAction(AProductIds: TArray<string>; FetchMode: TFetchMode);
begin
  var ApiServer := FindApiServer;
  var Repo := CreateRepositoryManager(Config.Folders.CredentialsFile(ApiServer.Name), FetchOptions, ApiServer.Url, false);
  try
    var VCSProducts := TVCSManager.Fetch(AProductIds, FetchMode = TFetchMode.OnlyInstalled);
    try
      if Repo <> nil then
      begin
        var Manager := TFetchManager.Create(Config.Folders, Repo, VCSProducts);
        try
          try
            case FetchMode of
              TFetchMode.OnlyInstalled:
              begin
                Logger.StartSection(TMessageType.Update, 'Updating installed products');
                try
                  Manager.UpdateInstalled(AProductIds);
                finally
                  Logger.FinishSection(TMessageType.Update, false);
                end;
              end
            else
              begin
                { TFetchMode.DownloadNew: }
                Logger.StartSection(TMessageType.Update, 'Updating selected products');
                try
                  Manager.UpdateSelected(AProductIds);
                finally
                  Logger.FinishSection(TMessageType.Update, false);
                end;
              end;
            end;

          finally
            LogFetchSummary(Manager.FetchItems);
          end;
        finally
          Manager.Free;
        end;
      end;
    finally
      VCSProducts.Free;
    end;
  finally
    Repo.Free;
  end;
end;

end.

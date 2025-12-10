unit Actions.Fetch;

{$I ../../tmssetup.inc}

interface

type
  TFetchMode = (OnlyInstalled, DownloadNew);

procedure ExecuteFetchAction(AProductIdsAndVersions: TArray<string>; FetchMode: TFetchMode);

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
  Fetching.ProductVersion,
  VCS.Manager, Downloads.VersionManager;

function FindApiServer: TServerConfig;
begin
  var ServerIndex := -1;
  for var i := 0 to Config.ServerConfig.ServerCount - 1 do
  begin
    var Server := Config.ServerConfig.GetServer(i);
    if not Server.Enabled then continue;
    if Server.ServerType <> TServerType.Api then continue;
    if ServerIndex <> -1 then raise Exception.Create('In this version of Smart Setup, only one API Server is allowed.');
    ServerIndex := i;
  end;

  if ServerIndex = -1 then exit(TServerConfig.Create('', TServerType.Api, '', false)); //not found
  Result := Config.ServerConfig.GetServer(ServerIndex);
end;

procedure ExecuteFetchAction(AProductIdsAndVersions: TArray<string>; FetchMode: TFetchMode);
begin
  var ApiServer := FindApiServer;
  var ProductVersions := ParseVersions(AProductIdsAndVersions);
  var Repo := CreateRepositoryManager(Config.Folders.CredentialsFile(ApiServer.Name), FetchOptions, ApiServer.Url, ApiServer.Name, false);
  try
    var VCSProducts := TVCSManager.Fetch(ProductVersions, FetchMode = TFetchMode.OnlyInstalled);
    try
      var Manager := TFetchManager.Create(Config.Folders, Repo, VCSProducts);
      try
        try
          case FetchMode of
            TFetchMode.OnlyInstalled:
            begin
              Logger.StartSection(TMessageType.Update, 'Updating installed products');
              try
                Manager.UpdateInstalled(ProductVersions);
              finally
                Logger.FinishSection(TMessageType.Update, false);
              end;
            end
          else
            begin
              { TFetchMode.DownloadNew: }
              Logger.StartSection(TMessageType.Update, 'Updating selected products');
              try
                  Manager.UpdateSelected(ProductVersions);
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
    finally
      VCSProducts.Free;
    end;
  finally
    Repo.Free;
  end;

  RotateDownloads(Config.MaxVersionsPerProduct);

end;

end.

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
  VCS.Manager;

procedure ExecuteFetchAction(AProductIds: TArray<string>; FetchMode: TFetchMode);
begin
  var Repo := CreateRepositoryManager(Config.Folders.CredentialsFile, FetchOptions, false);
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

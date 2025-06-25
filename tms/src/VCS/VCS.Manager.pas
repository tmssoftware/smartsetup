unit VCS.Manager;
{$I ../../tmssetup.inc}

interface
uses Classes, SysUtils, Commands.GlobalConfig, VCS.Registry,
{$IFDEF MSWINDOWS}
     Windows,
{$ENDIF}
     Generics.Collections, Threading, VCS.Summary, VCS.CoreTypes, UProjectDefinition;

type

  TVCSManager = class
  private
    class procedure DoFetch(const Products: TList<TRegisteredProduct>); static;
    class function CaptureFetch(TK: integer; Proc: TProc<integer>): TProc; static;
    class function FetchProduct(const Product: TRegisteredProduct): TVCSFetchStatus; static;
    class procedure DoFetchProduct(const Product: TRegisteredProduct); static;
    class function DoGetProduct(const Protocol: TVCSProtocol; const Url: string): TApplicationDefinition; static;
    class function HasErrors(const Status: TArray<TVCSFetchStatus>): boolean; static;
    class function GetInstalledProducts: THashSet<string>; static;
    class procedure AddPredefinedData(const Product: TRegisteredProduct); static;
  public
    class function Fetch(const AProductIds: TArray<string>; const OnlyInstalled: boolean): THashSet<string>; static;
    class function GetProduct(const Protocol: TVCSProtocol; const Url: string): TApplicationDefinition;

  end;

implementation
uses ULogger, UMultiLogger, UAppTerminated, VCS.Engine.Virtual,
     VCS.Engine.Factory, IOUtils, UConfigFolders, UTmsBuildSystemUtils, ULoggerTask,
{$IFDEF POSIX}
     Posix.UniStd, Posix.Stdio,
{$ENDIF}
     Removal.FolderDeleter, UProjectLoader;

{ TVCSManager }

procedure DeleteFolder(const Folder: string);
begin
  var Deleter := TFolderDeleter.Create;
  try
    Deleter.AddFolder(Folder);
    Deleter.DeleteAll;
  finally
    Deleter.Free;
  end;
end;

class function TVCSManager.DoGetProduct(const Protocol: TVCSProtocol; const Url: string): TApplicationDefinition;
begin
  // We originally made this very complex to try to avoid fetching the repo twice.
  // We would download to a tmp folder, and then on install, rename that folder.
  // But at the end, it is just simpler and more elegant to download the repo twice,
  // and do cleanup in place. It covers much better corner cases like someone yanking
  // the plug in the middle of a clone, then the clone living forever.

  Result := nil;
  try
    Logger.StartSpinner;
    try
      var Engine := TVCSFactory.Instance.GetEngine(Protocol);
      var TempGUIDProductFolder := TPath.Combine(Config.Folders.VCSTempFolder, GuidToStringN(TGUID.NewGuid));
      try
        Engine.GetFile(TProjectLoader.TMSBuildDefinitionFile, TempGUIDProductFolder, Url);
        var def := TProjectLoader.LoadProjectDefinition(TempGUIDProductFolder, 'root:application:description', true);
        try
          Result := def.Application.Clone;
        finally
          def.Free;
        end;
      finally
        DeleteFolder(TempGUIDProductFolder);
      end;
    finally
      Logger.StopSpinner;
    end;
  except
    Result.Free;
    raise;
  end;
end;

class function TVCSManager.GetProduct(const Protocol: TVCSProtocol; const Url: string): TApplicationDefinition;
begin
  Result := nil;
  var HasErrors := false;
  var ProtocolString := TRegisteredProduct.GetStringFromProtocol(Protocol);
  Logger.StartSection(TMessageType.VCSFetch, Url + ' from ' + ProtocolString);
  try
  try
    CheckAppTerminated;
    Logger.Info('Downloading config for ' + Url + ' from ' + ProtocolString);
    Result := DoGetProduct(Protocol, Url);
    Logger.Info('Downloaded config for ' + Url + ' from ' + ProtocolString);
  except
    on ex: Exception do
    begin
      Logger.Error(Format('Error fetching %s from %s: %s', [Url, ProtocolString, ex.Message]));
      HasErrors := true;
      Result.Free;
      raise;
    end;

  end;
  finally
    Logger.FinishSection(TMessageType.VCSFetch, HasErrors);
  end;
end;



class procedure TVCSManager.DoFetchProduct(const Product: TRegisteredProduct);
begin
  var Engine := TVCSFactory.Instance.GetEngine(Product.Protocol);
  var ProductFolder := TPath.Combine(Config.Folders.ProductsFolder, Product.ProductId);

  if Engine.GetProduct(ProductFolder, Product.Url) then exit; //direct get.
  

  if TDirectory.Exists(ProductFolder) then
  begin
    Engine.Pull(ProductFolder);
    exit;
  end;

  var TempProductFolder := TPath.Combine(Config.Folders.VCSTempFolder, Product.ProductId);

  if TDirectory.Exists(TempProductFolder) then //might be remaining from tms register-repo
  begin
    try
      Engine.Pull(TempProductFolder);
      RenameAndCheckFolder(TempProductFolder, ProductFolder);
      exit;
    except
      DeleteFolder(TempProductFolder);
    end;

  end;

  try
    Engine.Clone(TempProductFolder, Product.Url);
    RenameAndCheckFolder(TempProductFolder, ProductFolder);
  except
    DeleteFolder(TempProductFolder);
    raise;
  end;
end;

class procedure TVCSManager.AddPredefinedData(const Product: TRegisteredProduct);
begin
  if Product.PredefinedData.Tmsbuild_Yaml.Trim <> '' then
  begin
    var ProductFolder := TPath.Combine(Config.Folders.ProductsFolder, Product.ProductId);
    var tmsbuild_yaml := TPath.Combine(ProductFolder, TProjectLoader.TMSBuildDefinitionFile);
    //We give priority to the yaml that is in the product repo.
    //If the yaml exists in both the repo and the registry, we will use the repo.
    if not TFile.Exists(tmsbuild_yaml) then
    begin
      TFile.WriteAllText(tmsbuild_yaml, Product.PredefinedData.Tmsbuild_Yaml);
    end;
  end;
end;

class function TVCSManager.FetchProduct(const Product: TRegisteredProduct): TVCSFetchStatus;
begin
  Result := TVCSFetchStatus.Ok;
  Logger.StartSection(TMessageType.VCSFetch, Product.ProductId + ' from ' + Product.ProtocolString);
  try
    CheckAppTerminated;
    Logger.Info('Updating ' + Product.ProductId + ' from ' + Product.ProtocolString);
    DoFetchProduct(Product);
    AddPredefinedData(Product);
    begin

    end;

    Logger.Info('Updated ' + Product.ProductId + ' from ' + Product.ProtocolString);
  except
    on ex: Exception do
    begin
      Logger.Error(Format('Error fetching %s from %s: %s', [Product.ProductId, Product.ProtocolString, ex.Message]));
      Result := TVCSFetchStatus.Error;
    end;

  end;

  Logger.FinishSection(TMessageType.VCSFetch, Result = TVCSFetchStatus.Error);

end;

class function TVCSManager.CaptureFetch(TK: integer; Proc: TProc<integer>): TProc;
begin
  Result := procedure begin Proc(TK); end;
end;

class function TVCSManager.HasErrors(const Status: TArray<TVCSFetchStatus>): boolean;
begin
  Result := false;
  for var s in Status do if s = TVCSFetchStatus.Error then exit(true);
end;

function Completed(const Tasks: TArray<ITask>): boolean;
begin
  for var z := Low(Tasks) to High(Tasks) do
  begin
    if not (Tasks[z].Status in [TTaskStatus.Completed, TTaskStatus.Canceled, TTaskStatus.Exception]) then
    begin
      exit(False);
    end
  end;
  Result := true;
end;

class procedure TVCSManager.DoFetch(const Products: TList<TRegisteredProduct>);
begin
  var Tasks: TArray<ITask>;
  SetLength(Tasks, Products.Count);
  var Status: TArray<TVCSFetchStatus>;
  SetLength(Status, Products.Count);

  Logger.StartSpinner;
  try
    Logger.StartSection(TMessageType.VCSFetch, 'Retrieving from repositories');

    for var task := Low(Tasks) to High(Tasks) do
    begin
      Tasks[task] := RunTask(
      CaptureFetch(task,
      procedure (TK: integer)
      begin
        Status[TK] := FetchProduct(Products[TK]);
      end));
    end;

    while (not Completed(Tasks)) do
    begin
      if AppIsTerminated then
      begin
        for var task in Tasks do task.Cancel;
        TTask.WaitForAll(Tasks);
        exit;
      end;
      TTask.WaitForAny(Tasks, 1000);
    end;

    Logger.FinishSection(TMessageType.VCSFetch, HasErrors(Status));
  finally
    Logger.StopSpinner;
  end;

  LogVCSFetchSummary(Products, Status);

end;

class function TVCSManager.GetInstalledProducts: THashSet<string>;
begin
  Result := THashSet<string>.Create;
  if not TDirectory.Exists(Config.Folders.ProductsFolder) then exit;
  var Folders := TDirectory.GetDirectories(Config.Folders.ProductsFolder, '*', TSearchOption.soTopDirectoryOnly);
  for var Folder in Folders do
  begin
    if not TProjectLoader.HasProjectDefinition(Folder) then continue;
    var def := TProjectLoader.LoadProjectDefinition(Folder, 'root:application:id');
    try
      if not Result.Contains(def.Application.Id) then Result.Add(def.Application.Id);
    finally
      def.Free;
    end;
  end;
end;


class function TVCSManager.Fetch(const AProductIds: TArray<string>;
  const OnlyInstalled: boolean): THashSet<string>;
begin
  Result := THashSet<string>.Create;
  var ProductsToProcess := TList<TRegisteredProduct>.Create;
  try
    var InstalledProducts: THashSet<string> := nil;
    try
      if OnlyInstalled then InstalledProducts := GetInstalledProducts;

      if (Length(AProductIds) = 0) and (OnlyInstalled) then
      begin
        RegisteredVCSRepos.GetProducts('*', ProductsToProcess, InstalledProducts);
      end
      else begin
        for var Id in AProductIds do
        begin
          if RegisteredVCSRepos.GetProducts(Id, ProductsToProcess, InstalledProducts) then Result.Add(Id);
        end;
      end;
    finally
      InstalledProducts.Free;
    end;

    //We added all IDS, but they could have wildcards, like tms.biz.*.
    //We need both those with wildcards (to know later if a command like install tms.biz.* installed anything)
    //But also the real products, to handle dependencies.
    for var Prod in ProductsToProcess do Result.Add(Prod.ProductId);

    DoFetch(ProductsToProcess);
  finally
     ProductsToProcess.Free;
  end;
end;

end.

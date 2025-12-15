unit VCS.Manager;
{$I ../../tmssetup.inc}

interface
uses Classes, SysUtils, Commands.GlobalConfig, VCS.Registry,
{$IFDEF MSWINDOWS}
     Windows,
{$ENDIF}
     Generics.Collections, Threading, VCS.Summary, VCS.CoreTypes,
     UProjectDefinition, Fetching.InfoFile, Fetching.ProductVersion;

type

  TVCSManager = class
  private
    class procedure DoFetch(const Products: TList<TRegisteredVersionedProduct>); static;
    class function CaptureFetch(TK: integer; Proc: TProc<integer>): TProc; static;
    class function FetchProduct(const Product: TRegisteredVersionedProduct): TVCSFetchStatus; static;
    class procedure DoFetchProduct(const ProductFolderRoot, ProductFolder: string; const Product: TRegisteredVersionedProduct); static;
    class function HasErrors(const Status: TArray<TVCSFetchStatus>): boolean; static;
    class function GetInstalledProducts: THashSet<string>; static;
    class procedure AddPredefinedData(const ProductFolder: string; const Product: TRegisteredProduct); static;
    class function FindTmsBuildYaml(const ProductFolder: string; const Protocol: TVCSProtocol): string; static;
    class function ProcessDependencies(
      const ProductsToProcess: TObjectList<TRegisteredVersionedProduct>;
      const ProductsToProcessDict: TDictionary<string, string>;
      const AlreadyProcessed, InstalledProducts: THashSet<string>): boolean; static;
    class procedure GetDependencies(
      const Products: TObjectList<TRegisteredVersionedProduct>; const AlreadyProcessed, Dependencies: THashSet<string>); static;
  public
    class function Fetch(const AProductVersions: TArray<TProductVersion>; const OnlyInstalled: boolean): THashSet<string>; static;

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

class procedure TVCSManager.DoFetchProduct(const ProductFolderRoot, ProductFolder: string; const Product: TRegisteredVersionedProduct);
begin
  var Engine := TVCSFactory.Instance.GetEngine(Product.Product.Protocol);
  if Engine.GetProduct(ProductFolderRoot, ProductFolder, Product.Product.Url, Product.Product.Server, Product.Product.ProductId, Product.Version) then exit; //direct get.
  

  if TDirectory.Exists(ProductFolder) then
  begin
    Engine.Pull(ProductFolderRoot, ProductFolder, Product.Version);
    exit;
  end;

  var TempProductFolder := TPath.Combine(Config.Folders.VCSTempFolder, Product.Product.ProductId);

  if TDirectory.Exists(TempProductFolder) then
  begin
    DeleteFolder(TempProductFolder);
  end;

  try
    Engine.Clone(TempProductFolder, Product.Product.Url, Product.Version);
    RenameAndCheckFolder(TempProductFolder, ProductFolder);
    Engine.AfterClone(ProductFolderRoot, ProductFolder);
  except
    DeleteFolder(TempProductFolder);
    raise;
  end;
end;

class function TVCSManager.FindTmsBuildYaml(const ProductFolder: string; const Protocol: TVCSProtocol): string;
begin
  var Folder := ProductFolder;
  while True do
  begin
    var tmsbuild_yaml := TPath.GetFullPath(TPath.Combine(Folder, TProjectLoader.TMSBuildDefinitionFile));
    //We give priority to the yaml that is in the product repo.
    //If the yaml exists in both the repo and the registry, we will use the repo.
    if TFile.Exists(tmsbuild_yaml) then
    begin
      var Engine := TVCSFactory.Instance.GetEngine(Protocol);
      if Engine.FileIsVersioned(tmsbuild_yaml, TPath.GetDirectoryName(tmsbuild_yaml)) then exit('');
      exit(tmsbuild_yaml); //if the file exists, but it is not in version control, it was likely an older version we installed last time.
    end;

    //If it is a single folder, we assume the product is actually inside that single folder.
    //To be more strict, we could check for a single folder and no files, but we might have some files in there
    //(.etag, tmsfetch files).
    var Children := TDirectory.GetDirectories(Folder);
    if (Length(Children) <> 1) then exit(tmsbuild_yaml);
    Folder := TPath.Combine(Folder, Children[0]);
  end;

end;
class procedure TVCSManager.AddPredefinedData(const ProductFolder: string; const Product: TRegisteredProduct);
begin
  if Product.PredefinedData.Tmsbuild_Yaml.Trim <> '' then
  begin
    var tmsbuild_yaml := FindTmsBuildYaml(ProductFolder, Product.Protocol);
    if tmsbuild_yaml = '' then
    begin
      Logger.Trace('Kept existing tmsbuild.yaml in the repo');
      exit;
    end;

    Logger.Trace('Using tmsbuild.yaml from server');
    TFile.WriteAllText(tmsbuild_yaml, Product.PredefinedData.Tmsbuild_Yaml);
  end;
end;

class function TVCSManager.FetchProduct(const Product: TRegisteredVersionedProduct): TVCSFetchStatus;
begin
  Result := TVCSFetchStatus.Ok;
  if (Product.Pinned) then
  begin
    Logger.Info('Skipping ' + Product.Product.ProductId + ' because it is pinned.');
    exit;
  end;

  Logger.StartSection(TMessageType.VCSFetch, Product.ProductIdAndVersion + ' from ' + Product.Product.ProtocolString);
  try
    CheckAppTerminated;
    Logger.Info('Updating ' + Product.ProductIdAndVersion + ' from ' + Product.Product.ProtocolString);

    var ProductFolderRoot := TPath.Combine(Config.Folders.ProductsFolder, Product.Product.ProductId);
    var ProductFolder := TPath.Combine(ProductFolderRoot, 'src');

    DoFetchProduct(ProductFolderRoot, ProductFolder, Product);
    //Order here is important. Technically, we would have to first save tmsfetch.info before DoFetchProduct, so we can uninstall.
    //If something fails in DoFetchProduct, then `tms uninstall` will never get rid of it, since tmsfetch.info is missing.
    //But, if we do it in that order, and DoFetchProduct fails, you will end up with an empty folder with just tmsfetch.info inside.
    //So instead, we ensure DoFetchProduct either returns a full thing, or nothing. Then we save tmsfetch.info only if DoFetchFolder succeeded.
    var ProductVersion := Product.Version; if ProductVersion = '*' then ProductVersion := '';

    //When we fetch a new product, pinned is false
    TFetchInfoFile.SaveInFolder(ProductFolderRoot, Product.Product.ProductId, ProductVersion, Product.Product.Server, false);
    AddPredefinedData(ProductFolder, Product.Product);
    begin

    end;

    Logger.Info('Updated ' + Product.ProductIdAndVersion + ' from ' + Product.Product.ProtocolString);
  except
    on ex: Exception do
    begin
      Logger.Error(Format('Error fetching %s from %s: %s', [Product.ProductIdAndVersion, Product.Product.ProtocolString, ex.Message]));
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

class procedure TVCSManager.DoFetch(const Products: TList<TRegisteredVersionedProduct>);
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
    if HasErrors(Status) then raise Exception.Create('Errors downloading products from version control.');

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
    var YamlFolder := TProjectLoader.GetProjectDefinition(Folder);
    if YamlFolder = '' then continue;
    var def := TProjectLoader.LoadProjectDefinition(YamlFolder, 'root:application:id');
    try
      if not Result.Contains(def.Application.Id) then Result.Add(def.Application.Id);
    finally
      def.Free;
    end;
  end;
end;

class procedure TVCSManager.GetDependencies(const Products: TObjectList<TRegisteredVersionedProduct>; const AlreadyProcessed, Dependencies: THashSet<string>);
begin
  Dependencies.Clear;
  if not TDirectory.Exists(Config.Folders.ProductsFolder) then exit;

  for var Product in Products do
  begin
    var ProductId := Product.Product.ProductId;
    var Folder := CombinePath(Config.Folders.ProductsFolder, ProductId);
    var YamlFolder := TProjectLoader.GetProjectDefinition(Folder);
    if YamlFolder = '' then continue;
    var def := TProjectLoader.LoadProjectDefinition(YamlFolder);
    try
      for var Dependency in def.Dependencies do
      begin
        if AlreadyProcessed.Contains(Dependency.Id) then continue;
        AlreadyProcessed.Add(Dependency.Id);
        Logger.Trace(Format('%s depends on %s, added to processing queue', [ProductId, Dependency.Id]));
        Dependencies.Add(Dependency.Id);
      end;
    finally
      def.Free;
    end;
  end;
end;


class function TVCSManager.Fetch(const AProductVersions: TArray<TProductVersion>;
  const OnlyInstalled: boolean): THashSet<string>;
begin
  Result := THashSet<string>.Create;
  try
    var ProductsToProcess := TObjectList<TRegisteredVersionedProduct>.Create;
    try
      var ProductsToProcessDict := TDictionary<string, string>.Create;
      try
        var InstalledProducts: THashSet<string> := nil;
        try
          if OnlyInstalled then InstalledProducts := GetInstalledProducts;

          if (Length(AProductVersions) = 0) and (OnlyInstalled) then
          begin
            RegisteredVCSRepos.GetProducts(TProductVersion.Create('', ''), ProductsToProcess, ProductsToProcessDict, InstalledProducts);
          end
          else begin
            for var ProductVersion in AProductVersions do
            begin
              if RegisteredVCSRepos.GetProducts(ProductVersion, ProductsToProcess, ProductsToProcessDict, InstalledProducts) then Result.Add(ProductVersion.IdMask);
            end;
          end;

          //We added all IDS, but they could have wildcards, like tms.biz.*.
          //We need both those with wildcards (to know later if a command like install tms.biz.* installed anything)
          //But also the real products, to handle dependencies.
          for var Prod in ProductsToProcess do Result.Add(Prod.Product.ProductId);

          DoFetch(ProductsToProcess);

          while ProcessDependencies(ProductsToProcess, ProductsToProcessDict, Result, InstalledProducts) do;
        finally
          InstalledProducts.Free;
        end;
      finally
        ProductsToProcessDict.Free;
      end;
    finally
       ProductsToProcess.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

class function TVCSManager.ProcessDependencies(const ProductsToProcess: TObjectList<TRegisteredVersionedProduct>; const ProductsToProcessDict: TDictionary<string, string>; const AlreadyProcessed, InstalledProducts: THashSet<string>): boolean;
begin
  var Dependencies := THashSet<string>.Create;
  try
    GetDependencies(ProductsToProcess, AlreadyProcessed, Dependencies);
    ProductsToProcess.Clear;

    //If in an update, we could check that we the dependency is in installedproducts
    //But then, and update that added a new dependency wouldn't add it and the compile would break.
    //So we pass nil to the last parameter.
    for var Dependency in Dependencies do
    begin
      RegisteredVCSRepos.GetProducts(TProductVersion.Create(Dependency, '*'), ProductsToProcess, ProductsToProcessDict, nil);
    end;
  finally
    Dependencies.Free;
  end;


  if ProductsToProcess.Count = 0 then exit(false);

  DoFetch(ProductsToProcess);
  Result := true;
end;

end.

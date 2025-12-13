unit Fetching.Manager;

{$I ../../tmssetup.inc}

interface

uses
  System.Generics.Collections, System.IOUtils, System.SysUtils, UMultiLogger,
  Fetching.FetchItem, Fetching.InfoFile, Fetching.InstallInfo, UConfigFolders,
  URepositoryManager, Fetching.Options, Deget.Version, Fetching.ProductVersion;

type
  TStringSet = TDictionary<string, Boolean>;

  TFetchManager = class
  private
    FRepo: TRepositoryManager;
    FFolders: IBuildFolders;
    FFetchItems: TFetchItems;
    FInstalledProducts: TObjectList<TFetchInfoFile>;
    FAllInstalledProductsIncludingManual: THashSet<string>;
    FIgnoreMissing: Boolean;
    FAlreadyHandledProducts: THashSet<string>;
    FPinned: THashSet<string>;
    procedure AddMatchedItems(const ProductVersion: TProductVersion);
    procedure AddMatchedInstalledProducts(const ProductVersion: TProductVersion; Matched: TDictionary<string, TProductVersion>);
    function FindItem(const ProductId, Version: string; const CanBeAnyVersion, AllowDuplicates: boolean): TFetchItem;
    function ContainsItem(const ProductId, Version: string; const CanBeAnyVersion, AllowDuplicates: boolean): Boolean;
    function FindInstalledProduct(Item: TFetchItem): TFetchInfoFile;
    procedure FlagOutdatedItems;
    procedure DownloadOutdated;
    procedure ProcessDependencies;
    function GetProductDependencies(const ProductId: string): TArray<string>;
    function GetPinned: THashSet<string>;
  protected
    procedure UpdateItems;
    property Repo: TRepositoryManager read FRepo;
    function InstalledProducts: TList<TFetchInfoFile>;
    function AllInstalledProductsIncludingManual: THashSet<string>;
  public
    constructor Create(AFolders: IBuildFolders; ARepo: TRepositoryManager; AAlreadyHandledProducts: THashSet<string>); reintroduce;
    destructor Destroy; override;
    property FetchItems: TFetchItems read FFetchItems;
    procedure UpdateSelected(const ProductVersions: TArray<TProductVersion>);
    procedure UpdateInstalled(const ProductVersions: TArray<TProductVersion>);

    property AlreadyHandledProducts: THashSet<string> read FAlreadyHandledProducts;
    property Pinned: THashSet<string> read GetPinned;
  end;

  EFetchException = class(Exception)
  end;

implementation

uses
  System.Masks, Fetching.ParallelDownloader, UTmsBuildSystemUtils, UProjectLoader, UProjectDefinition, UProjectList,
  Commands.GlobalConfig;

{ TFetchManager }

procedure TFetchManager.ProcessDependencies;
begin
  for var Item in FetchItems do
    if (Item.Status in [TFetchStatus.Downloaded, TFetchStatus.SkippedUpToDate, TFetchStatus.SkippedManualVersionExists]) and not Item.DependenciesProcessed then
    begin
      Item.DependenciesProcessed := True;
      var Dependencies := GetProductDependencies(Item.ProductId);
      for var Dependency in Dependencies do
        AddMatchedItems(TProductVersion.Create(Dependency, '*'));
    end;
end;

procedure TFetchManager.AddMatchedInstalledProducts(const ProductVersion: TProductVersion; Matched: TDictionary<string, TProductVersion>);
begin
  Logger.Trace('Looking up installed products matching ' + ProductVersion.IdMask);
  var Found := False;
  for var InstalledProduct in InstalledProducts do
    if Config.IsIncluded(InstalledProduct.ProductId, ProductVersion.IdMask) then
    begin
      Found := True;
      var ExistingProduct: TProductVersion;
      if (Matched.TryGetValue(InstalledProduct.ProductId, ExistingProduct)) then
      begin
        if (ExistingProduct.Version <> ProductVersion.Version) then
        begin
          raise Exception.Create('The product ' + InstalledProduct.ProductId +' was requested to be installed in versions "' + ExistingProduct.Version + '" and "' + ProductVersion.Version + '" at the same time.');
        end;
      end else
      begin
        Matched.Add(InstalledProduct.ProductId, ProductVersion);
      end;
      Logger.Trace(Format('Product %s:%s to be analyzed', [InstalledProduct.ProductId, InstalledProduct.Version]));
    end else
    begin
      if Config.IsExcluded(InstalledProduct.ProductId) then Logger.Trace('Ignoring ' + InstalledProduct.ProductId + ' because it is in the "excluded products" section of tms.config.yaml');

    end;

  if not Found and not FAlreadyHandledProducts.Contains(ProductVersion.IdMask) then
  begin
    var ErrorMessage := Format('Could not find any products matching %s', [ProductVersion.IdMask]);
    if Config.IsExcluded(ProductVersion.IdMask) then ErrorMessage := Format('The product %s is excluded in the section "excluded products" from tms.config.yaml.', [ProductVersion.IdMask]);

    raise Exception.Create(ErrorMessage);
  end;
end;

procedure TFetchManager.AddMatchedItems(const ProductVersion: TProductVersion);
begin
  if Repo = nil then
  begin
    Logger.Trace('Skipping API server because it isn''t enabled or there are no credentials.');
  end;
  Logger.Trace('Looking up products matching ' + ProductVersion.IdMask + ' with version "' + ProductVersion.Version + '"');
  var Found := False;

  if Repo <> nil then
  begin
    for var RepoProduct in Repo.Products do
      if Config.IsIncluded(RepoProduct.Id, ProductVersion.IdMask) then
      begin
        // Ignore products that are not licensed unless the ProductIdMask is exactly the product name
        // (meaning it was explicitly provided by the user or from the list of installed products
        if (RepoProduct.LicenseStatus in [TLicenseStatus.none]) and (ProductVersion.IdMask <> RepoProduct.Id) then
          Continue;

        // Ignore products that were not released yet
        if RepoProduct.LatestVersion = nil then
        begin
          Logger.Trace(Format('Ignoring %s - no version available', [RepoProduct.Id]));
          Continue;
        end;

        Found := True;

        var VersionToInstall := ProductVersion.Version;
        if (VersionToInstall = '') or (VersionToInstall = '*') then VersionToInstall := RepoProduct.LatestVersion.Version;

        if ContainsItem(RepoProduct.Id, VersionToInstall, ProductVersion.Version = '*', false) then
        begin
          Logger.Trace(Format('Ignoring %s:%s because it was already added', [RepoProduct.Id, VersionToInstall]));
          Continue;
        end;

        var Item := TFetchItem.Create(RepoProduct.Id, VersionToInstall, Repo.Server, ProductVersion.Version = '*');
        FFetchItems.Add(Item);
        Item.Internal := RepoProduct.Internal;

        //We only have the FileHash of the latest version. If we aren't installing that, we will just leave it empty, and calculate it at download time.
        if VersionToInstall = RepoProduct.LatestVersion.Version then Item.FileHash := RepoProduct.LatestVersion.FileHash;
        if RepoProduct.Id = TRepositoryManager.TMSSetupProductId then
          Item.SkipExtraction := True;
        Logger.Info(Format('Found %s in repository', [RepoProduct.Id]));
      end else
      begin
        if Config.IsExcluded(RepoProduct.Id) then Logger.Trace('Ignoring ' + RepoProduct.Id + ' because it is in the "excluded products" section of tms.config.yaml');

      end;
  end;

  if not Found and (ProductVersion.IdMask <> TRepositoryManager.TMSSetupProductId) and not FAlreadyHandledProducts.Contains(ProductVersion.IdMask) then
  begin
    var ErrorMessage := Format('Could not find any products matching %s', [ProductVersion.IdMask]);
    if Config.IsExcluded(ProductVersion.IdMask) then ErrorMessage := Format('The product %s is excluded in the section "excluded products" from tms.config.yaml.', [ProductVersion.IdMask]);

    if FIgnoreMissing then
    begin
      var RepoServer := '';
      if Repo <> nil then RepoServer := Repo.Server;
      var Item := TFetchItem.Create(ProductVersion.IdMask, ProductVersion.Version, RepoServer, false);
      FFetchItems.Add(Item);
      Item.Status := TFetchStatus.Failed;
      Logger.Error(ErrorMessage);
    end
    else
      raise Exception.Create(ErrorMessage);
  end;
end;

function TFetchManager.ContainsItem(const ProductId, Version: string; const CanBeAnyVersion, AllowDuplicates: boolean): Boolean;
begin
  Result := FindItem(ProductId, Version, CanBeAnyVersion, AllowDuplicates) <> nil;
end;

constructor TFetchManager.Create(AFolders: IBuildFolders; ARepo: TRepositoryManager; AAlreadyHandledProducts: THashSet<string>);
begin
  FFetchItems := TFetchItems.Create;
  FAlreadyHandledProducts := AAlreadyHandledProducts;
  FRepo := ARepo;
  FFolders := AFolders;
end;

destructor TFetchManager.Destroy;
begin
  FFetchItems.Free;
  FInstalledProducts.Free;
  FAllInstalledProductsIncludingManual.Free;
  FPinned.Free;
  inherited;
end;

procedure TFetchManager.DownloadOutdated;
begin
  var Downloader := TParallelDownloader.Create(FFetchItems, FRepo, FFolders);
  try
    Downloader.Fetch;
  finally
    Downloader.Free;
  end;
end;

function TFetchManager.FindInstalledProduct(Item: TFetchItem): TFetchInfoFile;
begin
  for var Installed in InstalledProducts do
    if Installed.ProductId = Item.ProductId then
      Exit(Installed);
  Result := nil;
end;

function TFetchManager.FindItem(const ProductId, Version: string; const CanBeAnyVersion, AllowDuplicates: boolean): TFetchItem;
begin
  for var Item in FetchItems do
    if (Item.ProductId = ProductId) then
    begin
      if CanBeAnyVersion then Exit(Item);
      if Item.CanBeAnyVersion then
      begin
        Item.CanBeAnyVersion := false;
        Exit(Item);
      end;

      if (Item.Version = Version) then
       Exit(Item);

      if not AllowDuplicates then
      begin
        raise Exception.Create('The product ' + Item.ProductId +' was requested to be installed in versions "' + Version + '" and "' + Item.Version + '" at the same time.');
      end;
    end;
  Result := nil;
end;

function TFetchManager.InstalledProducts: TList<TFetchInfoFile>;
const
  {$i ../../../Version.inc}
begin
  if FInstalledProducts = nil then
  begin
    FInstalledProducts := TObjectList<TFetchInfoFile>.Create;
    GetFetchedProducts(FFolders.ProductsFolder, FInstalledProducts);

    // Special installed product: tms setup
    var TMSSetupInfoFile := TFetchInfoFile.Create;
    FInstalledProducts.Add(TMSSetupInfoFile);
    TMSSetupInfoFile.ProductId := TRepositoryManager.TMSSetupProductId;
    TMSSetupInfoFile.ProductPath := '';
    TMSSetupInfoFile.Channel := 'production';
    TMSSetupInfoFile.Version := TMSVersion;
  end;
  Result := FInstalledProducts;
end;

function TFetchManager.AllInstalledProductsIncludingManual: THashSet<string>;
begin
  if FAllInstalledProductsIncludingManual = nil then
  begin
    FAllInstalledProductsIncludingManual := THashSet<string>.Create;
    var Projects := TProjectList.Create;
    try
      TProjectLoader.LoadProjects(Config.GetAllRootFolders, Projects);
      for var Project in Projects.All do
      begin
        FAllInstalledProductsIncludingManual.Add(Project.Application.Id);
      end;
    finally
      Projects.Free;
    end;
  end;
  Result := FAllInstalledProductsIncludingManual;
end;

procedure TFetchManager.UpdateSelected(const ProductVersions: TArray<TProductVersion>);
begin
  // Since product ids are provided explicit, fail the command if id does not exist.
  FIgnoreMissing := False;
  FFetchItems.Clear;

  // Retrieve initial raw list of products that should be installed
  for var ProductVersion in ProductVersions do
    AddMatchedItems(ProductVersion);

  UpdateItems;
end;

procedure TFetchManager.UpdateInstalled(const ProductVersions: TArray<TProductVersion>);
begin
  // find the installed products to be updated
  var ProductsToUpdate: TArray<TProductVersion>;
  begin
    var MatchedProducts := TDictionary<string, TProductVersion>.Create;
    try
      for var ProductVersion in ProductVersions do
        AddMatchedInstalledProducts(ProductVersion, MatchedProducts);

      // if no productid mask was provided, then add all
      if (Length(ProductVersions) = 0) and (InstalledProducts.Count > 0) then
        AddMatchedInstalledProducts(TProductVersion.Create('', ''), MatchedProducts);

      ProductsToUpdate := nil; SetLength(ProductsToUpdate, MatchedProducts.Count);
      var i := 0;
      for var MatchedProduct in MatchedProducts do
      begin
        ProductsToUpdate[i] := TProductVersion.Create(MatchedProduct.Key, MatchedProduct.Value.Version);
        inc(i);
      end;
    finally
      MatchedProducts.Free;
    end;
  end;

  // If updating one product fails, do not block the update of the others
  FIgnoreMissing := True;
  FFetchItems.Clear;

  for var ProductVersion in ProductsToUpdate do
    AddMatchedItems(ProductVersion);

  UpdateItems;
end;

procedure TFetchManager.UpdateItems;
begin
  // Always check for tms smart setup
  Logger.Trace('Looking up for ' + TRepositoryManager.TMSSetupProductId);
  AddMatchedItems(TProductVersion.Create(TRepositoryManager.TMSSetupProductId, ''));

  // Process all fetch items
  while FetchItems.ContainsStatus(TFetchStatus.Unprocessed) do
  begin
    FlagOutdatedItems;
    DownloadOutdated;
    if FetchItems.ContainsStatus(TFetchStatus.Failed) then
      raise EFetchException.Create('Some download tasks have failed');
    ProcessDependencies;
  end;
end;

procedure TFetchManager.FlagOutdatedItems;
begin
  // First, check which items must be downloaded or are already downloaded
  for var Item in FetchItems do
  begin
    if Pinned.Contains(Item.ProductId) then
    begin
      Item.Status := TFetchStatus.SkippedPinned;
      Logger.Info('Skipped ' + Item.ProductId + ' because it is pinned');
      continue;
    end;

    if Item.Status = TFetchStatus.Unprocessed then
    begin
      var Installed := FindInstalledProduct(Item);
      if Installed = nil then
      begin
        if AllInstalledProductsIncludingManual.Contains(Item.ProductId) then
        begin
          Item.Status := TFetchStatus.SkippedManualVersionExists;
          Logger.Trace(Format('%s skipped, there is a local version manually installed.', [Item.ProductId]));
        end else
        begin
          Item.Status := TFetchStatus.Outdated;
          Logger.Trace(Format('%s flagged to be downloaded, local version not found', [Item.ProductId]));
        end;
        Continue;
      end;

      if Item.Version <> TVersion(Installed.Version) then
      begin
        Item.Status := TFetchStatus.Outdated;
        Logger.Trace(Format('%s flagged to be downloaded, local version %s is different from requested', [Item.ProductId, Installed.Version]));
        Continue;
      end;

      Item.Status := TFetchStatus.SkippedUpToDate;
      Logger.Trace(Format('%s skipped, local version is already in the correct version', [Item.ProductId]));
    end;
  end;
end;

function TFetchManager.GetPinned: THashSet<string>;
begin
  if FPinned <> nil then exit(FPinned);
  FPinned := THashSet<string>.Create;
  GetFetchedProducts(Config.Folders.ProductsFolder, FPinned,
    function(item: TFetchInfoFile): boolean
    begin
      Result := item.Pinned;
    end);
  Result := FPinned;
end;

function TFetchManager.GetProductDependencies(const ProductId: string): TArray<string>;
begin
  Result := [];
  var Folder := CombinePath(FFolders.ProductsFolder, ProductId);
  var YamlFolder := TProjectLoader.GetProjectDefinition(Folder);
  if YamlFolder = '' then Exit;

  var def := TProjectLoader.LoadProjectDefinition(YamlFolder);
  try
    for var Dependency in def.Dependencies do
    begin
      Logger.Trace(Format('%s depends on %s, added to processing queue', [ProductId, Dependency.Id]));
      Result := Result + [Dependency.Id];
    end;
  finally
    def.Free;
  end;
end;

end.

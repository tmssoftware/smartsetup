unit Fetching.Manager;

{$I ../../tmssetup.inc}

interface

uses
  System.Generics.Collections, System.IOUtils, System.SysUtils, UMultiLogger,
  Fetching.FetchItem, Fetching.InfoFile, Fetching.InstallInfo, UConfigFolders,
  URepositoryManager, Fetching.Options, Deget.Version;

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
    procedure AddMatchedItems(const ProductIdMask: string);
    procedure AddMatchedInstalledProducts(const ProductIdMask: string; Matched: TStringSet);
    function FindItem(const ProductId, Version: string): TFetchItem;
    function ContainsItem(const ProductId, Version: string): Boolean;
    function FindInstalledProduct(Item: TFetchItem): TFetchInfoFile;
    procedure FlagOutdatedItems;
    procedure DownloadOutdated;
    procedure ProcessDependencies;
    function GetProductDependencies(const ProductId: string): TArray<string>;
  protected
    procedure UpdateItems;
    property Repo: TRepositoryManager read FRepo;
    function InstalledProducts: TList<TFetchInfoFile>;
    function AllInstalledProductsIncludingManual: THashSet<string>;
  public
    constructor Create(AFolders: IBuildFolders; ARepo: TRepositoryManager; AAlreadyHandledProducts: THashSet<string>); reintroduce;
    destructor Destroy; override;
    property FetchItems: TFetchItems read FFetchItems;
    procedure UpdateSelected(const ProductIdMasks: TArray<string>);
    procedure UpdateInstalled(const ProductIdMasks: TArray<string>);

    property AlreadyHandledProducts: THashSet<string> read FAlreadyHandledProducts;
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
        AddMatchedItems(Dependency);
    end;
end;

procedure TFetchManager.AddMatchedInstalledProducts(const ProductIdMask: string; Matched: TStringSet);
begin
  Logger.Trace('Looking up installed products matching ' + ProductIdMask);
  var Found := False;
  for var InstalledProduct in InstalledProducts do
    if MatchesMask(InstalledProduct.ProductId, ProductIdMask) then
    begin
      Found := True;
      Matched.AddOrSetValue(InstalledProduct.ProductId, True);
      Logger.Trace(Format('Product %s:%s to be analyzed', [InstalledProduct.ProductId, InstalledProduct.Version]));
    end;

  if not Found and not FAlreadyHandledProducts.Contains(ProductIdMask) then
  begin
    var ErrorMessage := Format('Could not find any products matching %s', [ProductIdMask]);
    raise Exception.Create(ErrorMessage);
  end;
end;

procedure TFetchManager.AddMatchedItems(const ProductIdMask: string);
begin
  Logger.Trace('Looking up products matching ' + ProductIdMask);
  var Found := False;
  for var RepoProduct in Repo.Products do
    if MatchesMask(RepoProduct.Id, ProductIdMask) then
    begin
      // Ignore products that are not licensed unless the ProductIdMask is exactly the product name
      // (meaning it was explicitly provided by the user or from the list of installed products
      if (RepoProduct.LicenseStatus in [TLicenseStatus.none]) and (ProductIdMask <> RepoProduct.Id) then
        Continue;

      // Ignore products that were not released yet
      if RepoProduct.LatestVersion = nil then
      begin
        Logger.Trace(Format('Ignoring %s - no version available', [RepoProduct.Id]));
        Continue;
      end;

      Found := True;
      if ContainsItem(RepoProduct.Id, RepoProduct.LatestVersion.Version) then
      begin
        Logger.Trace(Format('Ignoring %s because it was already added', [RepoProduct.Id]));
        Continue;
      end;

      var Item := TFetchItem.Create(RepoProduct.Id, RepoProduct.LatestVersion.Version, Repo.Server);
      FFetchItems.Add(Item);
      Item.Internal := RepoProduct.Internal;
      Item.FileHash := RepoProduct.LatestVersion.FileHash;
      if RepoProduct.Id = TRepositoryManager.TMSSetupProductId then
        Item.SkipExtraction := True;
      Logger.Info(Format('Found %s:%s in repository', [RepoProduct.Id, RepoProduct.LatestVersion.Version]));
    end;

  if not Found and (ProductIdMask <> TRepositoryManager.TMSSetupProductId) and not FAlreadyHandledProducts.Contains(ProductIdMask) then
  begin
    var ErrorMessage := Format('Could not find any products matching %s', [ProductIdMask]);
    if FIgnoreMissing then
    begin
      var Item := TFetchItem.Create(ProductIdMask, '', Repo.Server);
      FFetchItems.Add(Item);
      Item.Status := TFetchStatus.Failed;
      Logger.Error(ErrorMessage);
    end
    else
      raise Exception.Create(ErrorMessage);
  end;
end;

function TFetchManager.ContainsItem(const ProductId, Version: string): Boolean;
begin
  Result := FindItem(ProductId, Version) <> nil;
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

function TFetchManager.FindItem(const ProductId, Version: string): TFetchItem;
begin
  for var Item in FetchItems do
    if (Item.ProductId = ProductId) and (Item.Version = Version) then
       Exit(Item);
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

procedure TFetchManager.UpdateSelected(const ProductIdMasks: TArray<string>);
begin
  // Since product ids are provided explicit, fail the command if id does not exist.
  FIgnoreMissing := False;
  FFetchItems.Clear;

  // Retrieve initial raw list of products that should be installed
  for var ProductIdMask in ProductIdMasks do
    AddMatchedItems(ProductIdMask);

  UpdateItems;
end;

procedure TFetchManager.UpdateInstalled(const ProductIdMasks: TArray<string>);
begin
  // find the installed products to be updated
  var ProductsToUpdate: TArray<string>;
  begin
    var MatchedProducts := TStringSet.Create;
    try
      for var ProductIdMask in ProductIdMasks do
        AddMatchedInstalledProducts(ProductIdMask, MatchedProducts);

      // if no productid mask was provided, then add all
      if (Length(ProductIdMasks) = 0) and (InstalledProducts.Count > 0) then
        AddMatchedInstalledProducts('*', MatchedProducts);

      ProductsToUpdate := MatchedProducts.Keys.ToArray;
    finally
      MatchedProducts.Free;
    end;
  end;

  // If updating one product fails, do not block the update of the others
  FIgnoreMissing := True;
  FFetchItems.Clear;

  for var ProductId in ProductsToUpdate do
    AddMatchedItems(ProductId);

  UpdateItems;
end;

procedure TFetchManager.UpdateItems;
begin
  // Always check for tms smart setup
  Logger.Trace('Looking up for ' + TRepositoryManager.TMSSetupProductId);
  AddMatchedItems(TRepositoryManager.TMSSetupProductId);

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

      if Item.Version > TVersion(Installed.Version) then
      begin
        Item.Status := TFetchStatus.Outdated;
        Logger.Trace(Format('%s flagged to be downloaded, local version %s is outdated', [Item.ProductId, Installed.Version]));
        Continue;
      end;

      Item.Status := TFetchStatus.SkippedUpToDate;
      Logger.Trace(Format('%s skipped, local version is already updated', [Item.ProductId]));
    end;
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

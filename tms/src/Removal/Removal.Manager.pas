unit Removal.Manager;

{$I ../../tmssetup.inc}

interface

uses
  System.Generics.Collections, System.IOUtils, System.SysUtils, UMultiLogger,
  UConfigFolders, Deget.Version, Fetching.InfoFile, Fetching.InstallInfo, Removal.Item;

type
  TRemovalManager = class
  private
    FFolders: IBuildFolders;
    FRootFolders: TArray<string>;
    FRemovalItems: TRemovalItems;
    FInstalledProducts: TObjectList<TFetchInfoFile>;
    FCascade: Boolean;
    FForce: Boolean;
    FIncludeManual: Boolean;
    FDependencies: TDictionary<string, TList<string>>;
    procedure AddMatchedItems(const ProductIdMask: string);
    procedure ProcessDependencies;
    function GetProductDependencies(const ProductPath: string): TArray<string>;
    procedure CheckDependents;
    procedure LoadDependencyMap;
    function IsProductInstalled(const ProductIdMask: string): boolean;
  protected
    function InstalledProducts: TEnumerable<TFetchInfoFile>;
  public
    constructor Create(AFolders: IBuildFolders; const ARootFolders: TArray<string>); reintroduce;
    destructor Destroy; override;
    procedure ProcessSelected(const ProductIdMasks: TArray<string>);
    property RemovalItems: TRemovalItems read FRemovalItems;

    // If true, dependencies of the selected products will also be flagged for removal
    property Cascade: Boolean read FCascade write FCascade;

    // If true, remove products even if there are other products depennding on it.
    // If false (default), raise an error
    property Force: Boolean read FForce write FForce;

    // If true, we will search for products we didn't originally install via tms install.
    property IncludeManual: Boolean read FIncludeManual write FIncludeManual;
  end;

implementation

uses
  System.Masks, UTmsBuildSystemUtils, UProjectLoader, UProjectList, UDelayedErrors;

{ TRemovalManager }

procedure GetAllProducts(const RootFolders: TArray<string>; Products: TObjectList<TFetchInfoFile>);
begin
  var Projects := TProjectList.Create;
  try
    TProjectLoader.LoadProjects(RootFolders, Projects);
    for var Project in Projects.All do
    begin
      var ProjectFolder := TPath.GetDirectoryName(Project.FullPath);
      if TPath.GetDirectoryName(ProjectFolder) = '' then continue; //Don't remove projects at the root of a drive.

      Products.Add(TFetchInfoFile.Create(Project.Application.Id, ProjectFolder, Project.Application.Version, 'unknown', ''));
    end;
  finally
    Projects.Free;
  end;
end;


function TRemovalManager.IsProductInstalled(const ProductIdMask: string): boolean;
begin
  var TmpInstalledProducts := TObjectList<TFetchInfoFile>.Create;
  try
    GetAllProducts(FRootFolders, TmpInstalledProducts);
    for var Product in TmpInstalledProducts do
    begin
      if MatchesMask(Product.ProductId, ProductIdMask) then exit (true);
    end;
  finally
    TmpInstalledProducts.Free;
  end;
  Result := false;
end;

procedure TRemovalManager.AddMatchedItems(const ProductIdMask: string);
begin
  Logger.Trace('Looking up products matching ' + ProductIdMask);
  var Found := False;
  for var InstalledProduct in InstalledProducts do
    if MatchesMask(InstalledProduct.ProductId, ProductIdMask) then
    begin
      Found := True;
      if RemovalItems.Contains(InstalledProduct.ProductId, InstalledProduct.Version) then
      begin
        Logger.Trace(Format('Ignoring %s because it was already added', [InstalledProduct.ProductId]));
        Continue;
      end;
      var Item := TRemovalItem.Create(InstalledProduct.ProductId, InstalledProduct.ProductPath, InstalledProduct.Version);
      FRemovalItems.Add(Item);
      Logger.Trace(Format('Product %s:%s flagged for removal', [InstalledProduct.ProductId, InstalledProduct.Version]));
    end;

  if not Found then
  begin
    var ErrorMessage := Format('Could not find any products matching %s', [ProductIdMask]);
    if IsProductInstalled(ProductIdMask) then
    begin
      ErrorMessage := Format('We found products matching %s, but those were not installed via Smart Setup, and we cannot remove them. To uninstall them, please delete the folders manually and run "tms build" after that.', [ProductIdMask]);
    end;


//    if FIgnoreMissing then
//    begin
//      var Item := TRemovalItem.Create(ProductIdMask, '');
//      FRemovealItems.Add(Item);
//      Item.Status := TRemovalStatus.Failed;
//      Logger.Error(ErrorMessage);
//    end
//    else
      raise Exception.Create(ErrorMessage);
  end;
end;

procedure TRemovalManager.CheckDependents;
begin
  LoadDependencyMap;
  for var Item in RemovalItems do
  begin
    for var InstalledProduct in InstalledProducts do
      // if the dependent products is also going to be removed, then we're fine
      // we only need to check for products that will remain installed
      if not RemovalItems.Contains(InstalledProduct.ProductId, InstalledProduct.Version) then
      begin
        var DependencyList: TList<string>;
        if FDependencies.TryGetValue(InstalledProduct.ProductId, DependencyList) then
          if DependencyList.Contains(Item.ProductId) then
            Item.Dependents.Add(InstalledProduct.ProductId);
      end;
    if Item.Dependents.Count > 0 then
    begin
      Item.Status := TRemovalStatus.Failed;
      Logger.Error(Format('Cannot remove %s because other products depend on it: %s', [Item.ProductId, string.Join(', ', Item.Dependents.ToArray)]));
    end;
  end;
end;

constructor TRemovalManager.Create(AFolders: IBuildFolders; const ARootFolders: TArray<string>);
begin
  FRemovalItems := TRemovalItems.Create;
  FFolders := AFolders;
  FRootFolders := ARootFolders;
  FDependencies := TObjectDictionary<string, TList<string>>.Create([doOwnsValues]);
end;

destructor TRemovalManager.Destroy;
begin
  FRemovalItems.Free;
  FInstalledProducts.Free;
  FDependencies.Free;
  inherited;
end;

function TRemovalManager.GetProductDependencies(const ProductPath: string): TArray<string>;
begin
  Result := [];
  if ProductPath = '' then
    Exit;
  var YamlProductPath := TProjectLoader.GetProjectDefinition(ProductPath);
  if YamlProductPath = '' then
    Exit;

  var def := TProjectLoader.LoadProjectDefinition(YamlProductPath);
  try
    for var Dependency in def.Dependencies do
      Result := Result + [Dependency.Id];
  finally
    def.Free;
  end;
end;


function TRemovalManager.InstalledProducts: TEnumerable<TFetchInfoFile>;
begin
  if FInstalledProducts = nil then
  begin
    FInstalledProducts := TObjectList<TFetchInfoFile>.Create;
    if IncludeManual then
    begin
      GetAllProducts(FRootFolders, FInstalledProducts);
    end
    else
    begin
      GetFetchedProducts(FFolders.ProductsFolder, FInstalledProducts);
    end;

  end;
  Result := FInstalledProducts;
end;

procedure TRemovalManager.LoadDependencyMap;
begin
  // Build a list of dependency-chain
  FDependencies.Clear;
  for var InstalledProduct in InstalledProducts do
    FDependencies.Add(InstalledProduct.ProductId,
      TList<string>.Create(GetProductDependencies(InstalledProduct.ProductPath)));
end;

procedure TRemovalManager.ProcessDependencies;
var
  ItemsAdded: Boolean;
begin
  repeat
    ItemsAdded := False;
    for var Item in RemovalItems do
      if not Item.DependenciesProcessed then
      begin
        Item.DependenciesProcessed := True;

        // Flag dependencies to be removed, if cascade is enabled
        if FCascade then
          for var Dependency in GetProductDependencies(Item.ProductPath) do
          begin
            AddMatchedItems(Dependency);
            ItemsAdded := True;
          end;
      end;
  until not ItemsAdded;
end;

procedure TRemovalManager.ProcessSelected(const ProductIdMasks: TArray<string>);
begin
  FRemovalItems.Clear;

  // Retrieve initial raw list of products that should be installed
  for var ProductIdMask in ProductIdMasks do
    AddMatchedItems(ProductIdMask);

  // Add cascaded dependencies
  ProcessDependencies;

  // Unless Force is specified, do not allow removal of products that are dependencies of remaining products
  if not FForce then
    CheckDependents;

  var Failed := RemovalItems.GetProductsWithStatus(TRemovalStatus.Failed);
  if Failed <> '' then
    DelayedErrors.Add('Uninstall failed: ' + Failed);
end;

end.

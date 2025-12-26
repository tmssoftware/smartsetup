unit Status.Manager;

interface

uses
  System.Generics.Collections, System.Generics.Defaults, System.SysUtils, Deget.Version, UConfigFolders, UProjectDefinition,
  Deget.CoreTypes, UConfigDefinition, Fetching.InfoFile;

type
  TPlatformStatus = class
  private
    FIsBuilt: Boolean;
    FIsRegistered: Boolean;
    FRegisteredItems: string;
  public
    property IsBuilt: Boolean read FIsBuilt write FIsBuilt;
    property IsRegistered: Boolean read FIsRegistered write FIsRegistered;
    property RegisteredItems: string read FRegisteredItems write FRegisteredItems;
  end;

  TIDEStatus = class
  private
    FPlatformStatusMap: TDictionary<TPlatform, TPlatformStatus>;
  public
    constructor Create;
    destructor Destroy; override;
    function PlatformStatus(const APlatform: TPlatform): TPlatformStatus;
  end;

  TProductStatus = class
  private
    FId: string;
    FName: string;
    FVersion: TLenientVersion;
    FInstalledVersion: TLenientVersion; //For API products, this is the same as FVersion. For VCS, FVersion might be empty (head), but this field will have the commit so it can be restored.
    FChannel: string;
    FServer: string;
    FPinned: boolean;
    FFetched: Boolean;
    FProject: TProjectDefinition;
    FIDEStatusMap: TDictionary<TIDEName, TIDEStatus>;
    procedure SetProject(const Value: TProjectDefinition);
  public
    constructor Create;
    destructor Destroy; override;
    function DisplayName: string;
    function IDEStatus(const IDEName: TIDEName): TIDEStatus;
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property Version: TLenientVersion read FVersion write FVersion;
    property InstalledVersion: TLenientVersion read FInstalledVersion write FInstalledVersion;
    property Channel: string read FChannel write FChannel;
    property Server: string read FServer write FServer;
    property Pinned: boolean read FPinned write FPinned;
    property Fetched: Boolean read FFetched write FFetched;
    property Project: TProjectDefinition read FProject write SetProject;
  end;

  TStatusManager = class
  private
    FConfig: TConfigDefinition;
    FProducts: TList<TProductStatus>;
    procedure LoadRemoteProducts;
    procedure LoadBuildableProducts;
    procedure UpdateProductStatus(Product: TProductStatus);
    function GetInstalledVersion(const Version: TLenientVersion; const ProductId: string): TLenientVersion;
  protected
    function FindProduct(const ProductId: string): TProductStatus;
  public
    constructor Create(AConfig: TConfigDefinition);
    destructor Destroy; override;
    procedure Update;
    property Products: TList<TProductStatus> read FProducts;
  end;

implementation

uses
  Fetching.InstallInfo, UProjectLoader, UProjectList, UFileHasher, UProjectInstaller, VCS.Manager;

{ TStatusManager }

constructor TStatusManager.Create(AConfig: TConfigDefinition);
begin
  FConfig := AConfig;
  FProducts := TObjectList<TProductStatus>.Create;
end;

destructor TStatusManager.Destroy;
begin
  FProducts.Free;
  inherited;
end;

function TStatusManager.FindProduct(const ProductId: string): TProductStatus;
begin
  for var Product in Products do
    if Product.Id = ProductId then
      Exit(Product);
  Result := nil;
end;

procedure TStatusManager.LoadBuildableProducts;
begin
  var Projs := TProjectList.Create;
  try
    TProjectLoader.LoadProjects(FConfig.GetAllRootFolders, Projs);
    while Projs.All.Count > 0 do
    begin
      var Proj := Projs.All.First;
      var Product := FindProduct(Proj.Application.Id);
      if Product = nil then
      begin
        Product := TProductStatus.Create;
        Products.Add(Product);
        Product.Id := Proj.Application.Id;
      end;

      // override properties, use data from tmsbuild.yaml instead of fetch.info.txt
      Product.Name := Proj.Application.Name;

      //Prefer the info in fetch.info.txt for products that come from github. Those will have likely the tmsbuild.yaml version empty or wrong.
      //We could also check if Proj.Application.Version = '' then... but the version in tmsbuild.yaml won't be at the commit level. Lots of commits would have the same version.
      if (Product.Version = '') and (Proj.Application.Version <> '') then Product.Version := TLenientVersion.Create(Proj.Application.Version, TVersionType.Semantic); //versions allowed in the yaml are semantic.
      if Product.InstalledVersion = '' then Product.InstalledVersion := GetInstalledVersion(Product.Version, Product.Id);



      Product.Project := Projs.All.Extract(Proj);
    end;
  finally
    Projs.Free;
  end;
end;

function TStatusManager.GetInstalledVersion(const Version: TLenientVersion; const ProductId: string): TLenientVersion;
begin
  if (Version <> '') and (Version <> '__latest') then exit(Version);
  Result := TLenientVersion.Create(TVCSManager.GetCurrentCommit(ProductId), TVersionType.FreeForm);
end;

procedure TStatusManager.LoadRemoteProducts;
begin
  var Items := TObjectList<TFetchInfoFile>.Create;
  try
    GetFetchedProducts(FConfig.Folders.ProductsFolder, Items);
    for var Item in Items do
    begin
      var Product := TProductStatus.Create;
      Products.Add(Product);
      Product.Id := Item.ProductId;
      Product.Name := Item.DisplayName;
      Product.Version := Item.Version;
      Product.InstalledVersion := GetInstalledVersion(Item.Version, Item.ProductId);
      Product.Channel := Item.Channel;
      Product.Server := Item.Server;
      Product.Pinned := Item.Pinned;
      Product.Fetched := True;
    end;
  finally
    Items.Free;
  end;
end;

procedure TStatusManager.Update;
begin
  FProducts.Clear;
  LoadRemoteProducts;
  LoadBuildableProducts;

  for var Product in FProducts do
    UpdateProductStatus(Product);

  FProducts.Sort(TComparer<TProductStatus>.Construct(
    function(const P1, P2: TProductStatus): Integer
    begin
      Result := CompareStr(P1.Id, P2.Id);
    end
  ));
end;

procedure TStatusManager.UpdateProductStatus(Product: TProductStatus);
begin
  if Product.Project = nil then exit;

  var Hasher := TFileHasher.Create(FConfig);
  try
    var Installer := TProjectInstaller.Create(FConfig);
    try
      for var IDEName := Low(TIDEName) to High(TIDEName) do
      begin
        for var Plat := Low(TPlatform) to High(TPlatform) do
        begin
          var PlatStatus := Product.IDEStatus(IDEName).PlatformStatus(Plat);
          var Hashes := Hasher.GetStoredHash(Product.Project, IDEName, Plat, '');
          PlatStatus.IsRegistered := Installer.HasPlatformUninstallInfo(Product.Id, IDEName, Plat) and (Hashes.RegistrationHash <> TSkipRegistering.All.ToInteger);
          PlatStatus.RegisteredItems := TSkipRegistering.FromInteger(Hashes.RegistrationHash).ToIncludedString;
          PlatStatus.IsBuilt := Hashes.HasAllHashes;
        end;
      end;
    finally
      Installer.Free;
    end;
  finally
    Hasher.Free;
  end;
end;

{ TProductStatus }

constructor TProductStatus.Create;
begin
  FIDEStatusMap := TObjectDictionary<TIDEName, TIDEStatus>.Create([doOwnsValues]);
end;

destructor TProductStatus.Destroy;
begin
  FProject.Free;
  FIDEStatusMap.Free;
  inherited;
end;

function TProductStatus.DisplayName: string;
begin
  var FullVersion := Version.Normalized;
  if (Channel <> '') and not SameText(Channel, 'production') then
    FullVersion := FullVersion + '-' + Channel;
  Result := Format('%s (%s)', [Id, FullVersion]);
end;

function TProductStatus.IDEStatus(const IDEName: TIDEName): TIDEStatus;
begin
  if not FIDEStatusMap.TryGetValue(IDEName, Result) then
  begin
    Result := TIDEStatus.Create;
    FIDEStatusMap.Add(IDEName, Result);
  end;
end;

procedure TProductStatus.SetProject(const Value: TProjectDefinition);
begin
  FreeAndNil(FProject);
  FProject := Value;
end;

{ TIDEStatus }

constructor TIDEStatus.Create;
begin
  FPlatformStatusMap := TObjectDictionary<TPlatform, TPlatformStatus>.Create([doOwnsValues]);
end;

destructor TIDEStatus.Destroy;
begin
  FPlatformStatusMap.Free;
  inherited;
end;

function TIDEStatus.PlatformStatus(const APlatform: TPlatform): TPlatformStatus;
begin
  if not FPlatformStatusMap.TryGetValue(APlatform, Result) then
  begin
    Result := TPlatformStatus.Create;
    FPlatformStatusMap.Add(APlatform, Result);
  end;
end;

end.

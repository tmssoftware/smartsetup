unit Status.Manager;

interface

uses
  System.Generics.Collections, System.Generics.Defaults, System.SysUtils, Deget.Version, UConfigFolders, UProjectDefinition,
  Deget.CoreTypes, UConfigDefinition;

type
  TPlatformStatus = class
  private
    FIsBuilt: Boolean;
    FIsRegistered: Boolean;
  public
    property IsBuilt: Boolean read FIsBuilt write FIsBuilt;
    property IsRegistered: Boolean read FIsRegistered write FIsRegistered;
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
    FVersion: TVersion;
    FChannel: string;
    FFetched: Boolean;
    FProject: TProjectDefinition;
    FIDEStatusMap: TDictionary<TIDEName, TIDEStatus>;
  public
    constructor Create;
    destructor Destroy; override;
    function DisplayName: string;
    function IDEStatus(const IDEName: TIDEName): TIDEStatus;
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property Version: TVersion read FVersion write FVersion;
    property Channel: string read FChannel write FChannel;
    property Fetched: Boolean read FFetched write FFetched;
    property Project: TProjectDefinition read FProject write FProject;
  end;

  TStatusManager = class
  private
    FConfig: TConfigDefinition;
    FProducts: TList<TProductStatus>;
    procedure LoadFetchedProducts;
    procedure LoadBuildableProducts;
    procedure UpdateProductStatus(Product: TProductStatus);
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
  Fetching.InfoFile, Fetching.InstallInfo, UProjectLoader, UProjectList, UFileHasher, UProjectInstaller;

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
      Product.Version := Proj.Application.Version;

      Product.Project := Projs.All.Extract(Proj);
    end;
  finally
    Projs.Free;
  end;
end;

procedure TStatusManager.LoadFetchedProducts;
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
      Product.Channel := Item.Channel;
      Product.Fetched := True;
    end;
  finally
    Items.Free;
  end;
end;

procedure TStatusManager.Update;
begin
  FProducts.Clear;
  LoadFetchedProducts;
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
  var Hasher := TFileHasher.Create(FConfig);
  try
    var Installer := TProjectInstaller.Create(FConfig);
    try
      for var IDEName := Low(TIDEName) to High(TIDEName) do
      begin
        for var Plat := Low(TPlatform) to High(TPlatform) do
        begin
          var PlatStatus := Product.IDEStatus(IDEName).PlatformStatus(Plat);
          PlatStatus.IsRegistered := Installer.HasPlatformUninstallInfo(Product.Id, IDEName, Plat);
          PlatStatus.IsBuilt := Hasher.GetStoredHash(Product.Project, IDEName, Plat, '').HasAllHashes;
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

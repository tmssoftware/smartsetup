unit VCS.Registry;
{$I ../../tmssetup.inc}

interface
uses Classes, SysUtils, Generics.Defaults, Generics.Collections, VCS.CoreTypes,
  UProjectDefinition, UConfigDefinition;
type
  //In the future, we might add more stuff to the registry, like an image or path files.
  //That data would go inside this record.
  TPredefinedData = record
  public
    Tmsbuild_Yaml: string;

    constructor Create(const aTmsbuild_Yaml: string);
    function Equals(const obj: TPredefinedData): boolean;
    class function Empty: TPredefinedData; static;
  end;

  TRegisteredProduct = class
  private
    const ProtocolToStr: Array[TVCSProtocol] of string = ('GIT', 'SVN', 'ZIPFILE');
  private
    FUrl: string;
    FProtocol: TVCSProtocol;
    FProductId: string;
    FName: string;
    FDescription: string;
    FServer: string;
    FIsPredefined: boolean;
    FPredefinedData: TPredefinedData;
    function GetProtocolString: string;
  public
    property Url: string read FUrl;
    property Protocol: TVCSProtocol read FProtocol;
    property ProtocolString: string read GetProtocolString;
    property ProductId: string read FProductId;
    property Name: string read FName;
    property Description: string read FDescription;
    property Server: string read FServer write FServer;
    property IsPredefined: boolean read FIsPredefined;
    property PredefinedData: TPredefinedData read FPredefinedData;

    class function GetProtocolFromString(const aProtocol: string; const DefaultIsGit: boolean): TVCSProtocol; static;
    class function GetStringFromProtocol(const aProtocol: TVCSProtocol): string; static;

    constructor Create(const aProductId: string; const aProtocol: TVCSProtocol; const aUrl, aName, aDescription, aServer: string; const aIsPredefined: boolean; const aPredefinedData: TPredefinedData);
    function Equals(ProductB: TObject): boolean; override;
  end;

  TProductRegistry = class
  private
    const RepoExt = '.repo.json';
  private
    BaseStoreFolder: string;
    FProducts: TObjectDictionary<string, TRegisteredProduct>;
    function AlreadySaved(const Filename: string; const Product: TRegisteredProduct; const Server: string): boolean;
    function GetProductFromFile(const Filename, Server: string): TRegisteredProduct;
    function GetProductID(const Filename: string): string;
    function GetProductFromJSON(const ProductId, Text: string;
      const Server: string; const aLoadingPredefined: boolean): TRegisteredProduct;
    function GetProductFromProject(const Project: TProjectDefinition; const Server: string; const aLoadingPredefined: boolean; const PredefinedText: string): TRegisteredProduct;
    procedure LoadPreregisteredProducts(const aServerName: string);
    procedure LoadOnePreregisteredProduct(const FileName, Text, Server: string);
    procedure LoadPreregisteredProductsFromServer(const Server: TServerConfig; const ServerName: string);
    procedure LoadLocalProductsFromServer(const Server: TServerConfig; const aServerName: string);
    function GetLocalStoreFolder(const Server: TServerConfig): string;
  public
    constructor Create(const aBaseStoreFolder, aServerName: string);
    destructor Destroy; override;

    procedure Add(const aProductId: string; const aProtocol: TVCSProtocol; const aUrl, aName, aDescription, aServer: string);
    function Remove(const aProductId: string): boolean;
    function GetProducts(const ProductIdMask: string; const List: TList<TRegisteredProduct>; const InstalledProducts: THashSet<string>): boolean;
    function Contains(const ProductId: string): boolean;
    class function LocalServer: TServerConfig; static;

    procedure Save;
    procedure Load(const aServerName: string);

  end;

implementation
uses IOUtils, UTmsBuildSystemUtils, Masks, Commands.GlobalConfig, JSON, USimpleJsonSerializer,
     System.Types, Zip, ZipFile.Download, UProjectLoader, UMultiLogger, VCS.Summary;

{ TRegisteredProduct }

constructor TRegisteredProduct.Create(const aProductId: string;
  const aProtocol: TVCSProtocol; const aUrl, aName, aDescription: string; const aServer: string; const aIsPredefined: boolean; const aPredefinedData: TPredefinedData);
begin
  FProductId := aProductId;
  FProtocol := aProtocol;
  FUrl := aUrl;
  FName := aName;
  FDescription := aDescription;
  FServer := aServer;
  FIsPredefined := aIsPredefined;
  FPredefinedData := aPredefinedData;
end;

function TRegisteredProduct.Equals(ProductB: TObject): boolean;
begin
  if not (ProductB is TRegisteredProduct) then exit(false);
  var b := TRegisteredProduct(ProductB);
  if b.FUrl <> FUrl then exit(false);
  if b.FProtocol <> FProtocol then exit(false);
  if b.FProductId <> FProductId then exit(false);
  if b.FName <> FName then exit(false);
  if b.FDescription <> FDescription then exit(false);
  if b.FIsPredefined <> FIsPredefined then exit(false);
  if b.Server <> FServer then exit(false);

  if not b.PredefinedData.Equals(FPredefinedData) then exit(false);


  exit(true);
end;

class function TRegisteredProduct.GetProtocolFromString(
  const aProtocol: string; const DefaultIsGit: boolean): TVCSProtocol;
begin
  if DefaultIsGit and (aProtocol.Trim = '') then exit (TVCSProtocol.Git);

  for var p := Low(TVCSProtocol) to High(TVCSProtocol) do
  begin
    if SameText(ProtocolToStr[p], aProtocol.Trim) then exit(p);
  end;

  raise Exception.Create('Undefined VCS engine: ' + aProtocol);
end;


function TRegisteredProduct.GetProtocolString: string;
begin
  Result := ProtocolToStr[Protocol];
end;

class function TRegisteredProduct.GetStringFromProtocol(
  const aProtocol: TVCSProtocol): string;
begin
  Result := ProtocolToStr[aProtocol];
end;

{ TProductRegistry }

function TProductRegistry.Contains(const ProductId: string): boolean;
begin
  Result := FProducts.ContainsKey(ProductId);
end;

constructor TProductRegistry.Create(const aBaseStoreFolder, aServerName: string);
begin
  BaseStoreFolder := aBaseStoreFolder;
  FProducts := TObjectDictionary<string, TRegisteredProduct>.Create([doOwnsValues], TIStringComparer.Ordinal);
  Load(aServerName);
end;

destructor TProductRegistry.Destroy;
begin
  FProducts.Free;
  inherited;
end;

procedure TProductRegistry.Add(const aProductId: string;
  const aProtocol: TVCSProtocol; const aUrl, aName, aDescription, aServer: string);
begin
  var Product: TRegisteredProduct := nil;
  if FProducts.TryGetValue(aProductId, Product) then
  begin
    if not Product.IsPredefined then raise Exception.Create('Product ' + aProductId + ' is already registered. Try unregistering it first.');
  end;
  FProducts.AddOrSetValue(aProductId, TRegisteredProduct.Create(aProductId, aProtocol, aUrl, aName, aDescription, aServer, false, TPredefinedData.Empty));
end;

function TProductRegistry.GetProducts(const ProductIdMask: string; const List: TList<TRegisteredProduct>; const InstalledProducts: THashSet<string>): boolean;
begin
  Result := false;
  for var ProductId in FProducts do
  begin
    if MatchesMask(ProductId.Key, ProductIdMask) then
    begin
      if (InstalledProducts = nil) or (InstalledProducts.Contains(ProductId.Key)) then
      begin
        List.Add(ProductId.Value);
        Result := true;
      end;
    end;
  end;
end;


function TProductRegistry.Remove(const aProductId: string): boolean;
begin
  Result := FProducts.ContainsKey(aProductId);
  if not Result then exit;
  FProducts.Remove(aProductId);
  Save;
end;

function TProductRegistry.GetProductID(const Filename: string): string;
begin
  Result := TPath.GetFileName(Filename);
  if not Result.EndsWith(RepoExt) then raise Exception.Create('Invalid filename: ' + Filename);
  Result := Result.Remove(Result.Length - RepoExt.Length);

end;

function TProductRegistry.GetProductFromJSON(const ProductId, Text: string; const Server: string; const aLoadingPredefined: boolean): TRegisteredProduct;
begin
  var Json := TJSONObject.ParseJSONValue(Text, false, true);
  try
  Result := TRegisteredProduct.Create(
                ProductId,
                TRegisteredProduct.GetProtocolFromString(Json.GetValue<string>('protocol'), false),
                Json.GetValue<string>('url'),
                Json.GetValue<string>('name'),
                Json.GetValue<string>('description'),
                Server,
                aLoadingPredefined,
                TPredefinedData.Empty);

  finally
    Json.Free;
  end;
end;

function TProductRegistry.GetProductFromProject(const Project: TProjectDefinition; const Server: string; const aLoadingPredefined: boolean; const PredefinedText: string): TRegisteredProduct;
begin
  Result := TRegisteredProduct.Create(
                Project.Application.Id,
                TRegisteredProduct.GetProtocolFromString(Project.Application.VCSProtocol, true),
                Project.Application.Url,
                Project.Application.Name,
                Project.Application.Description,
                Server,
                aLoadingPredefined,
                TPredefinedData.Create(PredefinedText) );
end;

function TProductRegistry.GetProductFromFile(const Filename, Server: string): TRegisteredProduct;
begin
  var Text := TFile.ReadAllText(Filename, TEncoding.UTF8);
  Result := GetProductFromJSON(GetProductId(Filename), Text, Server, false);
end;

procedure TProductRegistry.LoadOnePreregisteredProduct(const FileName, Text, Server: string);
begin
  var Project := TProjectDefinition.Create(FileName);
  try
    TProjectLoader.LoadDataIntoProject(FileName, Text, Project, 'root:supported frameworks', true);
    FProducts.AddOrSetValue(Project.Application.Id, GetProductFromProject(Project, Server, true, Text));
  finally
    Project.Free;
  end;

end;

procedure TProductRegistry.LoadPreregisteredProducts(const aServerName: string);
begin
  for var i := 0 to Config.ServerConfig.ServerCount - 1 do
  begin

    LoadPreregisteredProductsFromServer(Config.ServerConfig.GetServer(i), aServerName);
  end;
end;

procedure TProductRegistry.LoadPreregisteredProductsFromServer(const Server: TServerConfig; const ServerName: string);
const
  PredefinedZip = 'predefined.repos.zip';
begin
  if (Server.ServerType <> TServerType.ZipFile) or not Server.Enabled then exit;
  if (ServerName <> '') and (not SameText(ServerName, Server.Name)) then exit;

  try
    var PredefinedRepositories := TPath.Combine(Config.Folders.VCSMetaFolder, Server.Name + '.' + PredefinedZip);
    ZipDownloader.GetRepo(Server.Url, PredefinedRepositories, Server.Name, AddToVCSFetchLogSummary);

    var Zip := TZipFile.Create;
    try
      Zip.Open(PredefinedRepositories, TZipMode.zmRead);
      for var i := 0 to Zip.FileCount - 1 do
      begin
        if SameText(TPath.GetFileName(Zip.FileNames[i]), TProjectLoader.TMSBuildDefinitionFile) then
        begin
          var Filename := Zip.FileNames[i];
          var Bytes: TBytes;
          Zip.Read(i, Bytes);
          var Text := TEncoding.UTF8.GetString(Bytes);
          LoadOnePreregisteredProduct(FileName, Text, Server.Name);
        end;
      end;
    finally
      Zip.Free;
    end;
  except on ex: Exception do
  begin
    Logger.Error('Error loading server ' + Server.Name + ': "' + ex.Message + '". We can continue, but the preregistered GIT repositories might not be loaded.');
    //Do not re-raise. We can still work if this fails.
  end;
  end;
end;


procedure TProductRegistry.Load(const aServerName: string);
begin
  FProducts.Clear;
  LoadPreregisteredProducts(aServerName);
  for var i := 0 to Config.ServerConfig.ServerCount - 1 do
  begin
    LoadLocalProductsFromServer(Config.ServerConfig.GetServer(i), aServerName);
  end;
end;

function TProductRegistry.GetLocalStoreFolder(const Server: TServerConfig): string;
begin
  Result := BaseStoreFolder;
  if not Server.IsReservedName then
  begin
    if Server.Url.Trim = '' then Result := TPath.Combine(BaseStoreFolder, Server.Name)
    else Result := Server.Url;
  end;

end;

procedure TProductRegistry.LoadLocalProductsFromServer(const Server: TServerConfig; const aServerName: string);
begin
  if not Server.Enabled or (Server.ServerType <> TServerType.Local) then exit;
  if (aServerName <> '') and (not SameText(aServerName, Server.Name)) then exit;
  var StoreFolder := GetLocalStoreFolder(Server);
  if not TDirectory.Exists(StoreFolder) then exit;
  var Items := TDirectory.GetFiles(StoreFolder, '*' + RepoExt);
  for var Item in Items do
  begin
    var Data := GetProductFromFile(Item, Server.Name);
    FProducts.AddOrSetValue(Data.ProductId, Data);
  end;

end;

function TProductRegistry.AlreadySaved(const Filename: string; const Product: TRegisteredProduct; const Server: string): boolean;
begin
  if not TFile.Exists(Filename) then exit(false);

  var Data := GetProductFromFile(Filename, Server);
  try
    Result := Data.Equals(Product);
  finally
    Data.Free;
  end;

end;

class function TProductRegistry.LocalServer: TServerConfig;
begin
  var ServerCount := 0;
  for var i := 0 to Config.ServerConfig.ServerCount - 1 do
  begin
    var Server := Config.ServerConfig.GetServer(i);
    if Server.Enabled and (Server.ServerType = TServerType.Local) then
    begin
      inc(ServerCount);
      Result := Server;
    end;
  end;

  if ServerCount < 1 then raise Exception.Create('Error: There are no local sources where to register the repository. Make sure you have one source defined in tmsbuild.yaml.');
  if ServerCount > 1 then raise Exception.Create('Error: There are ' + IntToStr(ServerCount) + ' local sources defined in tmsbuild.yaml. To register a repo, you need a single one.');

end;

procedure TProductRegistry.Save;
begin
  var StoreFolder := GetLocalStoreFolder(LocalServer);

  TDirectory_CreateDirectory(StoreFolder);

  var ProductsInFolder := THashSet<string>.Create(TIStringComparer.Ordinal);
  try
    for var Product in FProducts.Values.ToArray do
    begin
      if Product.IsPredefined then continue;
      ProductsInFolder.Add(Product.ProductId + RepoExt);
    end;

    var Items := TDirectory.GetFiles(StoreFolder, '*' + RepoExt);
    for var Item in Items do
    begin
      if not ProductsInFolder.Contains(TPath.GetFileName(Item)) then
      begin
        DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, Item);
      end;
    end;

  finally
    ProductsInFolder.Free;
  end;

  for var Product in FProducts.Values.ToArray do
  begin
    if Product.IsPredefined then continue;
    var Filename := TPath.GetFullPath(TPath.Combine(StoreFolder, Product.ProductId + RepoExt));
    if AlreadySaved(Filename, Product, LocalServer.Name) then continue;

    var Item := TJSONObject.Create;
    try
      Item.AddPair('protocol', Product.ProtocolString);
      Item.AddPair('url', Product.Url);
      Item.AddPair('name', Product.Name);
      Item.AddPair('description', Product.Description);
      var Json := TSimpleJsonSerializer.Serialize(Item);
      TFile.WriteAllText(Filename, Json, TEncoding.UTF8);
    finally
      Item.Free;
    end;
  end;
end;

{ TPredefinedData }

constructor TPredefinedData.Create(const aTmsbuild_Yaml: string);
begin
  Tmsbuild_Yaml := aTmsbuild_Yaml;
end;

class function TPredefinedData.Empty: TPredefinedData;
begin
  Result.Tmsbuild_Yaml := '';
end;

function TPredefinedData.Equals(const obj: TPredefinedData): boolean;
begin
  Result := obj.Tmsbuild_Yaml = Tmsbuild_Yaml;
end;

end.

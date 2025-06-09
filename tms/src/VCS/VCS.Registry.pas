unit VCS.Registry;
{$I ../../tmssetup.inc}

interface
uses Classes, SysUtils, Generics.Defaults, Generics.Collections, VCS.CoreTypes;
type
  TRegisteredProduct = class
  private
    const ProtocolToStr: Array[TVCSProtocol] of string = ('GIT', 'SVN');
  private
    FUrl: string;
    FProtocol: TVCSProtocol;
    FProductId: string;
    FName: string;
    FDescription: string;
    FIsPredefined: boolean;
    function GetProtocolString: string;
  public
    property Url: string read FUrl;
    property Protocol: TVCSProtocol read FProtocol;
    property ProtocolString: string read GetProtocolString;
    property ProductId: string read FProductId;
    property Name: string read FName;
    property Description: string read FDescription;
    property IsPredefined: boolean read FIsPredefined;

    class function GetProtocolFromString(const aProtocol: string): TVCSProtocol; static;
    class function GetStringFromProtocol(const aProtocol: TVCSProtocol): string; static;

    constructor Create(const aProductId: string; const aProtocol: TVCSProtocol; const aUrl, aName, aDescription: string; const aIsPredefined: boolean);
    function Equals(ProductB: TObject): boolean; override;
  end;

  TProductRegistry = class
  private
    const RepoExt = '.repo.json';
  private
    StoreFolder: string;
    FProducts: TObjectDictionary<string, TRegisteredProduct>;
    function AlreadySaved(const Filename: string; const Product: TRegisteredProduct): boolean;
    function GetProductFromFile(const Filename: string): TRegisteredProduct;
    function GetProductID(const Filename: string): string;
    function GetProductFromText(const Filename, Text: string;
      const aLoadingPredefined: boolean): TRegisteredProduct;
    procedure LoadPreregisteredProducts;
  public
    constructor Create(const aStoreFolder: string);
    destructor Destroy; override;

    procedure Add(const aProductId: string; const aProtocol: TVCSProtocol; const aUrl, aName, aDescription: string);
    function Remove(const aProductId: string): boolean;
    function GetProducts(const ProductIdMask: string; const List: TList<TRegisteredProduct>; const InstalledProducts: THashSet<string>): boolean;
    function Contains(const ProductId: string): boolean;

    procedure Save;
    procedure Load;

  end;

implementation
uses IOUtils, UTmsBuildSystemUtils, Masks, Commands.GlobalConfig, JSON, USimpleJsonSerializer,
     System.Types, Zip;

{ TRegisteredProduct }

constructor TRegisteredProduct.Create(const aProductId: string;
  const aProtocol: TVCSProtocol; const aUrl, aName, aDescription: string; const aIsPredefined: boolean);
begin
  FProductId := aProductId;
  FProtocol := aProtocol;
  FUrl := aUrl;
  FName := aName;
  FDescription := aDescription;
  FIsPredefined := aIsPredefined;
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

  exit(true);
end;

class function TRegisteredProduct.GetProtocolFromString(
  const aProtocol: string): TVCSProtocol;
begin
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

constructor TProductRegistry.Create(const aStoreFolder: string);
begin
  StoreFolder := aStoreFolder;
  FProducts := TObjectDictionary<string, TRegisteredProduct>.Create([doOwnsValues], TIStringComparer.Ordinal);
  Load;
end;

destructor TProductRegistry.Destroy;
begin
  FProducts.Free;
  inherited;
end;

procedure TProductRegistry.Add(const aProductId: string;
  const aProtocol: TVCSProtocol; const aUrl, aName, aDescription: string);
begin
  var Product: TRegisteredProduct := nil;
  if FProducts.TryGetValue(aProductId, Product) then
  begin
    if not Product.IsPredefined then raise Exception.Create('Product ' + aProductId + ' is already registered. Try unregistering it first.');
  end;
  FProducts.AddOrSetValue(aProductId, TRegisteredProduct.Create(aProductId, aProtocol, aUrl, aName, aDescription, false));
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

function TProductRegistry.GetProductFromText(const Filename, Text: string; const aLoadingPredefined: boolean): TRegisteredProduct;
begin
  var Json := TJSONObject.ParseJSONValue(Text, false, true);
  try
  Result := TRegisteredProduct.Create(
                GetProductId(Filename),
                TRegisteredProduct.GetProtocolFromString(Json.GetValue<string>('protocol')),
                Json.GetValue<string>('url'),
                Json.GetValue<string>('name'),
                Json.GetValue<string>('description'),
                aLoadingPredefined);

  finally
    Json.Free;
  end;
end;

function TProductRegistry.GetProductFromFile(const Filename: string): TRegisteredProduct;
begin
  var Text := TFile.ReadAllText(Filename, TEncoding.UTF8);
  Result := GetProductFromText(Filename, Text, false);
end;

procedure TProductRegistry.LoadPreregisteredProducts;
begin
  var PredefinedRepositories := TResourceStream.Create(HInstance, 'PredefinedRepositories', RT_RCDATA);
  try
    var Zip := TZipFile.Create;
    try
      Zip.Open(PredefinedRepositories, TZipMode.zmRead);
      for var i := 0 to Zip.FileCount - 1 do
      begin
        if Zip.FileNames[i].EndsWith(RepoExt, true) then
        begin
          var Filename := Zip.FileNames[i];
          var Bytes: TBytes;
          Zip.Read(i, Bytes);
          var Text := TEncoding.UTF8.GetString(Bytes);
          FProducts.Add(GetProductId(Filename), GetProductFromText(Filename, Text, true));
        end;
      end;
    finally
      Zip.Free;
    end;

  finally
    PredefinedRepositories.Free;
  end;

end;


procedure TProductRegistry.Load;
begin
  FProducts.Clear;
  LoadPreregisteredProducts;
  if not TDirectory.Exists(StoreFolder) then exit;
  var Items := TDirectory.GetFiles(StoreFolder, '*' + RepoExt);
  for var Item in Items do
  begin
    var Data := GetProductFromFile(Item);
    FProducts.AddOrSetValue(Data.ProductId, Data);
  end;

end;

function TProductRegistry.AlreadySaved(const Filename: string; const Product: TRegisteredProduct): boolean;
begin
  if not TFile.Exists(Filename) then exit(false);

  var Data := GetProductFromFile(Filename);
  try
    Result := Data.Equals(Product);
  finally
    Data.Free;
  end;

end;

procedure TProductRegistry.Save;
begin
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
    if AlreadySaved(Filename, Product) then continue;

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

end.

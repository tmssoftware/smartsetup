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
    property PredefinedData: TPredefinedData read FPredefinedData;

    class function GetProtocolFromString(const aProtocol: string; const DefaultIsGit: boolean): TVCSProtocol; static;
    class function GetStringFromProtocol(const aProtocol: TVCSProtocol): string; static;

    constructor Create(const aProductId: string; const aProtocol: TVCSProtocol; const aUrl, aName, aDescription, aServer: string; const aPredefinedData: TPredefinedData);
    function Equals(ProductB: TObject): boolean; override;
  end;

  TProductRegistry = class
  private
    FProducts: TObjectDictionary<string, TRegisteredProduct>;
    function GetProductFromProject(const Project: TProjectDefinition; const Server: string; const PredefinedText: string): TRegisteredProduct;
    procedure LoadPreregisteredProducts(const aServerName: string);
    procedure LoadOnePreregisteredProduct(const FileName, Text, Server: string);
    procedure LoadPreregisteredProductsFromServer(const Server: TServerConfig; const ServerName: string);
  public
    constructor Create(aServerName: string);
    destructor Destroy; override;

    function GetProducts(const ProductIdMask: string; const List: TList<TRegisteredProduct>; const InstalledProducts: THashSet<string>): boolean;
    function Contains(const ProductId: string): boolean;

    procedure Load(const aServerName: string);

  end;

implementation
uses IOUtils, UTmsBuildSystemUtils, Masks, Commands.GlobalConfig, JSON, USimpleJsonSerializer,
     System.Types, Zip, ZipFile.Download, UProjectLoader, UMultiLogger, VCS.Summary;

{ TRegisteredProduct }

constructor TRegisteredProduct.Create(const aProductId: string;
  const aProtocol: TVCSProtocol; const aUrl, aName, aDescription: string; const aServer: string; const aPredefinedData: TPredefinedData);
begin
  FProductId := aProductId;
  FProtocol := aProtocol;
  FUrl := aUrl;
  FName := aName;
  FDescription := aDescription;
  FServer := aServer;
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

  raise Exception.Create('Undefined VCS protocol: ' + aProtocol);
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

constructor TProductRegistry.Create(aServerName: string);
begin
  FProducts := TObjectDictionary<string, TRegisteredProduct>.Create([doOwnsValues], TIStringComparer.Ordinal);
  Load(aServerName);
end;

destructor TProductRegistry.Destroy;
begin
  FProducts.Free;
  inherited;
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


function TProductRegistry.GetProductFromProject(const Project: TProjectDefinition; const Server: string; const PredefinedText: string): TRegisteredProduct;
begin
  Result := TRegisteredProduct.Create(
                Project.Application.Id,
                TRegisteredProduct.GetProtocolFromString(Project.Application.VCSProtocol, true),
                Project.Application.Url,
                Project.Application.Name,
                Project.Application.Description,
                Server,
                TPredefinedData.Create(PredefinedText) );
end;

procedure TProductRegistry.LoadOnePreregisteredProduct(const FileName, Text, Server: string);
begin
  var Project := TProjectDefinition.Create(FileName);
  try
    TProjectLoader.LoadDataIntoProject(FileName, Text, Project, 'root:supported frameworks', true);
    FProducts.AddOrSetValue(Project.Application.Id, GetProductFromProject(Project, Server, Text));
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
    raise Exception.Create('Error loading server ' + Server.Name + ': "' + ex.Message + '"');
  end;
  end;
end;


procedure TProductRegistry.Load(const aServerName: string);
begin
  FProducts.Clear;
  LoadPreregisteredProducts(aServerName);
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

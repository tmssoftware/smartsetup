unit Commands.ListRemote;

interface

uses
  System.Generics.Collections, System.SysUtils, UCommandLine;

procedure RegisterListRemoteCommand;

implementation
uses
  Commands.CommonOptions, Commands.Logging, Commands.GlobalConfig, URepositoryManager, Deget.Version, System.JSON,
  UJsonPrinter, UConfigDefinition, VCS.Registry, UProjectDefinition, Fetching.ProductVersion, UMultiLogger, URepositoryClasses;

var
  EnableLog: Boolean = False;
  RemoteServer: string;

type
  TAPIProduct = record
    Id: string;
    Name: string;
    Version: string;
    LicenseStatus: TLicenseStatus;
    VendorId: string;
    Server: string;

    Description: string;
    URL: string;
    DocsUrl: string;
    HomePage: string;
    VersionHistoryUrl: string;
    AvailableDefines: TArray<TAvailableDefine>;

    constructor Create(const aId: string;
        const aName: string;
        const aVersion: string;
        const aLicenseStatus: TLicenseStatus;
        const aVendorId: string;
        const aServer: string;
        const aDescription: string;
        const aURL: string;
        const aDocsUrl: string;
        const aHomePage: string;
        const aVersionHistoryUrl: string;
        const aAvailableDefines: TArray<TAvailableDefine>);
  end;

function DefinesToJson(const Defines: TArray<TAvailableDefine>): TJSONArray;
begin
  Result := TJSONArray.Create;
  for var Define in Defines do
  begin
    var Item := TJSONObject.Create;
    Result.AddElement(Item);
    Item.AddPair('name', Define.Name);
    Item.AddPair('description', Define.Description);
  end;
end;

procedure OutputAsJson(APIProducts: TList<TApiProduct>; const RepoProducts: TObjectList<TRegisteredVersionedProduct>);
const
  LicenseStatusStr: array[TLicenseStatus] of string = ('none', 'licensed');
begin
  var Root := TJSONObject.Create;
  try
    for var Product in APIProducts do
    begin
      var Item := TJSONObject.Create;
      Root.AddPair(Product.Id, Item);

      Item.AddPair('version', TLenientVersion.Create(Product.Version, TVersionType.Semantic).Normalized);
      Item.AddPair('version_type', TLenientVersion.IdSemantic);
      Item.AddPair('name', Product.Name);
      if Product.LicenseStatus <> TLicenseStatus.none then
         Item.AddPair('license_status', LicenseStatusStr[Product.LicenseStatus]);
      if Product.VendorId <> '' then
         Item.AddPair('vendor_id', Product.VendorId);
      Item.AddPair('server', Product.Server);
      Item.AddPair('description', Product.Description);
      Item.AddPair('url', Product.URL);
      Item.AddPair('docs', Product.DocsUrl);
      Item.AddPair('homepage', Product.HomePage);
      Item.AddPair('version_history_url', Product.VersionHistoryUrl);
      Item.AddPair('available_defines', DefinesToJson(Product.AvailableDefines));
    end;

    for var Product in RepoProducts do
    begin
      var Item := TJSONObject.Create;
      Root.AddPair(Product.Product.ProductId, Item);

      //In the future we can include the version from tmsbuild.yaml here. It is simple, but most likely it will be wrong as it won't be updated in the registry.
      Item.AddPair('version', '');
      Item.AddPair('version_type', TLenientVersion.IdFreeForm);
      Item.AddPair('name', Product.Product.Name);
      Item.AddPair('server', Product.Product.Server);
      Item.AddPair('description', Product.Product.Description);
      Item.AddPair('url', Product.Product.Url);
      Item.AddPair('docs', Product.Product.DocsUrl);
      Item.AddPair('homepage', Product.Product.HomePage);
      Item.AddPair('version_history_url', Product.Product.VersionHistoryUrl);
      Item.AddPair('available_defines', DefinesToJson(Product.Product.AvailableDefines));
    end;

    OutputJson(Root);
  finally
    Root.Free;
  end;
end;

procedure OutputAsText(APIProducts: TList<TAPIProduct>; const RepoProducts: TObjectList<TRegisteredVersionedProduct>);
begin
  for var Product in APIProducts do
    WriteLn(Format('%s (%s) -> %s', [Product.Id, TVersion(Product.Version).ToString, Product.Server]));
  for var Product in RepoProducts do
    WriteLn(Format('%s (%s) -> %s', [Product.Product.ProductId, '', Product.Product.Server]));
end;

function GetProductDefines(const List: TArray<TRepositoryAvailableDefine>): TArray<TAvailableDefine>;
begin
  Result := nil;
  SetLength(Result, Length(List));
  for var i := Low(List) to High(List) do Result[i] := TAvailableDefine.Create(List[i].Name, List[i].Description);    
end;

procedure AddApiProducts(const Server: TServerConfig; const ListedProducts: TList<TAPIProduct>);
begin
  var Repo := CreateRepositoryManager(Config.Folders.CredentialsFile(Server.Name), FetchOptions, Server.Url, Server.Name, Server.AllowInsecureConnections, true);
  try
      for var Product in Repo.Products do
        if (Product.LatestVersion <> nil) and not Product.Internal and not (Product.LicenseStatus in [TLicenseStatus.none]) then
        begin
          var APIProduct := TAPIProduct.Create(
              Product.Id,
              Product.Name,
              Product.LatestVersion.Version,
              Product.LicenseStatus,
              Product.VendorId,
              Server.Name,
              Product.LatestVersion.Metadata.Description,
              Product.LatestVersion.Metadata.Url,
              Product.LatestVersion.Metadata.DocsUrl,
              Product.LatestVersion.Metadata.HomePage,
              Product.LatestVersion.Metadata.VersionHistoryUrl,
              GetProductDefines(Product.LatestVersion.Metadata.AvailableDefines));

          ListedProducts.Add(APIProduct);

        end;
  finally
    Repo.Free;
  end;
end;

procedure AddZipFileProducts(const ListedProducts: TObjectList<TRegisteredVersionedProduct>);
begin
  RegisteredVCSRepos(RemoteServer).GetProducts(TProductVersion.Create('*', ''), ListedProducts, nil, nil);
end;

procedure RunListRemoteCommand;
begin
  InitFolderBasedCommand(EnableLog);

  var ListedAPIProducts := TList<TAPIProduct>.Create;
  try
    for var i := 0 to Config.ServerConfig.ServerCount - 1 do
    begin
      var Server := Config.ServerConfig.GetServer(i);
      if not Server.Enabled then continue;

      if (RemoteServer <> '') and (not SameText(RemoteServer, Server.Name)) then continue;

      case Server.ServerType of
        TServerType.Api: AddApiProducts(Server, ListedAPIProducts);
      end;

    end;

    var ListedZipFileProducts := TObjectList<TRegisteredVersionedProduct>.Create;
    try
      AddZipFileProducts(ListedZipFileProducts);

      if Logger.JsonMode then
        OutputAsJson(ListedAPIProducts, ListedZipFileProducts)
      else
        OutputAsText(ListedAPIProducts, ListedZipFileProducts);
    finally
      ListedZipFileProducts.Free;
    end;
  finally
    ListedAPIProducts.Free;
  end;
end;

procedure RegisterListRemoteCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('list-remote', '', 'list products in remote repository available to be installed',
    'More information: https://doc.tmssoftware.com/smartsetup/reference/tms-list-remote.html',
    'list-remote');

  RegisterRepoOption(cmd);

  var option := cmd.RegisterOption<Boolean>('log', '', 'enable logging for this command',
    procedure(const Value: Boolean)
    begin
      EnableLog := Value;
    end);
  option.HasValue := False;
  option.Hidden := True;

  RegisterJsonOption(cmd);

  option := cmd.RegisterOption<String>('server', '', 'if specified, we will only return the products from that server',
    procedure(const Value: String)
    begin
      RemoteServer := Value;
    end);
  option.HasValue := true;

  AddCommand(cmd.Name, CommandGroups.Status, RunListRemoteCommand);
end;

{ TAPIProduct }

constructor TAPIProduct.Create(const aId, aName, aVersion: string;
  const aLicenseStatus: TLicenseStatus; const aVendorId, aServer, aDescription, aURL, aDocsUrl,
  aHomePage, aVersionHistoryUrl: string; const aAvailableDefines: TArray<TAvailableDefine>);
begin
  Id := aId;
  Name := aName;
  Version := aVersion;
  LicenseStatus := aLicenseStatus;
  VendorId := aVendorId;
  Server := aServer;
  Description := aDescription;
  Url := aUrl;
  DocsUrl := aDocsUrl;
  HomePage := aHomePage;
  VersionHistoryUrl := aVersionHistoryUrl;
  AvailableDefines := aAvailableDefines;
end;

end.

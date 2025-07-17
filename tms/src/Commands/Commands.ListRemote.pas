unit Commands.ListRemote;

interface

uses
  System.Generics.Collections, System.SysUtils, UCommandLine;

procedure RegisterListRemoteCommand;

implementation
uses
  Commands.CommonOptions, Commands.Logging, Commands.GlobalConfig, URepositoryManager, Deget.Version, System.JSON,
  UJsonPrinter, UConfigDefinition, VCS.Registry;

var
  EnableLog: Boolean = False;
  UseJson: Boolean = False;
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

    constructor Create(const aId: string;
        const aName: string;
        const aVersion: string;
        const aLicenseStatus: TLicenseStatus;
        const aVendorId: string;
        const aServer: string;
        const aDescription: string;
        const aURL: string);
  end;

procedure OutputAsJson(APIProducts: TList<TApiProduct>; const RepoProducts: TList<TRegisteredProduct>);
const
  LicenseStatusStr: array[TLicenseStatus] of string = ('none', 'licensed');
begin
  var Root := TJSONObject.Create;
  try
    for var Product in APIProducts do
    begin
      var Item := TJSONObject.Create;
      Root.AddPair(Product.Id, Item);

      Item.AddPair('version', TVersion(Product.Version).Normalized);
      Item.AddPair('name', Product.Name);
      if Product.LicenseStatus <> TLicenseStatus.none then
         Item.AddPair('license_status', LicenseStatusStr[Product.LicenseStatus]);
      if Product.VendorId <> '' then
         Item.AddPair('vendor_id', Product.VendorId);
      Item.AddPair('server', Product.Server);
      Item.AddPair('description', Product.Description);
      Item.AddPair('url', Product.URL);
    end;

    for var Product in RepoProducts do
    begin
      if (RemoteServer <> '') and (not SameText(RemoteServer, Product.Server)) then continue;

      var Item := TJSONObject.Create;
      Root.AddPair(Product.ProductId, Item);

      //In the future we can include the version from tmsbuild.yaml here. It is simple, but most likely it will be wong as it won't be updated in the registry.
      Item.AddPair('version', '');
      Item.AddPair('name', Product.Name);
      Item.AddPair('server', Product.Server);
      Item.AddPair('description', Product.Description);
      Item.AddPair('url', Product.Url);
    end;

    OutputJson(Root);
  finally
    Root.Free;
  end;
end;

procedure OutputAsText(APIProducts: TList<TAPIProduct>; const RepoProducts: TList<TRegisteredProduct>);
begin
  for var Product in APIProducts do
    WriteLn(Format('%s (%s) -> %s', [Product.Id, TVersion(Product.Version).ToString, Product.Server]));
  for var Product in RepoProducts do
    WriteLn(Format('%s (%s) -> %s', [Product.ProductId, '', Product.Server]));
end;

procedure AddApiProducts(const Server: TServerConfig; const ListedProducts: TList<TAPIProduct>);
begin
  var Repo := CreateRepositoryManager(Config.Folders.CredentialsFile(Server.Name), FetchOptions, Server.Url, Server.Name, true);
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
              '',
              '');

          ListedProducts.Add(APIProduct);

        end;
  finally
    Repo.Free;
  end;
end;

procedure AddZipFileProducts(const ListedProducts: TList<TRegisteredProduct>);
begin
  RegisteredVCSRepos.GetProducts('*', ListedProducts, nil);
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

      case Server.Protocol of
        TServerProtocol.Api: AddApiProducts(Server, ListedAPIProducts);
      end;

    end;

    var ListedZipFileProducts := TList<TRegisteredProduct>.Create;
    try
      AddZipFileProducts(ListedZipFileProducts);

      if UseJson then
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
    '',
    'list-remote');

  RegisterRepoOption(cmd);

  var option := cmd.RegisterOption<Boolean>('log', '', 'enable logging for this command',
    procedure(const Value: Boolean)
    begin
      EnableLog := Value;
    end);
  option.HasValue := False;
  option.Hidden := True;

  option := cmd.RegisterOption<Boolean>('json', '', 'output data in JSON format',
    procedure(const Value: Boolean)
    begin
      UseJson := Value;
    end);
  option.HasValue := False;

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
  const aLicenseStatus: TLicenseStatus; const aVendorId, aServer, aDescription, aURL: string);
begin
  Id := aId;
  Name := aName;
  Version := aVersion;
  LicenseStatus := aLicenseStatus;
  VendorId := aVendorId;
  Server := aServer;
  Description := aDescription;
  Url := aUrl;
end;

end.

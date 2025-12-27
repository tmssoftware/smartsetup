unit URepositoryManager;

{$I ../../tmssetup.inc}

interface

uses
  System.Generics.Collections, System.SysUtils, Fetching.OfflineHTTPClient, Fetching.Options, UCredentials,
  System.JSON, System.JSON.Serializers, System.JSON.Readers, System.JSON.Converters, System.JSON.Types;

type
  TLicenseStatus = (none, licensed);

  TRepositoryVersion = class
  private
    [JsonName('version')]
    FVersion: string;
    [JsonName('release_date')]
    FReleaseDate: TDateTime;
    [JsonName('file_hash')]
    FFileHash: string;
  public
    property Version: string read FVersion write FVersion;
    property ReleaseDate: TDateTime read FReleaseDate write FReleaseDate;
    property FileHash: string read FFileHash write FFileHash;
  end;

  TRepositoryUserVersion = class(TRepositoryVersion)
  private
    [JsonName('license_status')]
    [JsonConverter(TJsonEnumNameConverter)]
    FLicenseStatus: TLicenseStatus;
  public
    property LicenseStatus: TLicenseStatus read FLicenseStatus write FLicenseStatus;
  end;

  TRepositoryProduct = class
  private
    [JsonName('id')]
    FId: string;
    [JsonName('name')]
    FName: string;
    [JsonName('latest_version')]
    FLatestVersion: TRepositoryVersion;
    [JsonName('license_status')]
    [JsonConverter(TJsonEnumNameConverter)]
    FLicenseStatus: TLicenseStatus;
    [JsonName('tms_id')]
    FVendorId: string;

    FInternal: Boolean;
    procedure SetLatestVersion(const Value: TRepositoryVersion);
  public
    destructor Destroy; override;
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property VendorId: string read FVendorId write FVendorId;
    property LatestVersion: TRepositoryVersion read FLatestVersion write SetLatestVersion;
    property LicenseStatus: TLicenseStatus read FLicenseStatus write FLicenseStatus;
    property Internal: Boolean read FInternal write FInternal;
  end;

  TDownloadInfo = record
    DownloadUrl: string;
    FileHash: string;
    FileName: string;
    RequiresToken: Boolean;
  end;

  TRepositoryManager = class
  const
  {$IFDEF MSWINDOWS}
    TMSSetupProductId = 'tms.smartsetup.windows';
  {$ENDIF}
  {$IFDEF LINUX}
    TMSSetupProductId = 'tms.smartsetup.linux';
  {$ENDIF}
  {$IFDEF MACOS}
    TMSSetupProductId = 'tms.smartsetup.macos';
  {$ENDIF}
  private
    FClient: TOfflineHTTPClient;
    FAccessToken: string;
    FUrl: string;
    FServer: string;
    FProducts: TObjectList<TRepositoryProduct>;
    FProductsLoaded: Boolean;
    function CreateRequest(const Method, Url: string): IHTTPRequest;
    function GetEndpoint(const Path: string): string;
    procedure CheckResponse(Req: IHTTPRequest; Resp: IHTTPResponse);
    procedure CheckProductsLoaded;
    function FindProductInList(const ProductId: string): TRepositoryProduct;
  public
    constructor Create;
    destructor Destroy; override;

    function Products: TEnumerable<TRepositoryProduct>;
    function GetDownloadInfo(const ProductId, Version: string): TDownloadInfo;
    function GetProductVersions(const ProductId: string; Versions: TList<TRepositoryUserVersion>): Boolean;
    property Url: string read FUrl write FUrl;
    property Server: string read FServer write FServer;
    property AccessToken: string read FAccessToken write FAccessToken;
  end;

function CreateRepositoryManager(const CredentialsFile: string; Options: TFetchOptions; const RootUrl, Server: string; const ThrowExceptions: boolean): TRepositoryManager;

implementation

uses
  UMultiLogger;

function CreateRepositoryManager(const CredentialsFile: string; Options: TFetchOptions; const RootUrl, Server: string; const ThrowExceptions: boolean): TRepositoryManager;
begin
  if not ThrowExceptions and (RootUrl = '') then exit(nil);

  Result := TRepositoryManager.Create;
  try
    Result.Url := Options.RepositoryInfo(RootUrl).ApiUrl;
    Result.Server := Server;
    Result.AccessToken := TCredentialsManager.GetAccessToken(CredentialsFile, Options, Options.RepositoryInfo(RootUrl).AuthUrl, Server);
  except
    Result.Free;
    if ThrowExceptions then raise else Result := nil;
  end;
end;

{ TRepositoryManager }

procedure TRepositoryManager.CheckProductsLoaded;
begin
  if FProductsLoaded then Exit;

  // perform request
  var Req := CreateRequest('GET', GetEndpoint('setup/user/products'));
  var Resp := FClient.Execute(Req);
  CheckResponse(Req, Resp);

  // Parse JSON into objects
  FProducts.Clear;
  var JObj := TJSONObject(TJSONObject.ParseJSONValue(Resp.ContentAsString));
  try
    var Serializer := TJsonSerializer.Create;
    try
      var Converter := TJsonListConverter<TRepositoryProduct>.Create;
      try
        Serializer.Converters.Add(Converter);
        var Reader := TJsonObjectReader.Create(JObj.Values['value']);
        try
          Serializer.Populate(Reader, FProducts);
        finally
          Reader.Free;
        end;
      finally
        Converter.Free;
      end;
    finally
      Serializer.Free;
    end;
  finally
    JObj.Free;
  end;

  FProductsLoaded := True;

  // Flat tms.smartsetup.* product as special. Hardcoded - do we want a "more dynamic, less hardcoded" way in future?
  var SmartSetup := FindProductInList(TMSSetupProductId);
  if SmartSetup <> nil then
    SmartSetup.Internal := True;

  // legacy product name, "tms.smartsetup", not used anymore but it's there in the repo
  SmartSetup := FindProductInList('tms.smartsetup');
  if SmartSetup <> nil then
    SmartSetup.Internal := True;
end;

procedure TRepositoryManager.CheckResponse(Req: IHTTPRequest; Resp: IHTTPResponse);
begin
  if (Resp.StatusCode >= 200) and (Resp.StatusCode < 300) then Exit;

  // Try to get error message from response
  var Error := '';
  if SameText(Resp.HeaderValue['Content-Type'], 'application/json') then
  begin
    // Parse XData error message
    var JsonObj: TJSONObject;
    var JsonError: TJSONObject;
    var Json := TJSONValue.ParseJSONValue(Resp.ContentAsString);
    try
      if Assigned(Json) and Json.TryGetValue(JsonObj) and JsonObj.TryGetValue('error', JsonError) then
        Error := JsonError.GetValue('message', '');
    finally
      Json.Free;
    end;
  end
  else
  if Resp.HeaderValue['Content-Type'].StartsWith('text/', True) then
    Error := Resp.ContentAsString;


  Logger.Info(Format('Request to %s failed with status code %d', [Req.URL.ToString, Resp.StatusCode]));
  Logger.Trace('Error response: ' + Error);

  // Try to get a better error message if it's empty
  if Error = '' then
    case Resp.StatusCode of
      401: Error := 'Credentials not provided. Use "tms credentials" to access the ' + Server + ' server, or disable it with "tms server-enable ' + Server + ' false"';
      403: Error := 'Credentials expired or forbidden';
    else
      Error := 'Request failed';
    end;
  raise Exception.Create(Error);
end;

constructor TRepositoryManager.Create;
begin
  inherited;
  FClient := TOfflineHTTPClient.Create;
  FProducts := TObjectList<TRepositoryProduct>.Create;
end;

function TRepositoryManager.CreateRequest(const Method, Url: string): IHTTPRequest;
begin
  Result := FClient.GetRequest(Method, Url);
  if AccessToken <> '' then
    Result.AddHeader('Authorization', 'Bearer ' + AccessToken);
end;

destructor TRepositoryManager.Destroy;
begin
  FClient.Free;
  FProducts.Free;
  inherited;
end;

function TRepositoryManager.FindProductInList(const ProductId: string): TRepositoryProduct;
begin
  for var Product in Products do
    if Product.Id = ProductId then
      Exit(Product);
  Result := nil;
end;

function TRepositoryManager.GetDownloadInfo(const ProductId, Version: string): TDownloadInfo;
begin
  var Req := CreateRequest('GET', GetEndpoint(Format('setup/products/%s/versions/%s/download/?channel=%s',
    [ProductId, Version, 'production'])));
  var Resp := FClient.Execute(Req);
  CheckResponse(Req, Resp);
  var JObj := TJSONObject(TJSONObject.ParseJSONValue(Resp.ContentAsString));
  try
    Result.DownloadUrl := JObj.GetValue<string>('download_url');
    Result.FileHash := JObj.GetValue<string>('file_hash');
    Result.RequiresToken := JObj.GetValue<Boolean>('requires_token', False);
    Result.FileName := JObj.GetValue<String>('file_name', '');
  finally
    JObj.Free;
  end;
end;

function TRepositoryManager.GetEndpoint(const Path: string): string;
begin
  Result := Url.TrimRight(['/']) + '/' + Path.TrimLeft(['/']);
end;

function TRepositoryManager.GetProductVersions(const ProductId: string; Versions: TList<TRepositoryUserVersion>): Boolean;
begin
  var Req := CreateRequest('GET', GetEndpoint(Format('setup/user/products/%s/versions?channel=%s',
    [ProductId, 'production'])));
  var Resp := FClient.Execute(Req);
  if Resp.StatusCode = 404 then Exit(False);
  CheckResponse(Req, Resp);

  // Parse JSON into objects
  var JObj := TJSONObject(TJSONObject.ParseJSONValue(Resp.ContentAsString));
  try
    var Serializer := TJsonSerializer.Create;
    try
      var Converter := TJsonListConverter<TRepositoryUserVersion>.Create;
      try
        Serializer.Converters.Add(Converter);
        var Reader := TJsonObjectReader.Create(JObj.Values['value']);
        try
          Serializer.Populate(Reader, Versions);
        finally
          Reader.Free;
        end;
      finally
        Converter.Free;
      end;
    finally
      Serializer.Free;
    end;
  finally
    JObj.Free;
  end;
  Result := True;
end;

function TRepositoryManager.Products: TEnumerable<TRepositoryProduct>;
begin
  CheckProductsLoaded;
  Result := FProducts;
end;

{ TRepositoryProduct }

destructor TRepositoryProduct.Destroy;
begin
  FLatestVersion.Free;
  inherited;
end;

procedure TRepositoryProduct.SetLatestVersion(const Value: TRepositoryVersion);
begin
  if FLatestVersion <> Value then
  begin
    FLatestVersion.Free;
    FLatestVersion := Value;
  end;
end;

end.

unit URepositoryManager;

{$I ../../tmssetup.inc}

interface

uses
  System.Generics.Collections, System.SysUtils, Fetching.OfflineHTTPClient, Fetching.Options, UCredentials,
  System.JSON.Types, System.JSON, System.JSON.Serializers, System.JSON.Readers, System.JSON.Converters,
  URepositoryClasses;

type
  TLicenseStatus = URepositoryClasses.TLicenseStatus;

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
    class function GetJSON(const Data: string): TJSONObject; static;
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

function CreateRepositoryManager(const CredentialsFile: string; Options: TFetchOptions; const RootUrl, Server: string; const AllowInsecureConnections: boolean; const ThrowExceptions: boolean): TRepositoryManager;

implementation

uses
  UMultiLogger, UConfigDefinition;

// Percent-encodes a single URL component (path segment or query value) as per RFC 3986.
// Everything except unreserved characters is encoded, including '/', '?', '&' and spaces.
function UrlEncode(const Value: string): string;
begin
  Result := '';
  for var B in TEncoding.UTF8.GetBytes(Value) do
  begin
    if CharInSet(Char(B), ['A'..'Z', 'a'..'z', '0'..'9', '-', '.', '_', '~']) then
      Result := Result + Char(B)
    else
      Result := Result + '%' + IntToHex(B, 2);
  end;
end;

function CreateRepositoryManager(const CredentialsFile: string; Options: TFetchOptions; const RootUrl, Server: string; const AllowInsecureConnections: boolean; const ThrowExceptions: boolean): TRepositoryManager;
begin
  if not ThrowExceptions and (RootUrl = '') then exit(nil);
  var ErrorMessage: string;
  if not TServerConfig.TryValidateUrlScheme(RootUrl, TServerType.Api, AllowInsecureConnections, Server, ErrorMessage) then
  begin
    if ThrowExceptions then raise Exception.Create(ErrorMessage);
    exit(nil);
  end;


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
  var JObj := GetJSON(Resp.ContentAsString);
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
    [UrlEncode(ProductId), UrlEncode(Version), UrlEncode('production')])));
  var Resp := FClient.Execute(Req);
  CheckResponse(Req, Resp);
  var JObj := GetJSON(Resp.ContentAsString);
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

class function TRepositoryManager.GetJSON(const Data: string): TJSONObject;
begin
  var Value := TJSONObject.ParseJSONValue(Data);
  if Value is TJSONObject then Exit(TJSONObject(Value));
  Value.Free;
  raise Exception.Create('Unexpected response from TMS API: The string "' + Data.Substring(0, 50) + '..." is not a JSON object.');
end;

function TRepositoryManager.GetProductVersions(const ProductId: string; Versions: TList<TRepositoryUserVersion>): Boolean;
begin
  var Req := CreateRequest('GET', GetEndpoint(Format('setup/user/products/%s/versions?channel=%s',
    [UrlEncode(ProductId), UrlEncode('production')])));
  var Resp := FClient.Execute(Req);
  if Resp.StatusCode = 404 then Exit(False);
  CheckResponse(Req, Resp);

  // Parse JSON into objects
  var JObj := GetJSON(Resp.ContentAsString);
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

end.

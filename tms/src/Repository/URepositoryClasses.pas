unit URepositoryClasses;

interface
uses Classes, SysUtils, System.JSON, System.JSON.Serializers, System.JSON.Readers, System.JSON.Writers,
     System.JSON.Converters, System.JSON.Types, UMultiLogger, IOUtils;
type
  TLicenseStatus = (none, licensed);

  TRepositoryAvailableDefine = record
  public
    [JsonName('name')]
    Name: string;
    [JsonName('description')]
    Description: string;

    constructor Create(const AName, ADescription: string);
  end;

  TRepositoryVersionMetaData = class
  private
    [JsonName('description')]
    FDescription: string;
    [JsonName('url')]
    FUrl: string;
    [JsonName('docs')]
    FDocsUrl: string;
    [JsonName('homepage')]
    FHomePage: string;
    [JsonName('version_history_url')]
    FVersionHistoryUrl: string;

    [JsonName('available_defines')]
    FAvailableDefines: TArray<TRepositoryAvailableDefine>;

  public
    constructor Create; overload;
    constructor Create(const ADescription, AUrl, ADocsUrl, AHomePage, AVersionHistoryUrl: string;
      const AAvailableDefines: TArray<TRepositoryAvailableDefine>); overload;
    property Description: string read FDescription;
    property Url: string read FUrl;
    property DocsUrl: string read FDocsUrl;
    property HomePage: string read FHomePage;
    property VersionHistoryUrl: string read FVersionHistoryUrl;
    property AvailableDefines: TArray<TRepositoryAvailableDefine> read FAvailableDefines;

    procedure Save(const FileName: string);
  end;


  TRepositoryVersion = class
  private
    [JsonName('version')]
    FVersion: string;
    [JsonName('release_date')]
    FReleaseDate: TDateTime;
    [JsonName('file_hash')]
    FFileHash: string;
    [JsonName('metadata')]
    FMetadataString: string;
  private
    [JsonIgnore]
    FMetadata: TRepositoryVersionMetadata;
    function GetMetadata: TRepositoryVersionMetadata;
  public
    property Version: string read FVersion write FVersion;
    property ReleaseDate: TDateTime read FReleaseDate write FReleaseDate;
    property FileHash: string read FFileHash write FFileHash;
    property Metadata: TRepositoryVersionMetadata read GetMetadata;
    destructor Destroy; override;
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


implementation

{ TRepositoryAvailableDefine }

constructor TRepositoryAvailableDefine.Create(const AName, ADescription: string);
begin
  Name := AName;
  Description := ADescription;
end;

{ TRepositoryVersionMetaData }

constructor TRepositoryVersionMetaData.Create;
begin
  inherited Create;
end;

constructor TRepositoryVersionMetaData.Create(const ADescription, AUrl, ADocsUrl, AHomePage, AVersionHistoryUrl: string;
  const AAvailableDefines: TArray<TRepositoryAvailableDefine>);
begin
  inherited Create;
  FDescription := ADescription;
  FUrl := AUrl;
  FDocsUrl := ADocsUrl;
  FHomePage := AHomePage;
  FVersionHistoryUrl := AVersionHistoryUrl;
  FAvailableDefines := AAvailableDefines;
end;

procedure TRepositoryVersionMetaData.Save(const FileName: string);
begin
  var StreamWriter := TStreamWriter.Create(FileName, false, TEncoding.ASCII);
  try
    var JsonWriter := TJsonTextWriter.Create(StreamWriter);
    try
      // Force all characters outside the ASCII range (> #127) to be escaped as \uXXXX
      JsonWriter.StringEscapeHandling := TJsonStringEscapeHandling.EscapeNonAscii;
      JsonWriter.Formatting := TJsonFormatting.None;

      var Serializer := TJsonSerializer.Create;
      try
        Serializer.Serialize(JsonWriter, Self);
      finally
        Serializer.Free;
      end;
    finally
      JsonWriter.Free;
    end;
  finally
    StreamWriter.Free;
  end;
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

{ TRepositoryVersion }

destructor TRepositoryVersion.Destroy;
begin
  FMetadata.Free;
  inherited;
end;

function TRepositoryVersion.GetMetadata: TRepositoryVersionMetadata;
begin
  if FMetadata <> nil then exit(FMetadata);
  if FMetadataString.Trim = '' then
  begin
    FMetadata := TRepositoryVersionMetaData.Create;
    exit(FMetadata);
  end;

  var Serializer := TJsonSerializer.Create;
  try
    try
      FMetadata := Serializer.Deserialize<TRepositoryVersionMetadata>(FMetadataString);
    except
      on ex: Exception do
      begin
        // Bad metadata in one product must not break listing all the others.
        Logger.Info('Invalid metadata for version ' + FVersion + ': ' + ex.Message);
        Logger.Trace('Metadata: ' + FMetadataString);
        FMetadata := nil;
      end;
    end;
  finally
    Serializer.Free;
  end;
  if FMetadata = nil then FMetadata := TRepositoryVersionMetaData.Create;
  Result := FMetadata;
end;


end.

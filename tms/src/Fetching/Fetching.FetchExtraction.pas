unit Fetching.FetchExtraction;

interface
uses Classes, SysUtils, Deget.Version;

type
  TOnFetchExtract = reference to procedure(const ProductId, BundleFileName, Server: string; const Version: TLenientVersion);
  TFetchExtraction = record
  private
    class var ExtractLock: TObject;
    class constructor Create;
    class destructor Destroy;
  private
    FOnExtract: TOnFetchExtract;
  public
    Skip: boolean;
    BundlesOnly: boolean;

    constructor Create(const aSkip, aBundlesOnly: boolean; aOnExtract: TOnFetchExtract);
    class function Extract: TFetchExtraction; static;

    function HasOnExtract: boolean;
    procedure DoOnExtract(const ProductId, BundleFileName, Server: string; const Version: TLenientVersion);
  end;

implementation

{ TFetchExtract }

class constructor TFetchExtraction.Create;
begin
  ExtractLock := TObject.Create;
end;

class destructor TFetchExtraction.Destroy;
begin
  ExtractLock.Free;
end;

procedure TFetchExtraction.DoOnExtract(const ProductId, BundleFileName,
  Server: string; const Version: TLenientVersion);
begin
  if not Assigned(FOnExtract) then exit;
  TMonitor.Enter(ExtractLock);
  try
    FOnExtract(ProductId, BundleFileName, Server, Version);
  finally
    TMonitor.Exit(ExtractLock);
  end;

end;

constructor TFetchExtraction.Create(const aSkip, aBundlesOnly: boolean;
  aOnExtract: TOnFetchExtract);
begin
  Skip := aSkip;
  BundlesOnly := aBundlesOnly;
  FOnExtract := aOnExtract;
end;

class function TFetchExtraction.Extract: TFetchExtraction;
begin
  Result.Skip := false;
  Result.BundlesOnly := false;
  Result.FOnExtract := nil;
end;

function TFetchExtraction.HasOnExtract: boolean;
begin
  Result := Assigned(FOnExtract);
end;

end.

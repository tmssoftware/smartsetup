unit UProductInfo;

interface

uses
  System.Generics.Collections, Deget.Version, Deget.CoreTypes;

type
  TProductPlatformInfo = class
  private
    FIsBuilt: Boolean;
    FIsRegistered: Boolean;
  public
    property IsBuilt: Boolean read FIsBuilt write FIsBuilt;
    property IsRegistered: Boolean read FIsRegistered write FIsRegistered;
  end;

  TProductIDEInfo = class
  private
    FPlatformInfoMap: TDictionary<TPlatform, TProductPlatformInfo>;
  public
    constructor Create;
    destructor Destroy; override;
    function PlatformInfo(const APlatform: TPlatform): TProductPlatformInfo;
  end;

  TProductInfo = class
  private
    FId: string;
    FName: string;
    FChannel: string;
    FLocal: Boolean;
    FIDEInfoMap: TDictionary<TIDEName, TProductIDEInfo>;
    FVersion: TLenientVersion;
    FVendorId: string;
    FServer: string;
    FPinned: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function IDEInfo(const IDEName: TIDEName): TProductIDEInfo;
    function HasIDEInfo: Boolean;
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property Version: TLenientVersion read FVersion write FVersion;
    property Channel: string read FChannel write FChannel;
    property Local: Boolean read FLocal write FLocal;
    property VendorId: string read FVendorId write FVendorId;
    property Server: string read FServer write FServer;
    property Pinned: Boolean read FPinned write FPinned;
  end;

  TProductInfoList = class(TObjectList<TProductInfo>)
  public
    function Find(const ProductId: string): TProductInfo;
  end;

implementation

{ TProductInfoList }

function TProductInfoList.Find(const ProductId: string): TProductInfo;
begin
  for var Product in Self do
    if Product.Id = ProductId then
      Exit(Product);
  Result := nil;
end;

{ TProductIDEInfo }

constructor TProductIDEInfo.Create;
begin
  FPlatformInfoMap := TObjectDictionary<TPlatform, TProductPlatformInfo>.Create([doOwnsValues]);
end;

destructor TProductIDEInfo.Destroy;
begin
  FPlatformInfoMap.Free;
  inherited;
end;

function TProductIDEInfo.PlatformInfo(const APlatform: TPlatform): TProductPlatformInfo;
begin
  if not FPlatformInfoMap.TryGetValue(APlatform, Result) then
  begin
    Result := TProductPlatformInfo.Create;
    FPlatformInfoMap.Add(APlatform, Result);
  end;
end;

{ TProductInfo }

constructor TProductInfo.Create;
begin
  FIDEInfoMap := TObjectDictionary<TIDEName, TProductIDEInfo>.Create([doOwnsValues]);
end;

destructor TProductInfo.Destroy;
begin
  FIDEInfoMap.Free;
  inherited;
end;

function TProductInfo.HasIDEInfo: Boolean;
begin
  Result := FIDEInfoMap.Count > 0;
end;

function TProductInfo.IDEInfo(const IDEName: TIDEName): TProductIDEInfo;
begin
  if not FIDEInfoMap.TryGetValue(IDEName, Result) then
  begin
    Result := TProductIDEInfo.Create;
    FIDEInfoMap.Add(IDEName, Result);
  end;
end;

end.

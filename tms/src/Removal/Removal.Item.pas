unit Removal.Item;

interface

uses
  System.Generics.Collections, System.SysUtils, URepositoryManager, Deget.Version;

type
  TRemovalStatus = (
    Flagged,       // product is succesfully flagged to be removed
    Failed         // cannot uninstall product (has dependencies)
  );
  TRemovalStatusSet = set of TRemovalStatus;

  TRemovalItem = class
  private
    FProductId: string;
    FProductPath: string;
    FVersion: TVersion;
    FDependenciesProcessed: Boolean;
    FDependents: TList<string>;
    FStatus: TRemovalStatus;
  public
    constructor Create(const AProductId, AProductPath, AVersion: string);
    destructor Destroy; override;
    property DependenciesProcessed: Boolean read FDependenciesProcessed write FDependenciesProcessed;
    property ProductId: string read FProductId;
    property ProductPath: string read FProductPath;
    property Version: TVersion read FVersion;
    property Dependents: TList<string> read FDependents;
    property Status: TRemovalStatus read FStatus write FStatus;
  end;

  TRemovalItems = class(TObjectList<TRemovalItem>)
  public
    function ContainsStatus(Status: TRemovalStatus): Boolean;
    function Contains(const ProductId, Version: string): Boolean;
    function Find(const ProductId, Version: string): TRemovalItem;
  end;

implementation

{ TRemovalItem }

constructor TRemovalItem.Create(const AProductId, AProductPath, AVersion: string);
begin
  FProductId := AProductId;
  FProductPath := AProductPath;
  FVersion := AVersion;
  FDependents := TList<string>.Create;
end;

destructor TRemovalItem.Destroy;
begin
  FDependents.Free;
  inherited;
end;

{ TRemovalItems }

function TRemovalItems.Contains(const ProductId, Version: string): Boolean;
begin
  Result := Find(ProductId, Version) <> nil;
end;

function TRemovalItems.ContainsStatus(Status: TRemovalStatus): Boolean;
begin
  for var Item in Self do
    if Item.Status = Status then
      Exit(True);
  Result := False;
end;

function TRemovalItems.Find(const ProductId, Version: string): TRemovalItem;
begin
  for var Item in Self do
    if (Item.ProductId = ProductId) and (Item.Version = TVersion(Version)) then
       Exit(Item);
  Result := nil;
end;

end.

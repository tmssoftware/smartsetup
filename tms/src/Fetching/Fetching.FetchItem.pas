unit Fetching.FetchItem;

interface

uses
  System.Generics.Collections, System.SysUtils, URepositoryManager, Deget.Version;

type
  TFetchStatus = (
    Unprocessed,  // just added to the list of products to be checked. Needs to check if must be updated or not
    Outdated,     // version was checked and product is outdated, a new version must be downloaded
    Downloaded,   // product has been downloaded and extacted. Final status.
    SkippedUpToDate,      // product is already updated to the latest version, no need to download. Final status.
    SkippedManualVersionExists, // skipped because there is a local non-managed version, which we never want to overwrite.
    Failed       // something failed during download. Final status.
  );
  TFetchStatusSet = set of TFetchStatus;

  TFetchItem = class
  private
    FProductId: string;
    FVersion: TVersion;
    FStatus: TFetchStatus;
    FDependenciesProcessed: Boolean;
    FSkipExtraction: Boolean;
    FInternal: Boolean;
    FFileHash: string;
  public
    constructor Create(const AProductId, AVersion: string);
    function UniqueName: string;
    property ProductId: string read FProductId;
    property Version: TVersion read FVersion;
    property FileHash: string read FFileHash write FFileHash;
    property Status: TFetchStatus read FStatus write FStatus;
    property SkipExtraction: Boolean read FSkipExtraction write FSkipExtraction;
    property Internal: Boolean read FInternal write FInternal;
    property DependenciesProcessed: Boolean read FDependenciesProcessed write FDependenciesProcessed;
  end;

  TFetchItems = class(TObjectList<TFetchItem>)
  public
    function ContainsStatus(Status: TFetchStatus): Boolean;
  end;

implementation

{ TFetchItem }

constructor TFetchItem.Create(const AProductId, AVersion: string);
begin
  FProductId := AProductId;
  FVersion := AVersion;
end;

function TFetchItem.UniqueName: string;
begin
//   Channel is hard-coded for now (production), as there is no channel information about latest version
  Result := Format('%s_%s_%s', [ProductId, 'production', Version.Normalized]);
end;

{ TFetchItems }

function TFetchItems.ContainsStatus(Status: TFetchStatus): Boolean;
begin
  for var Item in Self do
    if Item.Status = Status then
      Exit(True);
  Result := False;
end;

end.

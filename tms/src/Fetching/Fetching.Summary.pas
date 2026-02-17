unit Fetching.Summary;

{$I ../../tmssetup.inc}

interface

uses
  System.SysUtils, Fetching.FetchItem;

procedure LogFetchSummary(const Items: TFetchItems);

implementation

uses
  System.Generics.Collections, ULogger, UMultiLogger;

function LogItems(Items: TFetchItems; Status: TFetchStatusSet; const Suffix: string; LogInternal: Boolean): Boolean;
begin
  Result := False;
  for var Item in Items do
    if (Item.Status in Status) and (not Item.Internal or LogInternal) then
    begin
      Result := True;
      Logger.Info(Format('  - %s %s', [Item.ProductId, Suffix]));
    end;
end;

procedure LogFetchSummary(const Items: TFetchItems);
begin
  Logger.StartSection(TMessageType.Summary, 'Fetch Summary');
  Logger.Info('=== Fetch Summary ===');

  LogItems(Items, [TFetchStatus.SkippedUpToDate], '-> SKIPPED (Up to date)', False);
  LogItems(Items, [TFetchStatus.SkippedPinned], '-> SKIPPED (Pinned)', False);
  LogItems(Items, [TFetchStatus.SkippedManualVersionExists], '-> SKIPPED (A manually installed version exists)', False);
  LogItems(Items, [TFetchStatus.Downloaded], '-> UPDATED', True);
  LogItems(Items, [TFetchStatus.Failed], '-> FAILED', True);
  LogItems(Items, [TFetchStatus.Unprocessed, TFetchStatus.Outdated], '-> NOT PROCESSED', False);
  Logger.Info('');
  Logger.FinishSection(TMessageType.Summary);
end;

end.

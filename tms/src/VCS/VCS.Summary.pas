unit VCS.Summary;
{$I ../../tmssetup.inc}

interface

uses
  System.SysUtils, VCS.Registry, Generics.Collections;
type
  TVCSFetchStatus = (Ok, Error);
  TVCSFetchStatusSet = set of TVCSFetchStatus;

procedure LogVCSFetchSummary(const Products: TList<TRegisteredProduct>; const Status: TArray<TVCSFetchStatus>);

implementation

uses
  System.Generics.Collections, ULogger, UMultiLogger;

function LogItems(const Products: TList<TRegisteredProduct>; const Status: TArray<TVCSFetchStatus>; const TargetStatus: TVCSFetchStatusSet; const Suffix: string): Boolean;
begin
  Result := False;
  for var i := 0 to Products.Count - 1 do
    if (Status[i] in TargetStatus) then
    begin
      Result := True;
      Logger.Info(Format('  - %s %s', [Products[i].ProductId, Suffix]));
    end;
end;

procedure LogVCSFetchSummary(const Products: TList<TRegisteredProduct>; const Status: TArray<TVCSFetchStatus>);
begin
  Logger.StartSection(TMessageType.Summary, 'Repository Summary');
  Logger.Info('=== Repository Summary ===');

  LogItems(Products, Status, [TVCSFetchStatus.Ok], '-> OK');
  LogItems(Products, Status, [TVCSFetchStatus.Error], '-> FAILED');
  Logger.Info('');
  Logger.FinishSection(TMessageType.Summary);
end;

end.

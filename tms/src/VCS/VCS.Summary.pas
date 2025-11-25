unit VCS.Summary;
{$I ../../tmssetup.inc}

interface

uses
  System.SysUtils, VCS.Registry, Generics.Collections, ULogger;
type
  TVCSFetchStatus = (Ok, Error);
  TVCSFetchStatusSet = set of TVCSFetchStatus;

procedure LogVCSFetchSummary(const Products: TList<TRegisteredVersionedProduct>; const Status: TArray<TVCSFetchStatus>);
procedure AddToVCSFetchLogSummary(const Verbosity: TVerbosity; const msg: string);

implementation

uses
  System.Generics.Collections, UMultiLogger;

type
  TExtraMessage = record
    Message: string;
    Verbosity: TVerbosity;
  end;

var
  ExtraMessages: TArray<TExtraMessage> = nil;

procedure AddToVCSFetchLogSummary(const Verbosity: TVerbosity; const msg: string);
begin
  var NewMsg: TExtraMessage;
  NewMsg.Message := msg;
  NewMsg.Verbosity := Verbosity;
  ExtraMessages := ExtraMessages + [NewMsg];
end;

function LogItems(const Products: TList<TRegisteredVersionedProduct>; const Status: TArray<TVCSFetchStatus>; const TargetStatus: TVCSFetchStatusSet; const Suffix: string): Boolean;
begin
  Result := False;
  for var i := 0 to Products.Count - 1 do
    if (Status[i] in TargetStatus) then
    begin
      Result := True;
      Logger.Info(Format('  - %s %s', [Products[i].ProductIdAndVersion, Suffix]));
    end;
end;

procedure LogVCSFetchSummary(const Products: TList<TRegisteredVersionedProduct>; const Status: TArray<TVCSFetchStatus>);
begin
  Logger.StartSection(TMessageType.Summary, 'Repository Summary');
  Logger.Info('=== Repository Summary ===');

  LogItems(Products, Status, [TVCSFetchStatus.Ok], '-> OK');
  LogItems(Products, Status, [TVCSFetchStatus.Error], '-> FAILED');
  Logger.Info('');

  for var msg in ExtraMessages do
  begin
    Logger.Write(msg.Verbosity, msg.Message);
  end;
  if ExtraMessages <> nil then Logger.Info('');


  Logger.FinishSection(TMessageType.Summary);
end;

end.

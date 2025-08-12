unit UPersistence;
{$i ../../tmssetup.inc}

interface
uses UCoreTypes;
type

TStrUninstallInfo = record
  private
    FId: string;
    FData: string;
public
  property Id: string read FId;
  property Data: string read FData;

  constructor Create(const aId, aData: string);
end;

TPersistence = class
  public
  procedure Store(const Data: string; const Project: string; const IDE: string = ''; const Platform: string = ''; const Package: string = ''); virtual; abstract;
  function Retrieve(const Project: string; const IDE: string = ''; const Platform: string = ''; const Package: string = ''): string; virtual; abstract;
  procedure Remove(const Project: string; const IDE: string = ''; const Platform: string = ''; const Package: string = ''); virtual; abstract;
  procedure RemoveAndBelow(const Project: string; const IDE: string = ''; const Platform: string = ''); virtual; abstract;
  procedure RemoveAll(const ForceExcluded: boolean; out AllIncluded: boolean); virtual; abstract;

  function List(const Project: string; const IDE: string = ''; const Platform: string = ''): TArray<TStrUninstallInfo>; virtual; abstract;
end;

implementation
{ TStrUninstallInfo }

constructor TStrUninstallInfo.Create(const aId, aData: string);
begin
  FId := aId;
  FData := aData;
end;

end.

unit UConfigInfo;

interface

uses
  System.Generics.Collections, System.SysUtils;

type
  TServerConfigItem = class
    Name: string;
    Protocol: string;
    Url: string;
    Enabled: Boolean;
    function IsReserved: Boolean;
  end;

  TServerConfigItems = class(TObjectList<TServerConfigItem>)
  public
    function Find(const Name: string): TServerConfigItem;
  end;

  TConfigInfo = class
  private
    FServers: TServerConfigItems;
  public
    constructor Create;
    destructor Destroy; override;
    property Servers: TServerConfigItems read FServers;
  end;

implementation

{ TConfigInfo }

constructor TConfigInfo.Create;
begin
  FServers := TServerConfigItems.Create;
end;

destructor TConfigInfo.Destroy;
begin
  FServers.Free;
  inherited;
end;

{ TServerConfigItems }

function TServerConfigItems.Find(const Name: string): TServerConfigItem;
begin
  for var Item in Self do
    if SameText(Item.Name, Name) then
      Exit(Item);
  Result := nil;
end;

{ TServerConfigItem }

function TServerConfigItem.IsReserved: Boolean;
begin
  var TestName := Name.ToLower;
  Result := (TestName = 'tms') or (TestName = 'local') or (TestName = 'community');
end;

end.

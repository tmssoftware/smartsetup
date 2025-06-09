unit USkippedPlatforms;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes, Generics.Collections;
type
  TSkippedPlatforms = class
  private
    Platforms: TDictionary<integer, boolean>;
    IDEs: TDictionary<TIDEName, boolean>;
    function GetHash(const IDE: TIDEName; const Platform: TPlatform): integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const IDE: TIDEName; const Platform: TPlatform);
    function Empty: boolean;
    function ContainsIDE(const IDE: TIDEName): boolean;
    function ContainsPlatform(const IDE: TIDEName; const Platform: TPlatform): boolean;
  end;


implementation

{ TSkippedPlatforms }

procedure TSkippedPlatforms.Add(const IDE: TIDEName; const Platform: TPlatform);
begin
  IDEs.AddOrSetValue(IDE, true);
  Platforms.AddOrSetValue(GetHash(IDE, Platform), true);
end;

function TSkippedPlatforms.ContainsIDE(const IDE: TIDEName): boolean;
begin
  Result := IDEs.ContainsKey(IDE);
end;

function TSkippedPlatforms.ContainsPlatform(const IDE: TIDEName;
  const Platform: TPlatform): boolean;
begin
  Result := Platforms.ContainsKey(GetHash(IDE, Platform));
end;

constructor TSkippedPlatforms.Create;
begin
  Platforms := TDictionary<integer, boolean>.Create;
  IDEs := TDictionary<TIDEName, boolean>.Create
end;

destructor TSkippedPlatforms.Destroy;
begin
  Platforms.Free;
  IDEs.Free;
  inherited;
end;

function TSkippedPlatforms.Empty: boolean;
begin
  Result := Platforms.Count = 0;
end;

function TSkippedPlatforms.GetHash(const IDE: TIDEName;
  const Platform: TPlatform): integer;
begin
  Result := (Integer(IDE) shl 4) or Integer(Platform);
end;

end.

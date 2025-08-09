unit Megafolders.Definition;
{$i ../../tmssetup.inc}

interface
uses SysUtils, Classes, Generics.Defaults, Generics.Collections, Masks;
type
TUsedMegafolders = class
  private
    FItemLock: TObject;
    FItems: THashSet<string>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const s: string);
    function UnsafeContains(const s: string): boolean;
end;

TMegafolder = class
  private
    FFolder: string;
    FMask: string;
  public
    constructor Create(const aFolder, aMask: string);
    property Folder: string read FFolder;
    property Mask: string read FMask;
    function IsNone: boolean;
    function Equals(o: TObject): boolean; override;
end;

TMegafolderList = class
  private
    FList: TObjectList<TMegafolder>;
    function GetItems(const i: integer): TMegafolder;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Count: integer;
    property Items[const i: integer]: TMegafolder read GetItems; default;

    procedure Add(const Item: TMegafolder);
    function GetEnumerator: TEnumerator<TMegafolder>;

    //returns empty if no match
    function Match(const Id: string): string;

    function Equals(o: TObject): boolean; override;

end;


implementation
uses IOUtils;

{ TMegafolderList }

procedure TMegafolderList.Add(const Item: TMegafolder);
begin
  FList.Add(Item);
end;

procedure TMegafolderList.Clear;
begin
  FList.Clear;
end;

function TMegafolderList.Count: integer;
begin
  Result := FList.Count;
end;

constructor TMegafolderList.Create;
begin
  FList := TObjectList<TMegafolder>.Create;
end;

destructor TMegafolderList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TMegafolderList.Equals(o: TObject): boolean;
begin
  if not (o is TMegafolderList) then exit(false);
  var om := TMegafolderList(o);
  if om.Count <> Count then exit(false);
  for var i := 0 to Count - 1 do
  begin
    if not FList[i].Equals(om.FList[i]) then exit(false);
  end;
  Result := true;
end;

function TMegafolderList.GetEnumerator: TEnumerator<TMegafolder>;
begin
  Result := FList.GetEnumerator;
end;

function TMegafolderList.GetItems(const i: integer): TMegafolder;
begin
  Result := FList[i];
end;

function TMegafolderList.Match(const Id: string): string;
begin
  Result := '';
  for var Item in FList do
  begin
    if MatchesMask(Id, Item.Mask) then
    begin
      if Item.IsNone then exit('');      
      exit(Item.Folder);
    end;
  end;

end;

{ TMegafolder }

constructor TMegafolder.Create(const aFolder, aMask: string);
begin
  if not TPath.HasValidFileNameChars(aFolder, false) then raise Exception.Create('The Megafolder name "' + aFolder + '" has invalid characters. It must be a name that can be used in a filesystem folder.');
  if Length(aFolder) > 32 then raise Exception.Create('The Megafolder name "' + aFolder + '" is too long. It must be 32 characters or less.');

  FFolder := aFolder;
  FMask := aMask;
end;

function TMegafolder.Equals(o: TObject): boolean;
begin
  if not (o is TMegafolder)  then exit(false);
  var om := TMegafolder(o);
  Result := (om.Folder = Folder) and (om.Mask = Mask);

end;

function TMegafolder.IsNone: boolean;
begin
  Result := SameText(Folder, 'none')
end;

{ TUsedMegafolders }

function TUsedMegafolders.UnsafeContains(const s: string): boolean;
begin
  //No lock here, this is called from thread-safe places
  Result := FItems.Contains(s);
end;

constructor TUsedMegafolders.Create;
begin
  inherited;
  FItemLock := TObject.Create;
  FItems := THashSet<string>.Create(TIStringComparer.Ordinal);
end;

destructor TUsedMegafolders.Destroy;
begin
  FItems.Free;
  FItemLock.Free;

  inherited;
end;

procedure TUsedMegafolders.Add(const s: string);
begin
  TMonitor.Enter(FItemLock);
  try
    FItems.Add(s);
  finally
    TMonitor.Exit(FItemLock);
  end;

end;


end.

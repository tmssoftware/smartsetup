unit UConfigModel;

// The contents of tms.config.yaml, as `tms config-read -json -config-source:main` returns them.
// We keep the whole JSON, not only what the settings form shows: `tms config-write -clear` wipes the file,
// so when saving we must send back every value, including the ones the form doesn't edit.

interface
{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections, UCommonTypes;

type
  // A value that might not be in the file. NotSet means the key is removed, so tms uses the default, or for
  // a product, what is set in "all products".
  TOptionalBool = (NotSet, No, Yes);

  // The order of both enums matches the items in the settings form combos.
  TSkipRegisterMode = (NotSet, No, Yes, Custom);

  TDefineItem = record
    Name: string;
    Enabled: Boolean; // false is written as "NAME: false", which removes a define set in "all products".
  end;

  // A "configuration for <id>" section. It edits the JSON object owned by TTmsConfig.
  TProductConfig = class
  private
    FId: string;
    FJson: TJSONObject;
    function GetIsAllProducts: Boolean;
    function GetDebugDcus: TOptionalBool;
    procedure SetDebugDcus(const Value: TOptionalBool);
    function GetIdList(const Key: string; out Ids: TArray<string>): Boolean;
    procedure SetIdList(const Key: string; const IsSet: Boolean; const Ids: TArray<string>);
  public
    constructor Create(const AId: string; AJson: TJSONObject);

    procedure GetSkipRegister(out Mode: TSkipRegisterMode; out Parts: TArray<string>);
    // Parts are only used for Custom. They are names from SkipRegisterParts.
    procedure SetSkipRegister(const Mode: TSkipRegisterMode; const Parts: TArray<string>);

    // Return false if the list is not set: then all versions/platforms are used, or for a product, the ones in
    // "all products". A list set for a product replaces the list in "all products".
    function GetDelphiVersions(out Ids: TArray<string>): Boolean;
    procedure SetDelphiVersions(const IsSet: Boolean; const Ids: TArray<string>);
    function GetPlatforms(out Ids: TArray<string>): Boolean;
    procedure SetPlatforms(const IsSet: Boolean; const Ids: TArray<string>);

    function GetDefines: TArray<TDefineItem>;
    procedure SetDefines(const Defines: TArray<TDefineItem>);

    property Id: string read FId;
    property IsAllProducts: Boolean read GetIsAllProducts;
    property DebugDcus: TOptionalBool read GetDebugDcus write SetDebugDcus;
  end;

  TTmsConfig = class
  private
    FRoot: TJSONObject;
    FProducts: TObjectList<TProductConfig>;
    function Options: TJSONObject;
    function GetAlternateRegistryKey: string;
    procedure SetAlternateRegistryKey(const Value: string);
    function GetProductCount: Integer;
    function GetProduct(Index: Integer): TProductConfig;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromJson(const S: string);

    // The -p parameters for `tms config-write -clear`, with the same syntax `tms config-read -cmd` uses.
    function ToParams: TArray<string>;

    procedure GetServers(Items: TServerConfigItems);
    procedure SetServers(Items: TServerConfigItems);

    // "all products" is always the first one.
    property ProductCount: Integer read GetProductCount;
    property Products[Index: Integer]: TProductConfig read GetProduct;
    function FindProduct(const Id: string): TProductConfig;
    function AddProduct(const Id: string): TProductConfig;
    procedure RemoveProduct(const Id: string);

    property AlternateRegistryKey: string read GetAlternateRegistryKey write SetAlternateRegistryKey;
  end;

const
  AllProductsId = 'all products';
  SkipRegisterParts: array[0..6] of string = ('Packages', 'StartMenu', 'Help', 'WindowsPath', 'WebCore', 'Registry', 'FileLinks');

implementation

uses
  BBStrings;

const
  OptionsKey = 'tms smart setup options';
  ProductPrefix = 'configuration for ';

{ JSON helpers }

// The child object Name. If it doesn't exist, it is created when Create is true, or nil is returned.
function ChildObject(const Parent: TJSONObject; const Name: string; const Create: Boolean): TJSONObject;
begin
  var Value := Parent.GetValue(Name);
  if Value is TJSONObject then Exit(TJSONObject(Value));
  if not Create then Exit(nil);

  Result := TJSONObject.Create;
  var Pair := Parent.Get(Name);
  if Pair <> nil then Pair.JsonValue := Result else Parent.AddPair(Name, Result);
end;

// Sets Name to Value, keeping its position if it already exists. A nil Value removes it.
procedure SetJsonValue(const Parent: TJSONObject; const Name: string; const Value: TJSONValue);
begin
  var Pair := Parent.Get(Name);
  if Value = nil then
  begin
    if Pair <> nil then Parent.RemovePair(Name).Free;
    Exit;
  end;

  if Pair <> nil then Pair.JsonValue := Value else Parent.AddPair(Name, Value);
end;

// Removes Name from Parent, and Parent from GrandParent if it is now empty, so we don't write empty sections.
procedure RemoveValue(const GrandParent: TJSONObject; const ParentName, Name: string);
begin
  var Parent := ChildObject(GrandParent, ParentName, False);
  if Parent = nil then Exit;
  SetJsonValue(Parent, Name, nil);
  if Parent.Count = 0 then SetJsonValue(GrandParent, ParentName, nil);
end;

function BoolValue(const Value: Boolean): TJSONValue;
begin
  if Value then Result := TJSONTrue.Create else Result := TJSONFalse.Create;
end;

{ TProductConfig }

constructor TProductConfig.Create(const AId: string; AJson: TJSONObject);
begin
  inherited Create;
  FId := AId;
  FJson := AJson;
end;

function TProductConfig.GetIsAllProducts: Boolean;
begin
  Result := SameText(FId, AllProductsId);
end;

procedure TProductConfig.GetSkipRegister(out Mode: TSkipRegisterMode; out Parts: TArray<string>);
begin
  Parts := nil;
  Mode := TSkipRegisterMode.NotSet;
  var Options := ChildObject(FJson, 'options', False);
  if Options = nil then Exit;

  var Value := Options.GetValue('skip register');
  if Value is TJSONBool then
  begin
    if TJSONBool(Value).AsBoolean then Mode := TSkipRegisterMode.Yes else Mode := TSkipRegisterMode.No;
    Exit;
  end;

  if not (Value is TJSONArray) then Exit;

  // Items can be "all" and negatives like "-help", so we work out which parts end up skipped.
  var Skipped: TArray<Boolean>;
  SetLength(Skipped, Length(SkipRegisterParts)); // all false
  for var Item in TJSONArray(Value) do
  begin
    var Name := Item.Value.Trim;
    var Remove := Name.StartsWith('-');
    if Remove then Name := Name.Substring(1).Trim;
    for var i := 0 to High(Skipped) do
      if SameText(Name, 'all') or SameText(Name, SkipRegisterParts[i]) then Skipped[i] := not Remove;
  end;

  for var i := 0 to High(Skipped) do
    if Skipped[i] then Parts := Parts + [SkipRegisterParts[i]];

  if Length(Parts) = 0 then Mode := TSkipRegisterMode.No
  else if Length(Parts) = Length(SkipRegisterParts) then Mode := TSkipRegisterMode.Yes
  else Mode := TSkipRegisterMode.Custom;
end;

procedure TProductConfig.SetSkipRegister(const Mode: TSkipRegisterMode; const Parts: TArray<string>);
begin
  case Mode of
    TSkipRegisterMode.NotSet: RemoveValue(FJson, 'options', 'skip register');
    TSkipRegisterMode.No: SetJsonValue(ChildObject(FJson, 'options', True), 'skip register', BoolValue(False));
    TSkipRegisterMode.Yes: SetJsonValue(ChildObject(FJson, 'options', True), 'skip register', BoolValue(True));
    TSkipRegisterMode.Custom:
    begin
      var Arr := TJSONArray.Create;
      for var Part in Parts do Arr.Add(Part.ToLowerInvariant);
      SetJsonValue(ChildObject(FJson, 'options', True), 'skip register', Arr);
    end;
  end;
end;

function TProductConfig.GetIdList(const Key: string; out Ids: TArray<string>): Boolean;
begin
  Ids := nil;
  var Value := FJson.GetValue(Key);
  if not (Value is TJSONArray) then Exit(False);

  for var Item in TJSONArray(Value) do
  begin
    if Item is TJSONObject then
    begin
      // "delphi7: false" means not included.
      for var Pair in TJSONObject(Item) do
        if not (Pair.JsonValue is TJSONFalse) then Ids := Ids + [Pair.JsonString.Value];
    end
    else
      Ids := Ids + [Item.Value];
  end;
  Result := Ids <> nil;
end;

procedure TProductConfig.SetIdList(const Key: string; const IsSet: Boolean; const Ids: TArray<string>);
begin
  if not IsSet then
  begin
    SetJsonValue(FJson, Key, nil);
    Exit;
  end;

  var Arr := TJSONArray.Create;
  for var Id in Ids do Arr.Add(Id);
  SetJsonValue(FJson, Key, Arr);
end;

function TProductConfig.GetDelphiVersions(out Ids: TArray<string>): Boolean;
begin
  Result := GetIdList('delphi versions', Ids);
end;

procedure TProductConfig.SetDelphiVersions(const IsSet: Boolean; const Ids: TArray<string>);
begin
  SetIdList('delphi versions', IsSet, Ids);
end;

function TProductConfig.GetPlatforms(out Ids: TArray<string>): Boolean;
begin
  Result := GetIdList('platforms', Ids);
end;

procedure TProductConfig.SetPlatforms(const IsSet: Boolean; const Ids: TArray<string>);
begin
  SetIdList('platforms', IsSet, Ids);
end;

function TProductConfig.GetDebugDcus: TOptionalBool;
begin
  var Compilation := ChildObject(FJson, 'compilation options', False);
  if Compilation = nil then Exit(TOptionalBool.NotSet);
  var Value := Compilation.GetValue('debug dcus');
  if not (Value is TJSONBool) then Exit(TOptionalBool.NotSet);
  if TJSONBool(Value).AsBoolean then Result := TOptionalBool.Yes else Result := TOptionalBool.No;
end;

procedure TProductConfig.SetDebugDcus(const Value: TOptionalBool);
begin
  if Value = TOptionalBool.NotSet then
    RemoveValue(FJson, 'compilation options', 'debug dcus')
  else
    SetJsonValue(ChildObject(FJson, 'compilation options', True), 'debug dcus', BoolValue(Value = TOptionalBool.Yes));
end;

function TProductConfig.GetDefines: TArray<TDefineItem>;
begin
  Result := nil;
  var Compilation := ChildObject(FJson, 'compilation options', False);
  if Compilation = nil then Exit;
  var Value := Compilation.GetValue('defines');
  if not (Value is TJSONArray) then Exit;

  for var Item in TJSONArray(Value) do
  begin
    var Define: TDefineItem;
    if Item is TJSONObject then
    begin
      for var Pair in TJSONObject(Item) do
      begin
        Define.Name := Pair.JsonString.Value;
        Define.Enabled := not (Pair.JsonValue is TJSONFalse);
        Result := Result + [Define];
      end;
    end
    else
    begin
      Define.Name := Item.Value;
      Define.Enabled := True;
      Result := Result + [Define];
    end;
  end;
end;

procedure TProductConfig.SetDefines(const Defines: TArray<TDefineItem>);
begin
  if Length(Defines) = 0 then
  begin
    RemoveValue(FJson, 'compilation options', 'defines');
    Exit;
  end;

  var Arr := TJSONArray.Create;
  for var Define in Defines do
    if Define.Enabled then
      Arr.Add(Define.Name)
    else
      Arr.AddElement(TJSONObject.Create(TJSONPair.Create(Define.Name, TJSONFalse.Create)));
  SetJsonValue(ChildObject(FJson, 'compilation options', True), 'defines', Arr);
end;

{ TTmsConfig }

constructor TTmsConfig.Create;
begin
  inherited;
  FRoot := TJSONObject.Create;
  FProducts := TObjectList<TProductConfig>.Create;
end;

destructor TTmsConfig.Destroy;
begin
  FProducts.Free;
  FRoot.Free;
  inherited;
end;

procedure TTmsConfig.LoadFromJson(const S: string);
begin
  var Value := TJSONValue.ParseJSONValue(S);
  if not (Value is TJSONObject) then
  begin
    Value.Free;
    raise Exception.Create('Invalid result for `tms config-read -json`. Cannot parse "' + S + '" as a JSON object.');
  end;

  FProducts.Clear;
  FRoot.Free;
  FRoot := TJSONObject(Value);

  for var Pair in FRoot do
  begin
    var Name := Pair.JsonString.Value;
    if not Name.StartsWith(ProductPrefix) or not (Pair.JsonValue is TJSONObject) then Continue;
    var Product := TProductConfig.Create(Name.Substring(ProductPrefix.Length).Trim, TJSONObject(Pair.JsonValue));
    if Product.IsAllProducts then FProducts.Insert(0, Product) else FProducts.Add(Product);
  end;

  if FindProduct(AllProductsId) = nil then
  begin
    var Json := TJSONObject.Create;
    FRoot.AddPair(ProductPrefix + AllProductsId, Json);
    FProducts.Insert(0, TProductConfig.Create(AllProductsId, Json));
  end;
end;

function TTmsConfig.Options: TJSONObject;
begin
  Result := ChildObject(FRoot, OptionsKey, True);
end;

function TTmsConfig.GetAlternateRegistryKey: string;
begin
  var Value := Options.GetValue('alternate registry key');
  if (Value = nil) or (Value is TJSONNull) then Exit('');
  Result := Value.Value;
end;

procedure TTmsConfig.SetAlternateRegistryKey(const Value: string);
begin
  SetJsonValue(Options, 'alternate registry key', TJSONString.Create(Value.Trim));
end;

procedure TTmsConfig.GetServers(Items: TServerConfigItems);
begin
  Items.Clear;
  var Servers := ChildObject(Options, 'servers', False);
  if Servers = nil then Exit;

  for var Pair in Servers do
  begin
    if not (Pair.JsonValue is TJSONObject) then Continue;
    var Json := TJSONObject(Pair.JsonValue);
    var Item := TServerConfigItem.Create;
    Items.Add(Item);
    Item.Name := Pair.JsonString.Value;
    Item.ServerType := Json.GetValue<string>('type', '');
    Item.Url := Json.GetValue<string>('url', '');
    Item.Enabled := Json.GetValue<Boolean>('enabled', False);
    Item.AllowInsecureConnections := Json.GetValue<Boolean>('allow insecure connections', False);
  end;
end;

procedure TTmsConfig.SetServers(Items: TServerConfigItems);
begin
  var Servers := TJSONObject.Create;
  try
    for var Item in Items do
    begin
      var Json := TJSONObject.Create;
      Servers.AddPair(Item.Name.Trim, Json);
      // tms ignores everything but "enabled" for the built-in servers.
      if not Item.IsReserved then
      begin
        Json.AddPair('type', Item.ServerType);
        Json.AddPair('url', Item.Url.Trim);
      end;
      Json.AddPair('enabled', BoolValue(Item.Enabled));
      if not Item.IsReserved and Item.AllowInsecureConnections then
        Json.AddPair('allow insecure connections', BoolValue(True));
    end;
  except
    Servers.Free;
    raise;
  end;
  SetJsonValue(Options, 'servers', Servers);
end;

function TTmsConfig.GetProductCount: Integer;
begin
  Result := FProducts.Count;
end;

function TTmsConfig.GetProduct(Index: Integer): TProductConfig;
begin
  Result := FProducts[Index];
end;

function TTmsConfig.FindProduct(const Id: string): TProductConfig;
begin
  for var Product in FProducts do
    if SameText(Product.Id, Id.Trim) then Exit(Product);
  Result := nil;
end;

function TTmsConfig.AddProduct(const Id: string): TProductConfig;
begin
  if Id.Trim = '' then raise Exception.Create('The product id cannot be empty.');
  if FindProduct(Id) <> nil then raise Exception.CreateFmt('There is already a configuration for "%s".', [Id.Trim]);

  var Json := TJSONObject.Create;
  FRoot.AddPair(ProductPrefix + Id.Trim, Json);
  Result := TProductConfig.Create(Id.Trim, Json);
  FProducts.Add(Result);
end;

procedure TTmsConfig.RemoveProduct(const Id: string);
begin
  var Product := FindProduct(Id);
  if Product = nil then Exit;
  if Product.IsAllProducts then raise Exception.Create('The configuration for all products cannot be removed.');

  for var Pair in FRoot do
    if Pair.JsonValue = Product.FJson then
    begin
      FRoot.RemovePair(Pair.JsonString.Value).Free;
      Break;
    end;
  FProducts.Remove(Product);
end;

{ ToParams }

// Key names in -p double "_", "-" and "#", as a single "-" or "_" stands for a space. Same as TBBCmdReader.EscapeForCmd.
function EscapeKey(const Key: string): string;
begin
  Result := Key.Replace('_', '__', [rfReplaceAll]).Replace('-', '--', [rfReplaceAll]).Replace('#', '##', [rfReplaceAll]);
end;

function ScalarToCmd(const Value: TJSONValue): string;
begin
  if (Value = nil) or (Value is TJSONNull) then Exit('');
  if Value is TJSONBool then
  begin
    if TJSONBool(Value).AsBoolean then Exit('true') else Exit('false');
  end;
  // TJSONNumber is a TJSONString, so it must go first.
  if Value is TJSONNumber then Exit(TJSONNumber(Value).Value);
  if Value is TJSONString then Exit(BBYamlEscapeString(TJSONString(Value).Value, False));
  raise Exception.Create('Unexpected value in the configuration: ' + Value.ToJSON);
end;

// Arrays are written as "[a,b,{name = value}]", like `tms config-read -cmd` does.
function ArrayToCmd(const Arr: TJSONArray): string;
begin
  var Items: TArray<string> := nil;
  for var Item in Arr do
  begin
    if Item is TJSONObject then
    begin
      var Members: TArray<string> := nil;
      for var Pair in TJSONObject(Item) do
      begin
        if (Pair.JsonValue is TJSONObject) or (Pair.JsonValue is TJSONArray) then
          raise Exception.Create('Unexpected value in the configuration: ' + Item.ToJSON);
        var Value := ScalarToCmd(Pair.JsonValue);
        if Value = '' then Members := Members + [Pair.JsonString.Value + ' =']
        else Members := Members + [Pair.JsonString.Value + ' = ' + Value];
      end;
      Items := Items + ['{' + string.Join(',', Members) + '}'];
      Continue;
    end;

    if Item is TJSONArray then
      raise Exception.Create('Unexpected value in the configuration: ' + Arr.ToJSON);

    var Value := ScalarToCmd(Item);
    if Value <> '' then Items := Items + [Value];
  end;
  Result := '[' + string.Join(',', Items) + ']';
end;

procedure AddParams(const Path: string; const Obj: TJSONObject; const Params: TList<string>);
begin
  for var Pair in Obj do
  begin
    var FullKey := Path + EscapeKey(Pair.JsonString.Value);
    var Value := Pair.JsonValue;
    if Value is TJSONObject then
    begin
      AddParams(FullKey + ':', TJSONObject(Value), Params);
      Continue;
    end;

    var CmdValue: string;
    if Value is TJSONArray then CmdValue := ArrayToCmd(TJSONArray(Value)) else CmdValue := ScalarToCmd(Value);
    if CmdValue <> '' then CmdValue := ' ' + CmdValue;

    // The parameter goes inside double quotes in the command line, and there is no way to escape them.
    if (FullKey + CmdValue).Contains('"') then
      raise Exception.CreateFmt('The value of "%s" cannot contain double quotes (").', [FullKey.Replace(':', ' > ')]);

    Params.Add('-p:"' + FullKey + ' =' + CmdValue + '"');
  end;
end;

function TTmsConfig.ToParams: TArray<string>;
begin
  var Params := TList<string>.Create;
  try
    AddParams('', FRoot, Params);
    Result := Params.ToArray;
  finally
    Params.Free;
  end;
end;

end.

unit UUninstallInfo;
{$i ../../tmssetup.inc}
interface
uses UCoreTypes, Deget.CoreTypes, System.JSON;
type
  IUninstallInfo = interface
    function Value: TJSONObject;

    function UninstallLevel: TEngineLevel;

    function ProjectId: string;
    function IDE: TIDEName;
    function Platform: TPlatform;
    function Package: string;
    function PathToCompiler: string;
    function LockedFilesFolder: string;

    function DryRun: boolean;
  end;

  TUninstallInfo = class(TInterfacedObject, IUninstallInfo)
  private
    FValue: TJSONObject;
    FUninstallLevel: TEngineLevel;
    FDryRun: boolean;
    FIDE: TIDEName;
    FPlatform: TPlatform;
    FPackage: string;
    FProjectId: string;
    FPathToCompiler: string;
    FLockedFilesFolder: string;

  public
    constructor Create(const data: string; const aDryRun: boolean;
        const aUninstallLevel: TEngineLevel; const aProjectId: string;
        const aIDE: TIDEName; const aPlatform: TPlatform; const aPackage, aPathToCompiler, aLockedFilesFolder: string);
    destructor Destroy; override;

    function Value: TJSONObject;

    function UninstallLevel: TEngineLevel;
    function ProjectId: string;
    function IDE: TIDEName;
    function Platform: TPlatform;
    function Package: string;
    function DryRun: boolean;
    function PathToCompiler: string;
    function LockedFilesFolder: string;
  end;

  TJSONObjectHelper = class helper for TJSONObject
  public
    function ReadStr(const ParamName: string; DefaultValue: string = ''): string;
    procedure WriteStr(const ParamName, Value: string);
    function ReadBool(const ParamName: string; DefaultValue: Boolean = False): Boolean;
    procedure WriteBool(const ParamName: string; const Value: Boolean);
    function GetObject(const ParamName: string): TJSONObject;
    function GetArray(const ParamName: string): TJSONArray;
  end;


implementation

{ TUninstallInfo }

constructor TUninstallInfo.Create(const data: string;
             const aDryRun: boolean;
             const aUninstallLevel: TEngineLevel;
             const aProjectId: string; const aIDE: TIDEName;
             const aPlatform: TPlatform; const aPackage, aPathToCompiler, aLockedFilesFolder: string);
begin
  if data = '' then FValue := TJSONObject.Create
  else FValue := TJSONObject.ParseJSONValue(data, false, true) as TJSONObject;
  FDryRun := aDryRun;
  FUninstallLevel := aUninstallLevel;
  FProjectId := aProjectId;
  FIDE := aIDE;
  FPlatform := aPlatform;
  FPackage := aPackage;
  FPathToCompiler := aPathToCompiler;
  FLockedFilesFolder := aLockedFilesFolder;
end;

destructor TUninstallInfo.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TUninstallInfo.DryRun: boolean;
begin
  Result := FDryRun;
end;

function TUninstallInfo.IDE: TIDEName;
begin
  Result := FIDE;
end;

function TUninstallInfo.LockedFilesFolder: string;
begin
  Result := FLockedFilesFolder;
end;

function TUninstallInfo.Package: string;
begin
  Result := FPackage;
end;

function TUninstallInfo.PathToCompiler: string;
begin
  Result := FPathToCompiler;
end;

function TUninstallInfo.Platform: TPlatform;
begin
  Result := FPlatform;
end;

function TUninstallInfo.ProjectId: string;
begin
  Result := FProjectId;
end;

function TUninstallInfo.UninstallLevel: TEngineLevel;
begin
  Result := FUninstallLevel;
end;

function TUninstallInfo.Value: TJSONObject;
begin
  Result := FValue;
end;

{ TJSONObjectHelper }

function TJSONObjectHelper.GetObject(const ParamName: string): TJSONObject;
var
  JValue: TJSONValue;
begin
  JValue := Self.GetValue(ParamName);
  if (JValue <> nil) and (JValue is TJSONObject) then
    Exit(TJSONObject(JValue));

  Self.RemovePair(ParamName);
  Result := TJSONObject.Create;
  Self.AddPair(ParamName, Result);
end;

function TJSONObjectHelper.GetArray(const ParamName: string): TJSONArray;
var
  JValue: TJSONValue;
begin
  JValue := Self.GetValue(ParamName);
  if (JValue <> nil) and (JValue is TJSONArray) then
    Exit(TJSONArray(JValue));

  Self.RemovePair(ParamName);
  Result := TJSONArray.Create;
  Self.AddPair(ParamName, Result);
end;

function TJSONObjectHelper.ReadBool(const ParamName: string; DefaultValue: Boolean): Boolean;
var
  Element: TJsonValue;
begin
  Element := Self.GetValue(ParamName);
  if Element <> nil then
    Result := (Element as TJSONBool).AsBoolean
  else
    Result := DefaultValue;
end;

function TJSONObjectHelper.ReadStr(const ParamName: string; DefaultValue: string): string;
var
  Element: TJsonValue;
begin
  Element := Self.GetValue(ParamName);
  if Element <> nil then
    Result := (Element as TJSONString).Value
  else
    Result := DefaultValue;
end;

procedure TJSONObjectHelper.WriteBool(const ParamName: string; const Value: Boolean);
begin
  Self.RemovePair(ParamName);
  Self.AddPair(ParamName, Value);
end;

procedure TJSONObjectHelper.WriteStr(const ParamName, Value: string);
begin
  Self.RemovePair(ParamName);
  Self.AddPair(ParamName, Value);
end;


end.

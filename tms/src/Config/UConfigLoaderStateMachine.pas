unit UConfigLoaderStateMachine;
{$i ../../tmssetup.inc}

interface
uses BBArrays, BBClasses, Megafolders.Definition,
     UConfigDefinition, Generics.Collections, SysUtils,
     Deget.CoreTypes, UMultiLogger, UConfigKeys, ULogger;

type
  TSectionConf = class(TSection)
  protected
    Config: TConfigDefinition;
    ProductConfig: TProductConfigDefinition;
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition; const aProductConfig: TProductConfigDefinition);

  end;

  TMainSectionConf = class(TSectionConf)
  public
    constructor Create(const aConfig: TConfigDefinition);
    class function SectionNameStatic: string; override;
  end;

  TProductSectionConf = class(TSectionConf)
  private
    ProductId: string;
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition; const aProductId: string; const aErrorInfo: TErrorInfo);
    class function SectionNameStatic: string; override;
    function SectionName: string; override;
    function ExtraInfo: string; override;
  end;

  TOptionsSectionConf = class(TSectionConf)
  private
    function MatchSkipRegistering(const Value: string; const ErrorInfo: TErrorInfo): TSkipRegisteringSet;
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition; const aProductConfig: TProductConfigDefinition);
    class function SectionNameStatic: string; override;

    function GetVerbosity(const s: string; const ErrorInfo: TErrorInfo): TVerbosity;
    function GetSkipRegistering(const s: string; const ErrorInfo: TErrorInfo): TSkipRegisteringSet;
  end;

  TExcludedComponentsSectionConf = class(TSectionConf)
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition; const aProductConfig: TProductConfigDefinition);
    procedure LoadedState(const State: TArrayOverrideBehavior); override;
    class function SectionNameStatic: string; override;
  end;

  TIncludedComponentsSectionConf = class(TSectionConf)
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition; const aProductConfig: TProductConfigDefinition);
    procedure LoadedState(const State: TArrayOverrideBehavior); override;
    class function SectionNameStatic: string; override;
  end;

  TAdditionalProductsFoldersSectionConf = class(TSectionConf)
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition; const aProductConfig: TProductConfigDefinition);
    procedure LoadedState(const State: TArrayOverrideBehavior); override;
    class function SectionNameStatic: string; override;
  end;

  TServersSectionConf = class(TSectionConf)
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition; const aProductConfig: TProductConfigDefinition);
    procedure LoadedState(const State: TArrayOverrideBehavior); override;
    class function SectionNameStatic: string; override;
  end;

  TServerSectionConf = class(TSectionConf)
  private
    Name: string;
    function GetServerType(const s: string;
      const ErrorInfo: TErrorInfo): TServerType;
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition; const aName: string; const aProductConfig: TProductConfigDefinition);
    class function SectionNameStatic: string; override;
    function SectionName: string; override;
  end;

  TGitSectionConf = class(TSectionConf)
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition; const aProductConfig: TProductConfigDefinition);
    class function SectionNameStatic: string; override;
    function VarPrefix: string; override;
  end;

  TSvnSectionConf = class(TSectionConf)
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition; const aProductConfig: TProductConfigDefinition);
    class function SectionNameStatic: string; override;
    function VarPrefix: string; override;
  end;

  TDelphiVersionsSectionConf = class(TSectionConf)
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition; const aProductConfig: TProductConfigDefinition);
    procedure LoadedState(const State: TArrayOverrideBehavior); override;
    class function SectionNameStatic: string; override;

    procedure AddOrRemoveDelphiVersion(const dv: TIDEName; const value: string; const ErrorInfo: TErrorInfo);

    function Capture(const dv: TIDEName): TAction;
  end;

  TPlatformsSectionConf = class(TSectionConf)
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition; const aProductConfig: TProductConfigDefinition);
    procedure LoadedState(const State: TArrayOverrideBehavior); override;
    class function SectionNameStatic: string; override;

    procedure AddOrRemoveDelphiPlat(const dp: TPlatform; const value: string; const ErrorInfo: TErrorInfo);
    function Capture(const dp: TPlatform): TAction;
  end;

  TCompilationSectionConf = class(TSectionConf)
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition; const aProductConfig: TProductConfigDefinition);
    class function SectionNameStatic: string; override;

  end;

  TAdvancedOptionsSectionConf = class(TSectionConf)
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition; const aProductConfig: TProductConfigDefinition);
    class function SectionNameStatic: string; override;

  end;

  TRegistrationSectionConf = class(TSectionConf)
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition; const aProductConfig: TProductConfigDefinition);
    class function SectionNameStatic: string; override;

  end;

  TCompilerPathSectionConf = class(TSectionConf)
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition; const aProductConfig: TProductConfigDefinition);
    class function SectionNameStatic: string; override;
    function VarPrefix: string; override;

    function Capture(const dv: TIDEName): TAction;
  end;

  TCompilerParametersSectionConf = class(TSectionConf)
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition; const aProductConfig: TProductConfigDefinition);
    class function SectionNameStatic: string; override;

    function Capture(const dv: TIDEName): TAction;
  end;

  TDefinesSectionConf = class(TSectionConf)
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition;
        const aProductConfig: TProductConfigDefinition);
    procedure LoadedState(const State: TArrayOverrideBehavior); override;
    class function SectionNameStatic: string; override;

  end;

  TTmsBuildOptionsSectionConf = class(TSectionConf)
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition);
    class function SectionNameStatic: string; override;
  end;

  TDcuMegafoldersSectionConf = class(TSectionConf)
  public
    constructor Create(const aParent: TSection; const aConfig: TConfigDefinition; const aProductConfig: TProductConfigDefinition);
    procedure LoadedState(const State: TArrayOverrideBehavior); override;
    class function SectionNameStatic: string; override;
  end;

implementation

{ TSectionConf }

constructor TSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition; const aProductConfig: TProductConfigDefinition);
begin
  inherited Create(aParent);
  Config := aConfig;
  ProductConfig := aProductConfig;
end;

{ TMainSectionConf }

constructor TMainSectionConf.Create(const aConfig: TConfigDefinition);
begin
  inherited Create(nil, aConfig, nil);

  ChildSections.Add(TTmsBuildOptionsSectionConf.SectionNameStatic, TTmsBuildOptionsSectionConf.Create(Self, aConfig));

  ChildSectionAction :=
    function(Name: string; ErrorInfo: TErrorInfo; const KeepValues: boolean): TSection
    begin
      if ChildSections.TryGetValue(Name, Result, ErrorInfo, KeepValues) then exit;

      if not Name.StartsWith('configuration for ')
        then raise Exception.Create('The name "' + Name
                   + '" is invalid. Root options must start with "configuration for" or be "'
                   + TTmsBuildOptionsSectionConf.SectionNameStatic + '". ' + ErrorInfo.ToString);

      Name := Name.Substring(('configuration for ').Length).Trim;
      if ChildSections.TryGetValue(Name, Result, ErrorInfo, KeepValues) then exit;

      Result := TProductSectionConf.Create(Self, aConfig, Name, ErrorInfo);
      ChildSections.Add(Name, Result);
    end;
end;

class function TMainSectionConf.SectionNameStatic: string;
begin
  Result := 'root';
end;


{ TProductSectionConf }

constructor TProductSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition; const aProductId: string; const aErrorInfo: TErrorInfo);
begin
  var Product := aConfig.GetProduct(aProductId);
  inherited Create(aParent, aConfig, Product);
  Product.CreatedBy := aErrorInfo.ToString;
  ProductId := aProductId;

  ChildSections.Add(TOptionsSectionConf.SectionNameStatic, TOptionsSectionConf.Create(Self, aConfig, ProductConfig));
  ChildSections.Add(TDelphiVersionsSectionConf.SectionNameStatic, TDelphiVersionsSectionConf.Create(Self, aConfig, ProductConfig));
  ChildSections.Add(TPlatformsSectionConf.SectionNameStatic, TPlatformsSectionConf.Create(Self, aConfig, ProductConfig));
  ChildSections.Add(TCompilationSectionConf.SectionNameStatic, TCompilationSectionConf.Create(Self, aConfig, ProductConfig));
  ChildSections.Add(TRegistrationSectionConf.SectionNameStatic, TRegistrationSectionConf.Create(Self, aConfig, ProductConfig));
  ChildSections.Add(TAdvancedOptionsSectionConf.SectionNameStatic, TAdvancedOptionsSectionConf.Create(Self, aConfig, ProductConfig));
  ChildSections.Add(TCompilerPathSectionConf.SectionNameStatic, TCompilerPathSectionConf.Create(Self, aConfig, ProductConfig));
  ChildSections.Add(TCompilerParametersSectionConf.SectionNameStatic, TCompilerParametersSectionConf.Create(Self, aConfig, ProductConfig));

end;

function TProductSectionConf.ExtraInfo: string;
begin
  Result := ProductId;
end;

function TProductSectionConf.SectionName: string;
begin
  Result := ProductId;
end;

class function TProductSectionConf.SectionNameStatic: string;
begin
  Result := 'Error';
end;

{ TOptionsSectionConf }

constructor TOptionsSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition;
  const aProductConfig: TProductConfigDefinition);
var s: string;
begin
  inherited Create(aParent, aConfig, aProductConfig);

  Actions := TListOfActions.Create;
  s := 'verbosity'; Actions.Add(s, procedure(value: string; ErrorInfo: TErrorInfo) begin ProductConfig.SetInt(ConfigKeys.Verbosity, Integer(GetVerbosity(value, ErrorInfo))); end);
  s := 'dry run'; Actions.Add(s, procedure(value: string; ErrorInfo: TErrorInfo) begin ProductConfig.SetBool(ConfigKeys.DryRun, GetBool(value, ErrorInfo)); end);
  s := 'skip register'; Actions.Add(s, procedure(value: string; ErrorInfo: TErrorInfo) begin ProductConfig.SetInt(ConfigKeys.SkipRegister, Byte(GetSkipRegistering(value, ErrorInfo))); end);
end;

function TOptionsSectionConf.GetVerbosity(const s: string;
  const ErrorInfo: TErrorInfo): TVerbosity;
var
  s1: string;
begin
 s1 := AnsiLowerCase(s);
 if (s1 = 'trace') then exit(TVerbosity.trace);
 if (s1 = 'info') then exit(TVerbosity.info);
 if (s1 = 'error') then exit(TVerbosity.error);

 raise Exception.Create('"' + s + '" is not a valid verbosity value. It must be "trace", "info" or "error". ' + ErrorInfo.ToString);

end;

function TOptionsSectionConf.MatchSkipRegistering(const Value: string; const ErrorInfo: TErrorInfo): TSkipRegisteringSet;
begin
   var v := Value.ToLowerInvariant.Trim;
   if v = 'all' then exit([Low(TSkipRegisteringOptions)..High(TSkipRegisteringOptions)]);

   for var SkipOption := Low(TSkipRegisteringOptions) to High(TSkipRegisteringOptions) do
   begin
     if v = TSkipRegisteringName[SkipOption] then exit([SkipOption]);
   end;
   raise Exception.Create('"' + Value.Trim + '" is not a valid skip registering value. It must be any of [All, Packages, StartMenu, Help, WindowsPath, WebCore, Registry, FileLinks].'+ ErrorInfo.ToString);
end;

function TOptionsSectionConf.GetSkipRegistering(const s: string;
  const ErrorInfo: TErrorInfo): TSkipRegisteringSet;
var
  s1: string;
begin
 s1 := AnsiLowerCase(s);
 if (s1 = 'true') then exit([Low(TSkipRegisteringOptions)..High(TSkipRegisteringOptions)]);
 if (s1 = 'false') then exit([]);
 if not s1.StartsWith('[') or not s1.EndsWith(']') then
   raise Exception.Create('"' + s + '" is not a valid skip registering value. It must be "true", "false" or an array containing any of [All, Packages, StartMenu, Help, WindowsPath, WebCore, Registry, FileLinks]. ' + ErrorInfo.ToString);

 Result := [];
 var Values := s1.Substring(1, s1.Length - 2).Split([','], TStringSplitOptions.ExcludeEmpty);
 for var Value in Values do
 begin
   var TrimValue := Value.Trim;
   if TrimValue.StartsWith('-')
     then Result := Result - MatchSkipRegistering(TrimValue.Substring(1), ErrorInfo)
     else Result := Result + MatchSkipRegistering(TrimValue, ErrorInfo);
 end;


end;

class function TOptionsSectionConf.SectionNameStatic: string;
begin
  Result := 'options';
end;


{ TExcludedComponentsSectionConf }

constructor TExcludedComponentsSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition;
  const aProductConfig: TProductConfigDefinition);
begin
  inherited Create(aParent, aConfig, aProductConfig);
  SectionValueTypes := TSectionValueTypes.NoValues;

  ClearArrayValues := procedure begin aConfig.ClearExcludedComponents;end;

  ArrayMainAction := procedure(name, value: string; ErrorInfo: TErrorInfo) begin aConfig.AddExcludedComponent(name, ErrorInfo.ToString); end;

end;

procedure TExcludedComponentsSectionConf.LoadedState(
  const State: TArrayOverrideBehavior);
begin
  if Root.CreatedBy.StartsWith('Main: ') then  Config.PrefixedProperties[TGlobalPrefixedProperties.ExcludedProducts] := State;
end;

class function TExcludedComponentsSectionConf.SectionNameStatic: string;
begin
  Result := 'excluded products';
end;

{ TIncludedComponentsSectionConf }

constructor TIncludedComponentsSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition;
  const aProductConfig: TProductConfigDefinition);
begin
  inherited Create(aParent, aConfig, aProductConfig);
  SectionValueTypes := TSectionValueTypes.NoValues;
  ClearArrayValues := procedure begin aConfig.ClearIncludedComponents;end;

  ArrayMainAction := procedure(name, value: string; ErrorInfo: TErrorInfo) begin aConfig.AddIncludedComponent(name, ErrorInfo.ToString); end;
end;

procedure TIncludedComponentsSectionConf.LoadedState(
  const State: TArrayOverrideBehavior);
begin
  if Root.CreatedBy.StartsWith('Main: ') then  Config.PrefixedProperties[TGlobalPrefixedProperties.IncludedProducts] := State;
end;

class function TIncludedComponentsSectionConf.SectionNameStatic: string;
begin
  Result := 'included products';
end;

{ TAdditionalProductsFoldersSectionConf }

constructor TAdditionalProductsFoldersSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition;
  const aProductConfig: TProductConfigDefinition);
begin
  inherited Create(aParent, aConfig, aProductConfig);
  SectionValueTypes := TSectionValueTypes.NoValues;

  ClearArrayValues := procedure begin aConfig.ClearAdditionalProductsFolders;end;

  ArrayMainAction := procedure(name, value: string; ErrorInfo: TErrorInfo) begin aConfig.AddAdditionalProductsFolder(name, ErrorInfo.ToString); end;
end;

procedure TAdditionalProductsFoldersSectionConf.LoadedState(
  const State: TArrayOverrideBehavior);
begin
  if Root.CreatedBy.StartsWith('Main: ') then  Config.PrefixedProperties[TGlobalPrefixedProperties.AdditionalProductsFolders] := State;
end;

class function TAdditionalProductsFoldersSectionConf.SectionNameStatic: string;
begin
  Result := 'additional products folders';
end;

{ TDelphiVersionsSectionConf }

procedure TDelphiVersionsSectionConf.AddOrRemoveDelphiVersion(
  const dv: TIDEName; const value: string; const ErrorInfo: TErrorInfo);
begin
  if GetBoolEx(value, ErrorInfo) then
  begin
    ProductConfig.SetIDEName(dv , true);
  end
  else
  begin
    ProductConfig.SetIDEName(dv, false);
  end;
end;

function TDelphiVersionsSectionConf.Capture(const dv: TIDEName): TAction;
begin
  Result := procedure(value: string; ErrorInfo: TErrorInfo)
    begin
      AddOrRemoveDelphiVersion(dv, value, ErrorInfo);
    end;
end;

constructor TDelphiVersionsSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition;
  const aProductConfig: TProductConfigDefinition);
begin
  inherited Create(aParent, aConfig, aProductConfig);
  SectionValueTypes := TSectionValueTypes.NoValues;
  ContainsArrays := true;

  ClearArrayValues := procedure begin aProductConfig.ClearIDENames;end;

  Actions := TListOfActions.Create;
  for var dv := Low(TIDEName) to High(TIDEName) do
  begin
    Actions.Add(IDEId[dv], Capture(dv));
  end;
end;

procedure TDelphiVersionsSectionConf.LoadedState(
  const State: TArrayOverrideBehavior);
begin
  if Root.CreatedBy.StartsWith('Main: ') then ProductConfig.PrefixedProperties[TProductPrefixedProperties.DelphiVersions] := State;
end;

class function TDelphiVersionsSectionConf.SectionNameStatic: string;
begin
  Result := 'delphi versions';
end;

{ TPlatformsSectionConf }

procedure TPlatformsSectionConf.AddOrRemoveDelphiPlat(const dp: TPlatform; const value: string; const ErrorInfo: TErrorInfo);
begin
  if GetBoolEx(value, ErrorInfo) then
  begin
    ProductConfig.SetPlatform(dp , true);
  end
  else
  begin
    ProductConfig.SetPlatform(dp, false);
  end;
end;

function TPlatformsSectionConf.Capture(const dp: TPlatform): TAction;
begin
  Result := procedure(value: string; ErrorInfo: TErrorInfo) begin AddOrRemoveDelphiPlat(dp, value, ErrorInfo); end;
end;

constructor TPlatformsSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition;
  const aProductConfig: TProductConfigDefinition);
var
  dp: TPlatform;
begin
  inherited Create(aParent, aConfig, aProductConfig);
  SectionValueTypes := TSectionValueTypes.NoValues;
  ContainsArrays := true;

  ClearArrayValues := procedure begin aProductConfig.ClearPlatforms;end;

  Actions := TListOfActions.Create;
  for dp := Low(TPlatform) to High(TPlatform) do
  begin
    Actions.Add(PlatformId[dp], Capture(dp));
  end;
end;

procedure TPlatformsSectionConf.LoadedState(
  const State: TArrayOverrideBehavior);
begin
  if Root.CreatedBy.StartsWith('Main: ') then ProductConfig.PrefixedProperties[TProductPrefixedProperties.Platforms] := State;
end;


class function TPlatformsSectionConf.SectionNameStatic: string;
begin
  Result := 'platforms'
end;

{ TCompilerSectionConf }

constructor TCompilationSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition;
  const aProductConfig: TProductConfigDefinition);
var
  s: string;
begin
  inherited Create(aParent, aConfig, aProductConfig);

  Actions := TListOfActions.Create;
  s := 'debug dcus'; Actions.Add(s, procedure(value: string; ErrorInfo: TErrorInfo) begin ProductConfig.SetBool(ConfigKeys.DebugDcus, GetBool(value, ErrorInfo)); end);

  ChildSections.Add(TDefinesSectionConf.SectionNameStatic, TDefinesSectionConf.Create(Self, aConfig, ProductConfig));

end;


class function TCompilationSectionConf.SectionNameStatic: string;
begin
  Result := 'compilation options';
end;

{ TRegistrationSectionConf }

constructor TRegistrationSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition;
  const aProductConfig: TProductConfigDefinition);
var
  s: string;
begin
  inherited Create(aParent, aConfig, aProductConfig);

  Actions := TListOfActions.Create;
  s := 'add source code to library path'; Actions.Add(s, procedure(value: string; ErrorInfo: TErrorInfo) begin ProductConfig.SetBool(ConfigKeys.AddSourceCodeToLibraryPath, GetBool(value, ErrorInfo)); end);
end;

class function TRegistrationSectionConf.SectionNameStatic: string;
begin
  Result := 'registration options';
end;

{ TAdvancedOptionsSectionConf }

constructor TAdvancedOptionsSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition;
  const aProductConfig: TProductConfigDefinition);
begin
  inherited Create(aParent, aConfig, aProductConfig);

  Actions := TListOfActions.Create;
  Actions.Add('use symlinks', procedure(value: string; ErrorInfo: TErrorInfo) begin  ProductConfig.SetBool(ConfigKeys.SymLinks, GetBool(value, ErrorInfo)) end);
  Actions.Add('keep parallel folders', procedure(value: string; ErrorInfo: TErrorInfo) begin  ProductConfig.SetBool(ConfigKeys.KeepParallelFolders, GetBool(value, ErrorInfo)) end);
  Actions.Add('modify sources', procedure(value: string; ErrorInfo: TErrorInfo) begin  ProductConfig.SetBool(ConfigKeys.ModifySources, GetBool(value, ErrorInfo)) end);
  Actions.Add('partial builds', procedure(value: string; ErrorInfo: TErrorInfo) begin  ProductConfig.SetBool(ConfigKeys.PartialBuilds, GetBool(value, ErrorInfo)) end);
  Actions.Add('add source code to library path', procedure(value: string; ErrorInfo: TErrorInfo) begin ProductConfig.SetBool(ConfigKeys.AddSourceCodeToLibraryPath, GetBool(value, ErrorInfo)); end);

end;

class function TAdvancedOptionsSectionConf.SectionNameStatic: string;
begin
  Result := 'advanced options';
end;
{ TCompilerPathSectionConf }

function TCompilerPathSectionConf.Capture(const dv: TIDEName): TAction;
begin
  Result := procedure(value: string; ErrorInfo: TErrorInfo)
    begin
      ProductConfig.SetString(ConfigKeys.CompilerPath +  IDEId[dv], value);
    end;

end;

constructor TCompilerPathSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition;
  const aProductConfig: TProductConfigDefinition);
begin
  inherited Create(aParent, aConfig, aProductConfig);

  Actions := TListOfActions.Create;
  for var dv := Low(TIDEName) to High(TIDEName) do
  begin
    Actions.Add(IDEId[dv], Capture(dv));
  end;

end;

class function TCompilerPathSectionConf.SectionNameStatic: string;
begin
  Result := 'compiler paths';
end;

function TCompilerPathSectionConf.VarPrefix: string;
begin
  Result := 'compiler-paths_';
end;

{ TCompilerParametersSectionConf }

function TCompilerParametersSectionConf.Capture(const dv: TIDEName): TAction;
begin
  Result := procedure(value: string; ErrorInfo: TErrorInfo)
    begin
      ProductConfig.SetString(ConfigKeys.CompilerParameters +  IDEId[dv], value);
    end;

end;

constructor TCompilerParametersSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition;
  const aProductConfig: TProductConfigDefinition);
begin
  inherited Create(aParent, aConfig, aProductConfig);

  Actions := TListOfActions.Create;
  for var dv := Low(TIDEName) to High(TIDEName) do
  begin
    Actions.Add(IDEId[dv], Capture(dv));
  end;

end;

class function TCompilerParametersSectionConf.SectionNameStatic: string;
begin
  Result := 'compiler parameters';
end;

{ TDefinesSectionConf }

constructor TDefinesSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition;
  const aProductConfig: TProductConfigDefinition);
begin
  inherited Create(aParent, aConfig, aProductConfig);
  Duplicated := TDictionary<string, boolean>.Create;
  SectionValueTypes := TSectionValueTypes.Both;

  ClearArrayValues := procedure begin aProductConfig.ClearDefines;end;

  ArrayMainAction := procedure (name, value: string; ErrorInfo: TErrorInfo) begin
    if GetBoolEx(value, ErrorInfo) then
    begin
      ProductConfig.AddDefine(name, ErrorInfo.ToString);
    end
    else
    begin
      ProductConfig.RemoveDefine(name, ErrorInfo.ToString);
    end;
  end;
end;

procedure TDefinesSectionConf.LoadedState(
  const State: TArrayOverrideBehavior);
begin
  if Root.CreatedBy.StartsWith('Main: ') then ProductConfig.PrefixedProperties[TProductPrefixedProperties.Defines] := State;
end;


class function TDefinesSectionConf.SectionNameStatic: string;
begin
  Result := 'defines';
end;

{ TTmsBuildOptionsSectionConf }

constructor TTmsBuildOptionsSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition);
begin
  inherited Create(aParent, aConfig, nil);
  ChildSections.Add(TExcludedComponentsSectionConf.SectionNameStatic, TExcludedComponentsSectionConf.Create(Self, aConfig, ProductConfig));
  ChildSections.Add(TIncludedComponentsSectionConf.SectionNameStatic, TIncludedComponentsSectionConf.Create(Self, aConfig, ProductConfig));
  ChildSections.Add(TAdditionalProductsFoldersSectionConf.SectionNameStatic, TAdditionalProductsFoldersSectionConf.Create(Self, aConfig, ProductConfig));
  ChildSections.Add(TServersSectionConf.SectionNameStatic, TServersSectionConf.Create(Self, aConfig, ProductConfig));
  ChildSections.Add(TGitSectionConf.SectionNameStatic, TGitSectionConf.Create(Self, aConfig, ProductConfig));
  ChildSections.Add(TSvnSectionConf.SectionNameStatic, TSvnSectionConf.Create(Self, aConfig, ProductConfig));
  ChildSections.Add(TDcuMegafoldersSectionConf.SectionNameStatic, TDcuMegafoldersSectionConf.Create(Self, aConfig, ProductConfig));

  Actions := TListOfActions.Create;
  Actions.Add('build cores', procedure(value: string; ErrorInfo: TErrorInfo) begin  Config.BuildCores := GetInt(value, ErrorInfo) end);
  Actions.Add('alternate registry key', procedure(value: string; ErrorInfo: TErrorInfo) begin Config.AlternateRegistryKey := value; end);
  Actions.Add('prevent sleep', procedure(value: string; ErrorInfo: TErrorInfo) begin  Config.PreventSleep := GetBoolEx(value, ErrorInfo) end);
  Actions.Add('versions to keep', procedure(value: string; ErrorInfo: TErrorInfo) begin  Config.MaxVersionsPerProduct := GetInt(value, ErrorInfo) end);
  Actions.Add('error if skipped', procedure(value: string; ErrorInfo: TErrorInfo) begin  Config.ErrorIfSkipped := GetBool(value, ErrorInfo) end);
end;

class function TTmsBuildOptionsSectionConf.SectionNameStatic: string;
begin
  Result := 'tms smart setup options';
end;

{ TServersSectionConf }

constructor TServersSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition;
  const aProductConfig: TProductConfigDefinition);
begin
  inherited Create(aParent, aConfig, aProductConfig);

  ClearArrayValues := procedure begin aConfig.ServerConfig.ClearServers;end;
  for var i := Low(TServerConfig.BuiltinServers) to High(TServerConfig.BuiltinServers) do
  begin
    var server := TServerConfig.BuiltinServers[i];
    ChildSections.Add(server, TServerSectionConf.Create(Self, aConfig, server, aProductConfig ));
  end;

  ChildSectionAction :=
    function(Name: string; ErrorInfo: TErrorInfo; const KeepValues: boolean): TSection
    begin
      if ChildSections.TryGetValue(Name, Result, ErrorInfo, KeepValues) then
      begin
        if (Result.CreatedBy <> Root.CreatedBy) then
        begin
          Result.CreatedBy := Root.CreatedBy;
          exit; //Allow you to define the same server in different files. Not twice in the same file.
        end;

        raise Exception.Create('The server "' + Name
                   + '" is already defined. ' + ErrorInfo.ToString);

      end;

      Result := TServerSectionConf.Create(Self, aConfig, Name, aProductConfig);
      ChildSections.Add(Name, Result);
    end;
end;

procedure TServersSectionConf.LoadedState(
  const State: TArrayOverrideBehavior);
begin
  if Root.CreatedBy.StartsWith('Main: ') then  Config.PrefixedProperties[TGlobalPrefixedProperties.Servers] := State;
end;

class function TServersSectionConf.SectionNameStatic: string;
begin
  Result := 'servers';
end;

{ TServerSectionConf }

constructor TServerSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition; const aName: string;
  const aProductConfig: TProductConfigDefinition);
begin
  inherited Create(aParent, aConfig, aProductConfig);
  Name := aName;

  Actions := TListOfActions.Create;
  Actions.Add('type', procedure(value: string; ErrorInfo: TErrorInfo)
  begin;
    var Index := Config.ServerConfig.EnsureServer(aName);
    if not Config.ServerConfig.GetServer(Index).IsReservedName then
    begin
      Config.ServerConfig.SetInfo(Index,
        procedure (var Server: TServerConfig)
        begin
          Server.ServerType := GetServerType(Value, ErrorInfo);
      end);
    end;
  end);

  Actions.Add('url', procedure(value: string; ErrorInfo: TErrorInfo)
  begin;
    var Index := Config.ServerConfig.EnsureServer(aName);
    if not Config.ServerConfig.GetServer(Index).IsReservedName then
    begin
      Config.ServerConfig.SetInfo(Index,
        procedure (var Server: TServerConfig)
        begin
          Server.Url := Value.Trim;
        end);
    end;
  end);

  Actions.Add('enabled', procedure(value: string; ErrorInfo: TErrorInfo)
  begin;
    var Index := Config.ServerConfig.EnsureServer(aName);
    Config.ServerConfig.SetInfo(Index, procedure (var Server: TServerConfig) begin Server.Enabled := GetBool(Value, ErrorInfo); end);
  end);

end;

function TServerSectionConf.GetServerType(const s: string;
  const ErrorInfo: TErrorInfo): TServerType;
begin
  Result := TServerConfig.ServerTypeFromString(s, ErrorInfo.ToString);
end;


function TServerSectionConf.SectionName: string;
begin
  Result := Name;
end;

class function TServerSectionConf.SectionNameStatic: string;
begin
  Result := 'Error';
end;


{ TGitSectionConf }

constructor TGitSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition;
  const aProductConfig: TProductConfigDefinition);
begin
  inherited Create(aParent, aConfig, aProductConfig);

  Actions := TListOfActions.Create;
  Actions.Add('git location', procedure(value: string; ErrorInfo: TErrorInfo) begin; var Git := Config.GitConfig; Git.GitCommand := value; Config.GitConfig := Git; end);
  Actions.Add('clone command', procedure(value: string; ErrorInfo: TErrorInfo) begin; var Git := Config.GitConfig; Git.Clone := value; Config.GitConfig := Git; end);
  Actions.Add('pull command', procedure(value: string; ErrorInfo: TErrorInfo) begin; var Git := Config.GitConfig; Git.Pull := value; Config.GitConfig := Git; end);
  Actions.Add('checkout command', procedure(value: string; ErrorInfo: TErrorInfo) begin; var Git := Config.GitConfig; Git.Checkout := value; Config.GitConfig := Git; end);
  Actions.Add('shallow clone command', procedure(value: string; ErrorInfo: TErrorInfo) begin; var Git := Config.GitConfig; Git.ShallowClone := value; Config.GitConfig := Git; end);
end;

class function TGitSectionConf.SectionNameStatic: string;
begin
  Result := 'git';
end;

function TGitSectionConf.VarPrefix: string;
begin
  Result := 'git-';
end;

{ TSvnSectionConf }

constructor TSvnSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition;
  const aProductConfig: TProductConfigDefinition);
begin
  inherited Create(aParent, aConfig, aProductConfig);

  Actions := TListOfActions.Create;
  Actions.Add('svn location', procedure(value: string; ErrorInfo: TErrorInfo) begin; var Svn := Config.SvnConfig; Svn.SvnCommand := value; Config.SvnConfig := Svn; end);
  Actions.Add('checkout command', procedure(value: string; ErrorInfo: TErrorInfo) begin; var Svn := Config.SvnConfig; Svn.Checkout := value; Config.SvnConfig := Svn; end);
  Actions.Add('update command', procedure(value: string; ErrorInfo: TErrorInfo) begin; var Svn := Config.SvnConfig; Svn.Update := value; Config.SvnConfig := Svn; end);
  Actions.Add('export command', procedure(value: string; ErrorInfo: TErrorInfo) begin; var Svn := Config.SvnConfig; Svn.Export := value; Config.SvnConfig := Svn; end);
end;

class function TSvnSectionConf.SectionNameStatic: string;
begin
  Result := 'svn';
end;

function TSvnSectionConf.VarPrefix: string;
begin
  Result := 'svn-';
end;

{ TDcuMegafoldersSectionConf }

constructor TDcuMegafoldersSectionConf.Create(const aParent: TSection;
  const aConfig: TConfigDefinition; const aProductConfig: TProductConfigDefinition);
begin
  inherited Create(aParent, aConfig, aProductConfig);

  ClearArrayValues := procedure begin aConfig.DcuMegafolders.Clear;end;


  ArrayMainAction := procedure (name, value: string; ErrorInfo: TErrorInfo)
    begin
      Config.DcuMegafolders.Add(TMegafolder.Create(name, value));
    end;
end;

procedure TDcuMegafoldersSectionConf.LoadedState(
  const State: TArrayOverrideBehavior);
begin
  if Root.CreatedBy.StartsWith('Main: ') then  Config.PrefixedProperties[TGlobalPrefixedProperties.DcuMegafolders] := State;
end;


class function TDcuMegafoldersSectionConf.SectionNameStatic: string;
begin
  Result := 'dcu megafolders';
end;

end.

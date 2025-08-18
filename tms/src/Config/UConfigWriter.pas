unit UConfigWriter;
{$i ../../tmssetup.inc}

interface
uses Classes, SysUtils, Util.Replacer, UConfigDefinition, Generics.Defaults,
     Generics.Collections, ULogger, Deget.CoreTypes, Megafolders.Definition, BBArrays, BBClasses,
     BBYaml.Writer, Types, JSON, BBYaml.Types, UConfigKeys;
type
TConfigWriter = class
  private
    CmdFormat: boolean;
    Cfg: TConfigDefinition;
    function OnComment(const Sender: TBBYamlWriter; const  FullName :string; const Comment: string): string;
    function OnMember(const Sender: TBBYamlWriter;
      const FullName: string; const ArrayIndex: integer): TYamlValue;
    function GetPatternMembers(const Sender: TBBYamlWriter; const Id:string; const ArrayIndex: integer): TArray<TNameAndComment>;
    function GetIncludedExcludedComponents(
      const Values: TEnumerable<string>; const AddExamples: boolean): TYamlValue;
    function GetAdditionalProductsFolders(
      const Values: TEnumerable<string>): TYamlValue;
    function BlockArray: boolean;
    function FlowArray: boolean;
    function GetServer(const Id: string): TYamlValue;
    function GetServerList: TArray<TNameAndComment>;
    function GetDcuMegafolders(const Values: TMegafolderList): TYamlValue;
    function GetMegafolderKey(const ArrayIndex: integer): TArray<TNameAndComment>;
    function GetMegafolderValue(const ArrayIndex: integer): TYamlValue;
    function GetProductConfigurations: TArray<TNameAndComment>;
    function GetProductComment(const Name: string): string;
    function OnProductMember(const ProductId, FullName: string;
      const ArrayIndex: integer): TYamlValue;
    function GetProductId(const FullName: string): string;
    function GetProductCfg(const aProductId: string): TProductConfigDefinition;
    function GetVerbosity(
      const CfgProduct: TProductConfigDefinition): TYamlValue;
    function GetSkipRegister(
      const CfgProduct: TProductConfigDefinition): TYamlValue;
    function GetProductBool(const CfgProduct: TProductConfigDefinition;
      const Key: string; const KeyDefault: boolean): TYamlValue;
    function GetDelphiVersions(
      const CfgProduct: TProductConfigDefinition): TYamlValue;
    function GetPlatforms(
      const CfgProduct: TProductConfigDefinition): TYamlValue;
    function GetDefines(const CfgProduct: TProductConfigDefinition): TYamlValue;
    function GetCompilerPaths(
      const CfgProduct: TProductConfigDefinition; const IdeName: string): TYamlValue;
    function AddExampleCompilerPaths(
      const CfgProduct: TProductConfigDefinition): boolean;
    function GetLastEntry(const FullName: string): string;
    function GetDefine(const CfgProduct: TProductConfigDefinition;
      const Define: string): TYamlValue;
  public
    constructor Create(const aCmdFormat: boolean);
    procedure Save(const aCfg: TConfigDefinition; const FileName: string);
end;

implementation
uses UTmsBuildSystemUtils, Deget.IDETypes;


constructor TConfigWriter.Create(const aCmdFormat: boolean);
begin
{$message warn 'add and replace array defines'}
{$message warn 'config read including arrays'}

  CmdFormat := aCmdFormat;
end;

function TConfigWriter.BlockArray: boolean;
begin
  if CmdFormat then exit(true);
  exit(false);
end;

function TConfigWriter.FlowArray: boolean;
begin
  exit(true);
end;

function TConfigWriter.GetServerList: TArray<TNameAndComment>;
begin 
  var Start := Length(TServerConfig.BuiltinServers); 
  Result := nil;             
  SetLength(Result, Cfg.ServerConfig.ServerCount - Start);

  for var i := 0 to High(Result) do
  begin
    Result[i] := TNameAndComment.Create(Cfg.ServerConfig.GetServer(i + Start).Name,'');
  end;
end;

function TConfigWriter.GetMegafolderKey(const ArrayIndex: integer): TArray<TNameAndComment>;
begin 
  Result := nil;
  SetLength(Result, 1);
  Result[0] := TNameAndComment.Create(Cfg.DcuMegafolders[ArrayIndex].Folder,'');
end;

function TConfigWriter.GetMegafolderValue(const ArrayIndex: integer): TYamlValue;
begin 
  Result := Cfg.DcuMegafolders[ArrayIndex].Mask;
end;

function TConfigWriter.GetProductComment(const Name: string): string;
begin
  if Name = GlobalProductId then exit('Global configuration');
  Result := '==============='#10 + Name.ToUpperInvariant + #10'===============';
end;

function TConfigWriter.GetProductConfigurations: TArray<TNameAndComment>;
begin 
  var SortedProducts := Cfg.Products.Keys.ToArray;
  var Comparer: IComparer<string> := TDelegatedComparer<string>.Create(
  function(const Left, Right: string): integer
  begin
    if Left = Right then exit(0);
    if Left = GlobalProductId then exit(-1);
    if Right = GlobalProductId then exit(1);
    exit (Left.CompareTo(Right));
  end);

  TArray.Sort<string>(SortedProducts, Comparer);

  Result := nil;             
  SetLength(Result, Length(SortedProducts));

  for var i := 0 to High(Result) do
  begin
    Result[i] := TNameAndComment.Create('configuration for ' + SortedProducts[i], GetProductComment(SortedProducts[i]));
  end;
end;


function TConfigWriter.GetPatternMembers(const Sender: TBBYamlWriter;
  const Id: string; const ArrayIndex: integer): TArray<TNameAndComment>;
begin
  if Id.StartsWith('tms smart setup options:servers:') then exit(GetServerList);
  if Id.StartsWith('tms smart setup options:dcu megafolders:') then exit(GetMegafolderKey(ArrayIndex));
  if Id.StartsWith('^configuration for') then exit(GetProductConfigurations);
  
  raise Exception.Create('Unknown property id: ' + Id);
end;

function TConfigWriter.GetIncludedExcludedComponents(const Values: TEnumerable<string>; const AddExamples: boolean): TYamlValue;
begin
  var ArrayResult := Values.ToArray;
  if Length(ArrayResult) > 0 then
  begin
    TArray.Sort<string>(ArrayResult);
    exit(TYamlValue.MakeArray(ArrayResult, BlockArray));
  end;

  if AddExamples then 
  exit(TYamlValue.MakeArray([
    TYamlValue.MakeEmpty('tms.example1'),
    TYamlValue.MakeEmpty('tms.example2')
  ], BlockArray));

  exit(TYamlValue.MakeEmpty(''));

end;

function TConfigWriter.GetAdditionalProductsFolders(const Values: TEnumerable<string>): TYamlValue;
begin
  var ArrayResult := Values.ToArray;
  if Length(ArrayResult) > 0 then
  begin
    TArray.Sort<string>(ArrayResult);
    exit(TYamlValue.MakeArray(ArrayResult, BlockArray));
  end;

  Result := TYamlValue.MakeArray(
  [
    TYamlValue.MakeEmpty('c:\MyProducts   #All folders containing tmsbuild.yaml files inside c:\MyProducts will be added to the build.'),
    TYamlValue.MakeEmpty('..\..\Other products  #Try to use relative folders if possible.')
  ], BlockArray);

end;

function TConfigWriter.GetServer(const Id: string): TYamlValue;
begin
  var ServerName := Id.Substring(0, Id.IndexOf(':')); 
  var PropName := Id.Substring(ServerName.Length + 1);
  var Server := Cfg.ServerConfig.GetServer(ServerName);
  if PropName = '' then exit(TYamlValue.MakeObject);
  if PropName = 'type:' then exit(Server.ServerTypeString);
  if PropName = 'url:' then exit(Server.Url);
  if PropName = 'enabled:' then exit(Server.Enabled);

  raise Exception.Create('Unexpected server property: ' + PropName);
end;   

function TConfigWriter.GetDcuMegafolders(const Values: TMegafolderList): TYamlValue;
begin
  Result := TYamlValue.MakeArray(Values.Count, 
                function(i: integer): TYamlValue begin Result := TYamlValue.MakeObject; end, 
                BlockArray);
end;

function TConfigWriter.GetProductId(const FullName: string): string;
begin
  var Start := 'configuration for '.Length;
  Result := FullName.Substring(Start, FullName.IndexOf(':') - Start).Trim;
end;

function TConfigWriter.GetProductCfg(const aProductId: string): TProductConfigDefinition;
begin
  var ProductId := aProductId;
  if ProductId = GlobalProductId.Replace(' ', '.') then ProductId := GlobalProductId;

  Result := Cfg.GetProduct(ProductId);
end;


function TConfigWriter.OnComment(const Sender: TBBYamlWriter; const FullName,
  Comment: string): string;
begin
  if FullName.StartsWith('configuration for') 
    and FullName.Contains(':compiler paths:') 
    and not FullName.EndsWith(':compiler paths:') then exit('');

  exit(Comment);
end;

function TConfigWriter.OnMember(const Sender: TBBYamlWriter; const FullName: string; const ArrayIndex: integer): TYamlValue;
begin
  if FullName = 'tms smart setup options:' then exit(TYamlValue.MakeObject);
  if FullName = 'tms smart setup options:build cores:' then exit(Cfg.BuildCores);
  if FullName = 'tms smart setup options:alternate registry key:' then exit(Cfg.AlternateRegistryKey);
  if FullName = 'tms smart setup options:prevent sleep:' then exit(Cfg.PreventSleep);
  if FullName = 'tms smart setup options:versions to keep:' then exit(Cfg.MaxVersionsPerProduct);
  if FullName = 'tms smart setup options:error if skipped:' then exit(Cfg.ErrorIfSkipped);

  if FullName = 'tms smart setup options:excluded products:' then exit(GetIncludedExcludedComponents(Cfg.GetExcludedComponents, true));
  if FullName = 'tms smart setup options:included products:' then exit(GetIncludedExcludedComponents(Cfg.GetIncludedComponents, false));

  if FullName = 'tms smart setup options:additional products folders:' then exit(GetAdditionalProductsFolders(Cfg.GetAdditionalProductsFolders));

  const ServersSection = 'tms smart setup options:servers:';
  if FullName = ServersSection then exit(TYamlValue.MakeObject);
  if FullName.StartsWith(ServersSection) then exit(GetServer(FullName.Substring(ServersSection.Length)));

  if FullName = 'tms smart setup options:git:' then exit(TYamlValue.MakeObject);
  if FullName = 'tms smart setup options:git:git location:' then exit(Cfg.GitConfig.GitCommand);
  if FullName = 'tms smart setup options:git:clone command:' then exit(Cfg.GitConfig.Clone);
  if FullName = 'tms smart setup options:git:pull command:' then exit(Cfg.GitConfig.Pull);
  if FullName = 'tms smart setup options:svn:' then exit(TYamlValue.MakeObject);
  if FullName = 'tms smart setup options:svn:svn location:' then exit(Cfg.SvnConfig.SvnCommand);
  if FullName = 'tms smart setup options:svn:checkout command:' then exit(Cfg.SvnConfig.Checkout);
  if FullName = 'tms smart setup options:svn:update command:' then exit(Cfg.SvnConfig.Update);
  if FullName = 'tms smart setup options:dcu megafolders:' then exit(GetDcuMegafolders(Cfg.DcuMegafolders));
  if FullName.StartsWith('tms smart setup options:dcu megafolders:') then exit(GetMegafolderValue(ArrayIndex));

  if FullName.StartsWith('configuration for ') then exit(OnProductMember(GetProductId(FullName), FullName, ArrayIndex)); 

  raise Exception.Create('Unknown variable: ' + FullName);

end;

function TConfigWriter.GetVerbosity(const CfgProduct: TProductConfigDefinition): TYamlValue;
begin
  if not CfgProduct.IsGlobal and not CfgProduct.HasInt(ConfigKeys.Verbosity) then exit(TYamlValue.MakeNull);
  Result := 'info';
  var v := TVerbosity(CfgProduct.GetInt(ConfigKeys.Verbosity, 1));
  case v of
    TVerbosity.trace: Result := 'trace';
    TVerbosity.info: Result := 'info';
    TVerbosity.error: Result := 'error';
  end;
end;

function TConfigWriter.GetSkipRegister(const CfgProduct: TProductConfigDefinition): TYamlValue;
begin
  if not CfgProduct.IsGlobal and not CfgProduct.HasInt(ConfigKeys.SkipRegister) then exit(TYamlValue.MakeNull);
  var s := CfgProduct.GetString(ConfigKeys.SkipRegisterExt, 'false');
  if s = TSkipRegisteringOptionsExt_True then exit(true);
  if s = TSkipRegisteringOptionsExt_False then exit(false);
  if s ='' then if CfgProduct.IsGlobal then exit(false) else exit(TYamlValue.MakeNull);

  if not s.StartsWith('[') or not s.EndsWith(']') then raise Exception.Create('Invalid Skip Register: ' + s);
  var Items := s.Substring(1, s.Length - 2).Split([','], TStringSplitOptions.ExcludeEmpty);
  for var i := Low(Items) to High(Items) do Items[i] := Items[i].Trim;

  Result := TYamlValue.MakeArray(Items, FlowArray);
  
end;

function TConfigWriter.GetDelphiVersions(const CfgProduct: TProductConfigDefinition): TYamlValue;
begin
  var IDEs := CfgProduct.GetIDENames();

  var Items: TArray<TYamlValue>;
  for var IDE := Low(TIDEName) to High(TIDEName) do
  begin
    if IDE in IDEs then Items := Items + [IDEId[IDE]] else 
    begin
      if CfgProduct.IsGlobal then Items := Items + [TYamlValue.MakeEmpty(IDEId[IDE])];
    end;
  end;

  Result := TYamlValue.MakeArray(Items, BlockArray);
  
end;

function TConfigWriter.GetPlatforms(const CfgProduct: TProductConfigDefinition): TYamlValue;
begin
  var Platforms := CfgProduct.GetPlatforms();

  var Items: TArray<TYamlValue>;
  for var Platform := Low(TPlatform) to High(TPlatform) do
  begin
    if Platform in Platforms then Items := Items + [PlatformId[Platform]] else 
    begin
      if CfgProduct.IsGlobal then Items := Items + [TYamlValue.MakeEmpty(PlatformId[Platform])];
    end;
  end;

  Result := TYamlValue.MakeArray(Items, BlockArray);
  
end;

function TConfigWriter.GetDefines(const CfgProduct: TProductConfigDefinition): TYamlValue;
begin
  var Defines := CfgProduct.Defines;
  var YamlArray: TArray<TYamlValue> := nil;
  
  if (Defines.Count > 0) then
  begin
    var ArrResult := Defines.Keys.ToArray;
    TArray.Sort<string>(ArrResult);

    SetLength(YamlArray, Length(ArrResult));
    for var i := Low(ArrResult) to High(ArrResult) do
    begin
      if Defines[ArrResult[i]] then YamlArray[i] := ArrResult[i] else YamlArray[i] := TYamlValue.MakeObject([ArrResult[i]]);
    end;
  end
  else
  begin
    if CfgProduct.IsGlobal then
    YamlArray :=
    [
      TYamlValue.MakeEmpty('RTTI'),
      TYamlValue.MakeEmpty('USE_UNICODE')
    ]
    else YamlArray :=
    [
      TYamlValue.MakeEmpty('RTTI: false')
    ];
  end;
  
  Result := TYamlValue.MakeArray(YamlArray, BlockArray);
end;

function TConfigWriter.GetDefine(const CfgProduct: TProductConfigDefinition; const Define: string): TYamlValue;
begin
  var Defines := CfgProduct.Defines;
  Result := Defines[Define];
end;

function TConfigWriter.GetProductBool(const CfgProduct: TProductConfigDefinition; const Key: string; const KeyDefault: boolean): TYamlValue;
begin
  if not CfgProduct.IsGlobal and not CfgProduct.HasBool(Key) then exit(TYamlValue.MakeNull);
  Result := CfgProduct.GetBool(Key, KeyDefault);
end;

function TConfigWriter.AddExampleCompilerPaths(const CfgProduct: TProductConfigDefinition): boolean;
begin
  if not CfgProduct.IsGlobal then exit(false);

  for var i := Low(TIDEName) to High(TIDEName) do
  begin
    if CfgProduct.GetString(ConfigKeys.CompilerPath + IDEId[i], '') <> '' then exit(false);
  end;
  Result := true;
end;

function TConfigWriter.GetLastEntry(const FullName: string): string;
begin
  var idx := FullName.LastIndexOf(':', FullName.Length - 2);
  Result := FullName.Substring(idx + 1, FullName.Length - idx - 2);
end;

function TConfigWriter.GetCompilerPaths(const CfgProduct: TProductConfigDefinition; const IdeName: string): TYamlValue;
begin
  var CompilerPath := CfgProduct.GetString(ConfigKeys.CompilerPath +  IdeName.Substring(0, IdeName.Length - 1), '');
  if CompilerPath <> '' then exit (CompilerPath);

  if AddExampleCompilerPaths(CfgProduct) then
  begin
    if IdeName = 'lazarus:' then exit(TYamlValue.MakeEmpty('c:\fpc\lazarus'));
    if IdeName = IDEId[High(TIDEName)] + ':' then exit(TYamlValue.MakeEmpty('c:\dev\Embarcadero\Studio\' + DelphiProductVersion[High(TIDEName)]));
  end;

  Result := TYamlValue.MakeNull;
end;


function TConfigWriter.OnProductMember(const ProductId: string; const FullName: string; const ArrayIndex: integer): TYamlValue;
begin
  var CfgStart := 'configuration for ' + ProductId + ':'; 
  var CfgProduct := GetProductCfg(ProductId);
  if FullName =  CfgStart then exit(TYamlValue.MakeObject);

  if FullName = CfgStart + 'options:' then exit(TYamlValue.MakeObject);
  if FullName = CfgStart + 'options:verbosity:' then exit(GetVerbosity(CfgProduct));
  if FullName = CfgStart + 'options:skip register:' then exit(GetSkipRegister(CfgProduct));
  if FullName = CfgStart + 'options:dry run:' then exit(GetProductBool(CfgProduct, ConfigKeys.DryRun, false));

  if FullName = CfgStart + 'delphi versions:' then exit(GetDelphiVersions(CfgProduct));
  if FullName = CfgStart + 'platforms:' then exit(GetPlatforms(CfgProduct));

  if FullName = CfgStart + 'compilation options:' then exit(TYamlValue.MakeObject);
  if FullName = CfgStart + 'compilation options:debug dcus:' then exit(GetProductBool(CfgProduct, ConfigKeys.DebugDcus, true));
  if FullName = CfgStart + 'compilation options:defines:' then exit(GetDefines(CfgProduct));
  if FullName.StartsWith(CfgStart + 'compilation options:defines:') then exit(GetDefine(CfgProduct, GetLastEntry(FullName)));


  if FullName = CfgStart + 'advanced options:' then exit(TYamlValue.MakeObject);
  if FullName = CfgStart + 'advanced options:use symlinks:' then exit(GetProductBool(CfgProduct, ConfigKeys.SymLinks, false));
  if FullName = CfgStart + 'advanced options:keep parallel folders:' then exit(GetProductBool(CfgProduct, ConfigKeys.KeepParallelFolders, false));
  if FullName = CfgStart + 'advanced options:modify sources:' then exit(GetProductBool(CfgProduct, ConfigKeys.ModifySources, false));
  if FullName = CfgStart + 'advanced options:partial builds:' then exit(GetProductBool(CfgProduct, ConfigKeys.PartialBuilds, false));
  if FullName = CfgStart + 'advanced options:add source code to library path:' then exit(GetProductBool(CfgProduct, ConfigKeys.AddSourceCodeToLibraryPath, false));

  var CompilerPaths := CfgStart + 'compiler paths:';
  if FullName = CompilerPaths then exit(TYamlValue.MakeObject);  
  if FullName.StartsWith(CompilerPaths) then exit(GetCompilerPaths(CfgProduct, FullName.Substring(CompilerPaths.Length)));

  raise Exception.Create('Invalid tag for product configuration: ' + FullName);
end;

function GetData(const Stream: TStream): string;
begin
  var Buffer: TBytes;
  SetLength(Buffer, Stream.Size);
  Stream.Position := 0;
  Stream.ReadBuffer(Buffer, Stream.Size);
  Result := TEncoding.UTF8.GetString(Buffer);
end;

procedure TConfigWriter.Save(const aCfg: TConfigDefinition; const FileName: string);
begin
  Cfg := aCfg;
  var SchemaStream := TResourceStream.Create(HInstance, 'TmsConfigSchema', RT_RCDATA);
  try
    var BBWriter := TBBYamlWriter.Create(true, false);
    try
      BBWriter.OnMember := OnMember;
      BBWriter.OnComment := OnComment;
      BBWriter.GetPatternMembers := GetPatternMembers;      
      var TextWriter := TStreamWriter.Create(FileName, false, TUTF8NoBOMEncoding.Instance);
      try
        var Schema := TJSONObject.ParseJSONValue(GetData(SchemaStream), true, true) as TJSONObject;
        try
          BBWriter.Save(TextWriter, Schema,
            'https://raw.githubusercontent.com/tmssoftware/smartsetup/refs/heads/dev/tms/example-config/tms.config.schema.json',
            'TMS Smart Setup configuration file'#10'Modify settings as needed.');
        finally
          Schema.Free;
        end;
      finally
        TextWriter.Free;
      end;
    finally
      BBWriter.Free;
    end;

  finally
    SchemaStream.Free;
  end;
end;

end.

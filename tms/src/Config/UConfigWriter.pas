unit UConfigWriter;
{$i ../../tmssetup.inc}

interface
uses Classes, SysUtils, Util.Replacer, UConfigDefinition, Generics.Defaults,
     Generics.Collections, ULogger, Deget.CoreTypes, Megafolders.Definition, BBArrays, BBClasses;
type
TConfigWriter = class
  private
    CmdFormat: boolean;

    function ReplaceGlobalVariables(const Cfg: TConfigDefinition; const GlobalTemplate, ProductTemplate: string): string;
    function ReplaceProductVariables(const ProductCfg: TProductConfigDefinition; const ProductTemplate: string): string;
    function ReplaceAllProductVariables(const Products: TProductConfigDefinitionDictionary; const ProductTemplate: string): string;
    function GetAdditionalProductsFolders(
      const Values: TEnumerable<string>): TArray<string>;
    function GetIncludedExcludedComponents(
      const Values: TEnumerable<string>): TArray<string>;
    function GetVerbosity(const v: TVerbosity): string;
    function GetSkipRegistering(const v: TSkipRegisteringSet): string;
    function GetDelphiVersions(const IDEs: TIDENameSet): string;
    function GetPlatforms(const Platforms: TPlatformSet): string;
    function GetDefines(const Values: TDictionary<string, boolean>; const IsAllProducts: boolean): TArray<string>;
    function GetHeaderComment(const ProductId: string): string;
    function GetCompilerPaths(
      const ProductCfg: TProductConfigDefinition): string;
    function HasOptions(const ProductCfg: TProductConfigDefinition): boolean;
    function HasDelphiVersions(
      const ProductCfg: TProductConfigDefinition): boolean;
    function HasPlatforms(const ProductCfg: TProductConfigDefinition): boolean;
    function HasCompilationOptions(
      const ProductCfg: TProductConfigDefinition): boolean;
    function HasAdvancedOptions(
      const ProductCfg: TProductConfigDefinition): boolean;
    function HasCompilerPaths(
      const ProductCfg: TProductConfigDefinition): boolean;
    function GetArray(const Items: array of string;
      const Level: integer; const Separator: string = '- '): string;
    function GetCmdArray(const Items: array of string): string;
    function GetCompilerPath(const ProductCfg: TProductConfigDefinition;
      const ProductId: string): string;
    function CommentBlock(const s: string): string;
    function GetServers(const Servers: TServerConfigList): string;
    function GetDcuMegafolders(const Values: TMegafolderList): TArray<string>;
    function GetHeaderVarName(const Header: string): string;
    function GetHeaderContent(const Header: string;
      const ArrayPrefix: TArrayOverrideBehavior): string;
  public
    constructor Create(const aCmdFormat: boolean);
    function ReplaceVariables(const Cfg: TConfigDefinition; const GlobalTemplate, ProductTemplate: string): string;
    procedure Save(const Cfg: TConfigDefinition; const GlobalTemplate, ProductTemplate: TStream; const FileName: string);
end;

implementation
uses UConfigKeys, IOUtils, UTmsBuildSystemUtils;
const
  NewLine = #13#10;

function BoolToStrLower(const b: boolean): string;
begin
  if b then exit ('true');
  exit ('false');
end;

function ExampleString(const s, example: string): string;
begin
  if String.IsNullOrWhiteSpace(s) then exit(example);
  exit(s);

end;

function GetComment(const s: string): string;
begin
  if String.IsNullOrWhiteSpace(s) then exit('#');
  exit('');

end;

function Quote(const s: string): string;
begin
  if s.IndexOfAny(['*', '%']) < 0 then exit(s);
  Result := '''' + s.Replace('''', '''''') + '''';
end;

function TConfigWriter.GetCmdArray(const Items: Array of string): string;
begin
  Result := '';
  for var Item in Items do
  begin
    var TrimItem := Item.Trim;
    if TrimItem = '' then continue;
    if TrimItem.StartsWith('#') then continue;
    if Result <> '' then Result := Result + ',';
    Result := Result + TrimItem;
  end;

  Result := '[' + Result + ']';
end;

function TConfigWriter.GetArray(const Items: Array of string; const Level: integer; const Separator: string): string;
begin
  if CmdFormat then exit(GetCmdArray(Items));

  Result := '';
  var Indent := StringOfChar(' ', Level * 2);

  for var Item in Items do
  begin
    var TrimItem := Item.Trim;
    if TrimItem = '' then continue;

    if Result <> '' then Result := Result + NewLine;

    if TrimItem.StartsWith('#') then Result := Result + Indent + '#' + Separator + TrimItem.Substring(1)
    else Result := Result + Indent + Separator + TrimItem;

  end;
end;


function TConfigWriter.GetIncludedExcludedComponents(const Values: TEnumerable<string>): TArray<string>;
begin
  Result := Values.ToArray;
  if Length(Result) > 0 then
  begin
    TArray.Sort<string>(Result);
    exit;
  end;

  Result :=
  [
    '#tms.example1',
    '#tms.example2'
  ];

end;

constructor TConfigWriter.Create(const aCmdFormat: boolean);
begin
  CmdFormat := aCmdFormat;
end;

function TConfigWriter.GetAdditionalProductsFolders(const Values: TEnumerable<string>): TArray<string>;
begin
  Result := Values.ToArray;
  if Length(Result) > 0 then
  begin
    TArray.Sort<string>(Result);
    exit;
  end;

  Result :=
  [
    '#c:\MyProducts   #All folders containing tmsbuild.yaml files inside c:\MyProducts will be added to the build.',
    '#..\..\Other products  #Try to use relative folders if possible.'
  ];

end;

function TConfigWriter.GetDcuMegafolders(const Values: TMegafolderList): TArray<string>;
begin
  Result := nil;
  SetLength(Result, Values.Count);
  for var i := 0 to Values.Count - 1 do
  begin
    Result[i] := Values[i].Folder + ': ' + Quote(Values[i].Mask);
  end;

  if Values.Count > 0 then exit;

  Result :=
  [
    '#none: ''tms.flexcel.vcl''   # FlexCel VCL has over 5000 units, it is not worth putting it into a megafolder',
    '#tms: ''tms.*''   # All other products matching tms.* except FlexCel go to the tms folder',
    '#none: ''biglib.*'' # All "none" entries won''t use megafolders. Use none for big libraries.',
    '#other: ''*'' #all products that didn''t match our previous rules go into other.'
  ];

end;


function TConfigWriter.GetServers(const Servers: TServerConfigList): string;
begin
  Result := '';
  for var i := 0 to Servers.ServerCount - 1 do
  begin
    var Server := Servers.GetServer(i);
    if i > 0 then  Result := Result + NewLine + NewLine;
    
    Result := Result + '    ' + Server.Name + ':' + NewLine;
    if not Server.IsReservedName then
    begin
      Result := Result + '      type: ' + Server.ServerTypeString + NewLine;
      Result := Result + '      url: ' + Server.Url + NewLine;
    end;
    Result := Result + '      enabled: ' + BoolToStrLower(Server.Enabled);
  end;

end;

function GetProduct(const Cfg: TConfigDefinition; const AProductId: string): TProductConfigDefinition;
begin
  var ProductId := aProductId;
  if ProductId = GlobalProductId.Replace(' ', '.') then ProductId := GlobalProductId;

  Result := Cfg.GetProduct(ProductId);
end;

function TConfigWriter.GetHeaderVarName(const Header: string): string;
begin
  Result := Header.Replace(' ', '-') + '-header';
end;

function TConfigWriter.GetHeaderContent(const Header: string; const ArrayPrefix: TArrayOverrideBehavior): string;
begin
  exit(TArrayOverrideBehavior_ToStringPrefix(ArrayPrefix) + Header);
end;

function TConfigWriter.ReplaceGlobalVariables(const Cfg: TConfigDefinition; const GlobalTemplate, ProductTemplate: string): string;
const
  ArrayPropertyHeaders: Array[TGlobalPrefixedProperties] of string =
    ('excluded products', 'included products', 'additional products folders',
     'servers', 'dcu megafolders');
begin
  Result := ParseString(GlobalTemplate, function(varName: string): string
    begin
      if varName = 'build-cores' then exit(IntToStr(Cfg.BuildCores));
      if varName = 'alternate-registry-key' then exit(Cfg.AlternateRegistryKey);
      if varName = 'prevent-sleep' then exit(BoolToStrLower(Cfg.PreventSleep));
      if varName = 'versions-to-keep' then exit(IntToStr(Cfg.MaxVersionsPerProduct));
      if varName = 'error-if-skipped' then exit(BoolToStrLower(Cfg.ErrorIfSkipped));
      if varName = 'excluded-products' then exit(GetArray(GetIncludedExcludedComponents(Cfg.GetExcludedComponents), 2));
      if varName = 'included-products' then exit(GetArray(GetIncludedExcludedComponents(Cfg.GetIncludedComponents), 2));
      if varName = 'additional-products-folders' then exit(GetArray(GetAdditionalProductsFolders(Cfg.GetAdditionalProductsFolders), 2));

      if varName = 'servers' then exit(GetServers(Cfg.ServerConfig));

      if varName = 'git-git-location' then exit(ExampleString(Quote(Cfg.GitConfig.GitCommand), 'c:\git\git.exe'));
      if varName = 'git-git-location-comment' then exit(GetComment(Cfg.GitConfig.GitCommand));
      if varName = 'git-clone-command' then exit(ExampleString(Quote(Cfg.GitConfig.Clone), 'clone'));
      if varName = 'git-clone-command-comment' then exit(GetComment(Cfg.GitConfig.Clone));
      if varName = 'git-pull-command' then exit(ExampleString(Quote(Cfg.GitConfig.Pull), 'pull'));
      if varName = 'git-pull-command-comment' then exit(GetComment(Cfg.GitConfig.Pull));

      if varName = 'svn-svn-location' then exit(ExampleString(Quote(Cfg.SvnConfig.SvnCommand), 'C:\fpc\fpcbootstrap\svn\bin\svn.exe'));
      if varName = 'svn-svn-location-comment' then exit(GetComment(Cfg.SvnConfig.SvnCommand));
      if varName = 'svn-checkout-command' then exit(ExampleString(Quote(Cfg.SvnConfig.Checkout), 'checkout'));
      if varName = 'svn-checkout-command-comment' then exit(GetComment(Cfg.SvnConfig.Checkout));
      if varName = 'svn-update-command' then exit(ExampleString(Quote(Cfg.SvnConfig.Update), 'update'));
      if varName = 'svn-update-command-comment' then exit(GetComment(Cfg.SvnConfig.Update));

      if varName = 'dcu-megafolders' then exit(GetArray(GetDcuMegafolders(Cfg.DcuMegafolders), 2));


      if varName = 'config-by-product' then exit(ReplaceAllProductVariables(Cfg.Products, ProductTemplate));
      if varName.StartsWith('config-for-product_') then exit(ReplaceProductVariables(GetProduct(Cfg, varName.Substring(varName.IndexOf('_') + 1)), ProductTemplate));

      for var ArrayProperty := Low(TGlobalPrefixedProperties) to High(TGlobalPrefixedProperties) do
      begin
        if varName = GetHeaderVarName(ArrayPropertyHeaders[ArrayProperty])  then exit(GetHeaderContent(ArrayPropertyHeaders[ArrayProperty], Cfg.PrefixedProperties[ArrayProperty]))
      end;


      raise Exception.Create('Unknown variable: ' + varName);
    end);
end;

function TConfigWriter.GetHeaderComment(const ProductId: string): string;
begin
  if ProductId = GlobalProductId then exit('# the settings in this section apply to all products. You might apply different settings to specific products below');
  exit('# the settings here override "all products" settings for product with id ' + ProductId + '.');
end;

function TConfigWriter.GetVerbosity(const v: TVerbosity): string;
begin
  Result := 'info';
  case v of
    TVerbosity.trace: Result := 'trace';
    TVerbosity.info: Result := 'info';
    TVerbosity.error: Result := 'error';
  end;
end;

function TConfigWriter.GetSkipRegistering(const v: TSkipRegisteringSet): string;
begin
  if v = [] then exit('false');

  Result := '';
  var HasAllOptions := true;
  var Sep := '';
  for var skip := Low(TSkipRegisteringOptions) to High(TSkipRegisteringOptions) do
  begin
    if skip in v then Result := Result + Sep + TSkipRegisteringName[skip]
    else HasAllOptions := false;
    Sep := ', ';
  end;

  if HasAllOptions then exit('true');
  Result := '[' + Result + ']';

end;

function TConfigWriter.GetDelphiVersions(const IDEs: TIDENameSet): string;
begin
  var Items: array[TIDEName] of string;
  for var IDE := Low(TIDEName) to High(TIDEName) do
  begin
    if IDE in IDEs then Items[IDE] := IDEId[IDE] else Items[IDE] := '#' + IDEId[IDE];
  end;
  Result := GetArray(Items, 2);
end;

function TConfigWriter.GetPlatforms(const Platforms: TPlatformSet): string;
begin
  var Items: array[TPlatform] of string;
  for var Platform := Low(TPlatform) to High(TPlatform) do
  begin
    if Platform in Platforms then Items[Platform] := PlatformId[Platform] else Items[Platform] := '#' + PlatformId[Platform];
  end;
  Result := GetArray(Items, 2);
end;


function TConfigWriter.GetDefines(const Values: TDictionary<string, boolean>; const IsAllProducts: boolean): TArray<string>;
begin
  if (Values.Count > 0) then
  begin
    Result := Values.Keys.ToArray;
    for var i := Low(Result) to High(Result) do
    begin
      if not Values[Result[i]] then Result[i] := Result[i] + ': false';
    end;
    TArray.Sort<string>(Result);

    exit;
  end;

  if IsAllProducts then
  Result :=
  [
    '#RTTI',
    '#USE_UNICODE'
  ]
  else Result :=
  [
    '#RTTI: false',
    '#USE_VCL'
  ];

end;

function TConfigWriter.GetCompilerPath(const ProductCfg: TProductConfigDefinition; const ProductId: string): string;
begin
  Result := ProductCfg.GetString(ConfigKeys.CompilerPath +  ProductId, '');
end;

function TConfigWriter.GetCompilerPaths(const ProductCfg: TProductConfigDefinition): string;
begin
  var Items: Array[TIDEName] of string;
  for var i := Low(Items) to High(Items) do
  begin
    var Comment := '';
    var CompilerPath := ProductCfg.GetString(ConfigKeys.CompilerPath +  IDEId[i], '');
    if CompilerPath = '' then
    begin
      Comment := '#';
      case i of
        lazarus: CompilerPath := 'C:\fpc\lazarus';
        delphi12: CompilerPath := 'C:\Program Files (x86)\Embarcadero\Studio\23.0';
        else continue;
      end;
    end;
    Items[i] := Comment + IDEId[i] + ': ' + CompilerPath;
  end;

  Result := GetArray(Items, 2, '');
end;

function TConfigWriter.HasOptions(const ProductCfg: TProductConfigDefinition): boolean;
begin
  if ProductCfg.ProductId = GlobalProductId then exit(true);
  Result :=
    ProductCfg.HasInt(ConfigKeys.Verbosity)
    or ProductCfg.HasInt(ConfigKeys.SkipRegister)
    or ProductCfg.HasBool(ConfigKeys.DryRun)
end;

function TConfigWriter.HasDelphiVersions(const ProductCfg: TProductConfigDefinition): boolean;
begin
  if ProductCfg.ProductId = GlobalProductId then exit(true);
  Result :=
    ProductCfg.GetIDENames <> [];
end;

function TConfigWriter.HasPlatforms(const ProductCfg: TProductConfigDefinition): boolean;
begin
  if ProductCfg.ProductId = GlobalProductId then exit(true);
  Result :=
    ProductCfg.GetPlatforms <> [];
end;

function TConfigWriter.HasCompilationOptions(const ProductCfg: TProductConfigDefinition): boolean;
begin
  if ProductCfg.ProductId = GlobalProductId then exit(true);
  Result :=
    ProductCfg.HasBool(ConfigKeys.DebugDcus)
    or (ProductCfg.Defines.Count > 0);
end;

function TConfigWriter.HasAdvancedOptions(const ProductCfg: TProductConfigDefinition): boolean;
begin
  if ProductCfg.ProductId = GlobalProductId then exit(true);
  Result :=
    ProductCfg.HasBool(ConfigKeys.SymLinks)
    or ProductCfg.HasBool(ConfigKeys.KeepParallelFolders)
    or ProductCfg.HasBool(ConfigKeys.ModifySources)
    or ProductCfg.HasBool(ConfigKeys.PartialBuilds)
    or ProductCfg.HasBool(ConfigKeys.AddSourceCodeToLibraryPath)
end;

function TConfigWriter.HasCompilerPaths(const ProductCfg: TProductConfigDefinition): boolean;
begin
  if ProductCfg.ProductId = GlobalProductId then exit(true);
  for var i := Low(TIDEName) to High(TIDEName) do
  begin
    if ProductCfg.HasString(ConfigKeys.CompilerPath) then exit(true);
  end;
  Result := false;
end;


function TConfigWriter.ReplaceProductVariables(
  const ProductCfg: TProductConfigDefinition; const ProductTemplate: string): string;
const
  ArrayPropertyHeaders: Array[TProductPrefixedProperties] of string =
    ('delphi versions', 'platforms', 'defines');

begin
  Result := ParseString(ProductTemplate, function(varName: string): string
    begin
      if varName = 'header-comment' then exit(GetHeaderComment(ProductCfg.ProductId));

      if varName = 'product-id' then exit(ProductCfg.ProductId);

      if varName = 'verbosity' then exit(GetVerbosity(TVerbosity(ProductCfg.GetInt(ConfigKeys.Verbosity, 1))));
      if varName = 'skip-register' then exit(GetSkipRegistering(TSkipRegisteringSet(Byte(ProductCfg.GetInt(ConfigKeys.SkipRegister, 0)))));
      if varName = 'dry-run' then exit(BoolToStrLower(ProductCfg.GetBool(ConfigKeys.DryRun, false)));
      if varName = 'has-options' then exit(BoolToStr(HasOptions(ProductCfg), true));


      if varName = 'delphi-versions' then exit(GetDelphiVersions(ProductCfg.GetIDENames()));
      if varName = 'has-delphi-versions' then exit(BoolToStr(HasDelphiVersions(ProductCfg), true));
      if varName = 'platforms' then exit(GetPlatforms(ProductCfg.GetPlatforms()));
      if varName = 'has-platforms' then exit(BoolToStr(HasPlatforms(ProductCfg), true));

      if varName = 'debug-dcus' then exit(BoolToStrLower(ProductCfg.GetBool(ConfigKeys.DebugDcus, true)));
      if varName = 'defines' then exit(GetArray(GetDefines(ProductCfg.Defines, ProductCfg.ProductId = GlobalProductId), 3));
      if varName = 'has-compilation-options' then exit(BoolToStr(HasCompilationOptions(ProductCfg), true));

      if varName = 'use-symlinks' then exit(BoolToStrLower(ProductCfg.GetBool(ConfigKeys.SymLinks, false)));
      if varName = 'keep-parallel-folders' then exit(BoolToStrLower(ProductCfg.GetBool(ConfigKeys.KeepParallelFolders, false)));
      if varName = 'modify-sources' then exit(BoolToStrLower(ProductCfg.GetBool(ConfigKeys.ModifySources, false)));
      if varName = 'partial-builds' then exit(BoolToStrLower(ProductCfg.GetBool(ConfigKeys.PartialBuilds, false)));
      if varName = 'add-source-code-to-library-path' then exit(BoolToStrLower(ProductCfg.GetBool(ConfigKeys.AddSourceCodeToLibraryPath, false)));
      if varName = 'has-advanced-options' then exit(BoolToStr(HasAdvancedOptions(ProductCfg), true));

      if varName = 'compiler-paths' then exit(GetCompilerPaths(ProductCfg));
      if varName.StartsWith('compiler-paths_') then exit(GetCompilerPath(ProductCfg, varName.Substring(varName.IndexOf('_') + 1)));

      if varName = 'has-compiler-paths' then exit(BoolToStr(HasCompilerPaths(ProductCfg), true));

      if varName = 'is-all-products' then exit(BoolToStr(ProductCfg.ProductId = GlobalProductId, true));
      if varName = 'is-not-all-products' then exit(BoolToStr(ProductCfg.ProductId <> GlobalProductId, true));

      for var ArrayProperty := Low(TProductPrefixedProperties) to High(TProductPrefixedProperties) do
      begin
        if varName = GetHeaderVarName(ArrayPropertyHeaders[ArrayProperty])  then exit(GetHeaderContent(ArrayPropertyHeaders[ArrayProperty], ProductCfg.PrefixedProperties[ArrayProperty]))
      end;


      raise Exception.Create('Unknown variable: ' + varName);
    end);

end;

function TConfigWriter.CommentBlock(const s: string): string;
begin
  Result := s;
  var offs := 0;
  var i := 0;
  while i < Length(s) do
  begin
    if s.Chars[i] = #10 then
    begin
      inc(i);
      while (i < Length(s)) and (s.Chars[i] = ' ') or (s.Chars[i] = #9) do inc(i);
      if (s.Chars[i] <> '#') and (s.Chars[i] <> #10) and (s.Chars[i] <> #13) and (i < Length(s)) then
      begin
        Result.Insert(offs + i, '#');
        inc(offs);
      end;
    end;

    inc(i);
  end;


end;

function TConfigWriter.ReplaceAllProductVariables(
  const Products: TProductConfigDefinitionDictionary; const ProductTemplate: string): string;
begin
  var SortedProducts := Products.Keys.ToArray;
  var Comparer: IComparer<string> := TDelegatedComparer<string>.Create(
  function(const Left, Right: string): integer
  begin
    if Left = Right then exit(0);
    if Left = GlobalProductId then exit(-1);
    if Right = GlobalProductId then exit(1);
    exit (Left.CompareTo(Right));
  end);

  TArray.Sort<string>(SortedProducts, Comparer);

  Result := '';
  for var SortedProduct in SortedProducts do
  begin
    Result := Result + ReplaceProductVariables(Products[SortedProduct], ProductTemplate);
  end;
  if Length(SortedProducts) <= 1 then
  begin
    var ExampleProduct := TProductConfigDefinition.Create('tms.example');
    ExampleProduct.SetIDEName(delphisydney, true);
    ExampleProduct.SetIDEName(delphi12, true);
    ExampleProduct.SetBool(ConfigKeys.DebugDcus, false);
    try
      Result := Result + CommentBlock(ReplaceProductVariables(ExampleProduct, ProductTemplate));
    finally
      ExampleProduct.Free;
    end;
  end;
end;


function TConfigWriter.ReplaceVariables(const Cfg: TConfigDefinition;
  const GlobalTemplate, ProductTemplate: string): string;
begin
  Result := ReplaceGlobalVariables(Cfg, GlobalTemplate, ProductTemplate);
end;

function GetData(const Stream: TStream): string;
begin
  var Buffer: TBytes;
  SetLength(Buffer, Stream.Size);
  Stream.Position := 0;
  Stream.ReadBuffer(Buffer, Stream.Size);
  Result := TEncoding.UTF8.GetString(Buffer);
end;

procedure TConfigWriter.Save(const Cfg: TConfigDefinition; const GlobalTemplate,
  ProductTemplate: TStream; const FileName: string);
begin
  TFile.WriteAllText(FileName, ReplaceVariables(Cfg, GetData(GlobalTemplate), GetData(ProductTemplate)), TUTF8NoBOMEncoding.Instance);
end;

end.

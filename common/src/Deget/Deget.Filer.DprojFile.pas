unit Deget.Filer.DprojFile;
{$SCOPEDENUMS ON}
{$R packages.res}

interface
{$IFNDEF MSWINDOWS}
uses
  Classes, Deget.CoreTypes;
{$ELSE}
uses
  Generics.Collections, System.SysUtils, System.Classes, System.Types, Xml.XMLIntf, System.Zip,
  WinApi.ActiveX,

  Deget.Nullable,
  Deget.CoreTypes, Deget.IDEInfo, Deget.Filer.Types;

  // Note: package update (writer) is not working fine with Delphi 2007
  // because the form is very different from other Delphis. For now it will be
  // let as-is because still not sure if it's worth it to implement specific code only for Delphi 2007
  // (during install the compilation will be fine because we use dcc32 so it will only
  // cause problems if user tries to manually compile the packages instead of using our tool)

type
  TCBuilderOutputMode = (None, All);
  TPathModifier = reference to function(NodeValue, NodeName: string; IsOutput, IsExe, IsBase: boolean): string;

  TDProjConditionConverter = record
  private
    class function InternalConvert(
      const PlatformAndConfig: TPlatformAndConfig;
      const AConditionAll, AConditionAllDebug, AConditionAllRelease, AConditionPlatAll, AconditionPlatDebug, AConditionPlatRelease: string): string; static;
  public
    class function Convert(const AIDEName: TIDEName; const DprojCondString: string): TPlatformAndConfig; overload; static;
    class function Convert(const PlatformAndConfig: TPlatformAndConfig): string; overload; static;
    class function ParentCondition(const PlatformAndConfig: TPlatformAndConfig): string; static;
  end;

  TBasePackageReadData = class
  protected
    FRcFiles: TRcCompileFiles;
    FDcrFiles: TIncludeFiles;
    FPasFiles: TPasIncludeFiles;
    FNoneFiles: TIncludeFiles;
    FDcpFiles: TIncludeFiles;

    FFrameworkType: string;
    FSupportedPlatforms: TPlatformSet;
    FDescription: string;
    FNamespaces: string;
    FUsage: TPackageUsage;
    FWin32Namespaces: string;
    FWin64Namespaces: string;
    FWin64xNamespaces: string;
    FProjectGuid: TGuid;
    FPackageName: string;
    FTargetedPlatforms: string;
    FCBuilderOutputMode: TCBuilderOutputMode;
    FPropertyGroups: TPropertyGroupEntryList;

  public
    constructor Create;
    destructor Destroy; override;

    property PackageName: string read FPackageName write FPackageName;
    property ProjectGuid: TGuid read FProjectGuid write FProjectGuid;

    property RcFiles: TRcCompileFiles read FRcFiles;
    property DcrFiles: TIncludeFiles read FDcrFiles;
    property PasFiles: TPasIncludeFiles read FPasFiles;
    property NoneFiles: TIncludeFiles read FNoneFiles;
    property DcpFiles: TIncludeFiles read FDcpFiles;

    property FrameworkType: string read FFrameworkType write FFrameworkType;
    property SupportedPlatforms: TPlatformSet read FSupportedPlatforms write FSupportedPlatforms;
    property Description: string read FDescription write FDescription;
    property Usage: TPackageUsage read FUsage write FUsage;
    property Namespaces: string read FNamespaces write FNamespaces;
    property Win32Namespaces: string read FWin32Namespaces write FWin32Namespaces;
    property Win64Namespaces: string read FWin64Namespaces write FWin64Namespaces;
    property Win64xNamespaces: string read FWin64xNamespaces write FWin64xNamespaces;
    property CBuilderOutputMode: TCBuilderOutputMode read FCBuilderOutputMode write FCBuilderOutputMode;
//    property ProjectImports: TList<TProjectImport> read FProjectImports;
    property PropertyGroups: TPropertyGroupEntryList read FPropertyGroups;
    property TargetedPlatforms: string read FTargetedPlatforms write FTargetedPlatforms;
  end;

  TPackageReadData = class(TBasePackageReadData)
  public
    constructor Create;
    destructor Destroy; override;


  end;

  TProjectWarnings = (Enabled, Disabled, AsErrors);
  TProjectHints = (Enabled, Disabled);
  TDebugInformation = (Full, None, Limited);

  TPackageWriteData = class(TPackageReadData)
  public
    const
      VclNamespaces = 'Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;System;Xml;Data;Datasnap;Web;Soap;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell';
  private
    FCustomLibSuffix: string;
    FEnableHints: TProjectHints;
    FEnableWarnings: TProjectWarnings;
    FDcpOutputDir: string;
    FHppOutputDir: string;
    FObjOutputDir: string;
    FDcuOutputDir: string;
    FExeOutputDir: string;
    FBpiOutputDir: string;
    FBplOutputDir: string;
    FXmlOutputDir: string;
    FBrccOutputDir: string;
    FExplicitRebuild: boolean;
    FDebugInformation: TDebugInformation;
    function GetLibSuffix: string;
  public
    constructor Create;
    destructor Destroy; override;
    property CustomLibSuffix: string read FCustomLibSuffix write FCustomLibSuffix;
    property EnableWarnings: TProjectWarnings read FEnableWarnings write FEnableWarnings;
    property EnableHints: TProjectHints read FEnableHints write FEnableHints;
    property ExplicitRebuild: boolean read FExplicitRebuild write FExplicitRebuild;
    property LibSuffix: string read GetLibSuffix;
    property DebugInformation: TDebugInformation read FDebugInformation write FDebugInformation;

    property DcuOutputDir: string read FDcuOutputDir write FDcuOutputDir;
    property ExeOutputDir: string read FExeOutputDir write FExeOutputDir;
    property BplOutputDir: string read FBplOutputDir write FBplOutputDir;
    property DcpOutputDir: string read FDcpOutputDir write FDcpOutputDir;
    property BpiOutputDir: string read FBpiOutputDir write FBpiOutputDir;
    property HppOutputDir: string read FHppOutputDir write FHppOutputDir;
    property ObjOutputDir: string read FObjOutputDir write FObjOutputDir;
    property XmlOutputDir: string read FXmlOutputDir write FXmlOutputDir;
    property BrccOutputDir: string read FBrccOutputDir write FBrccOutputDir;
  end;

  TPropGroupInfo = class
    Node: IXmlNode;
    Condition: string;
    IsBase: boolean;
    IsWin32: boolean;
    IsWin64: boolean;
    IsWin64x: boolean;
    Is2007Debug: boolean; // quick and dirty
  end;

  TDprojFiler = class
  const
    BaseConfigCondition = '''$(Base)''!=''''';
    BaseWin64ConfigCondition = '''$(Base_Win64)''!=''''';
    BaseWin64xConfigCondition = '''$(Base_Win64x)''!=''''';
    BaseWin32ConfigCondition = '''$(Base_Win32)''!=''''';
  const
    D2007DebugConfigCondition = ' ''$(Configuration)|$(Platform)'' == ''Debug|AnyCPU'' ';
    D2007ReleaseConfigCondition = ' ''$(Configuration)|$(Platform)'' == ''Release|AnyCPU'' ';
  strict private
    FIDEName: TIDEName;
    FFileName: string;
    FIDEInfo: IDelphiIDEInfo;
    function GetXml: IXmlDocument;
  strict protected
    FXml: IXmlDocument;
    function FindNodeByAtt(const Node: IXMLNode; const SubNodeName, AttName, AttValue: string): IXMLNode;
    function GetNodeText(Node: IXmlNode): string;
    function ProjectNode: IXMLNode;
    function MainPropertyGroupNode: IXmlNode;
    function PropertyGroupNode(const Condition: string): IXmlNode;
    function BaseConfigNode: IXmlNode;
    function DelphiPersonalityNode: IXmlNode;
    function PlatformsNode: IXmlNode;
    function ItemGroupNode: IXmlNode;
    procedure IteratePropertyGroups(Func: TFunc<TPropGroupInfo, Boolean>);
  strict protected
    property Xml: IXmlDocument read GetXml;
    property IDEInfo: IDelphiIDEInfo read FIDEInfo;
    property IDEName: TIDEName read FIDEName;
  public
    class procedure GenerateEmptyFile(const TargetDprojFile: string; IDEName: TIDEName; DeleteExisting: Boolean);
  public
    constructor Create(const AFileName: string; AIDEName: TIDEName);
    destructor Destroy; override;
    property FileName: string read FFileName;
  end;

  TDprojWriter = class(TDprojFiler)
  strict private
    function NextDCCIncludeIndex: integer;
    function NextRcCompileIndex: integer;
    function NextNoneIncludeIndex: integer;
    procedure CreateOrSetNode(ParentNode: IXMLNode; const NodeName, NodeValue: string);
    procedure RemoveNode(ParentNode: IXMLNode; const NodeName: string);
    procedure RemoveNodes(Nodes: IXMLNodeList; const NodeName: string);
    procedure UpdateBaseConfigProperty(const Name: string; Value: string);
    procedure UpdateConfigProperties(const Name: string; const Action: TFunc<string, string>);
    procedure UpdateNamespacesNode(Parent: IXmlNode; Value: string);
    function CreatePropertyGroupCache(
      const Values: TPropertyGroupEntryList): TDictionary<string, TPropertyGroupEntry>;
    procedure CreateCfgNode(const LastPropertyGroup: IXMLNode; var LastPropertyIndex: integer; const Conf: TPlatformAndConfig);
  private
    function IsModified: boolean;
  strict protected
    procedure WriteData(Data: TPackageWriteData); // remove this eventually
  public
    procedure UpdateProjectGuid(const Guid: TGuid);
    procedure UpdatePackageName(const PackageName: string);
    procedure UpdateFrameworkType(const FrameworkType: string);
    procedure UpdateDescription(const Description: string);
    procedure UpdateUsage(const Usage: TPackageUsage);
    procedure UpdateDllSuffix(const Value: string);
    procedure UpdateUnitScopeNames(const Value: string);
    procedure UpdateWin32UnitScopeNames(const Value: string);
    procedure UpdateWin64UnitScopeNames(const Value: string);
    procedure UpdateWin64xUnitScopeNames(const Value: string);

    procedure UpdateExeOutput(const Value: string);
    procedure UpdateDcuOutput(const Value: string);
    procedure UpdateObjOutput(const Value: string);
    procedure UpdateDccObjPath(const Action: TFunc<string, string>);
    procedure UpdateBplOutput(const Value: string);
    procedure UpdateBpiOutput(const Value: string);
    procedure UpdateDcpOutput(const Value: string);
    procedure UpdateHppOutput(const Value: string);
    procedure UpdateXmlOutput(const Value: string);
    procedure UpdateBrccOutput(const Value: string);

    procedure UpdateDCCUnitSearchPath(
      const Values: TPropertyGroupEntryList);


    procedure UpdateCBuilderOutput(const Value: TCBuilderOutputMode);
    procedure UpdateDebugInformation(const Value: TDebugInformation);

    procedure UpdateWarnings(const Value: TProjectWarnings);
    procedure UpdateHints(const Value: TProjectHints);

    procedure UpdateSupportedPlatforms(Platforms: TPlatformSet);

    // ExplicitRebuild = true (explicit rebuild, obviously)
    // ExplicitRebuild = false (rebuild as needed)
    procedure UpdateExplicitRebuild(ExplicitRebuild: boolean);

    procedure RemoveExcludedPackages;
    procedure RemoveDCCIncludes;
    procedure RemoveRcCompiles;
    procedure RemoveNoneIncludes;
    procedure AddDCCInclude(const IncludeName: string; const FormName: string = '');
    procedure AddRcCompile(const IncludeName: string; const FormName: string = '');
    procedure AddNoneInclude(const IncludeName: string; const FormName: string = '');
    procedure UpdateOutputXmlDocumentation(const Enable: Boolean);
  public
//    class procedure WritePackageData(Target: TDelphiPackage; Data: TPackageWriteData);
    procedure Flush(const OnlyIfModified: boolean = false);
  end;

  TDprojReader = class(TDprojFiler)
  public
    procedure ReadData(Data: TPackageReadData);
    class function PlatformIsSupported(const AllPlats: integer; const dp: TPlatform): boolean; static;
  end;

  TDprojModifier = class(TDProjFiler)
  private
    function IsSingleReplacement(const NodeName: string): boolean;
    function IsMultipleReplacement(const NodeName: string): boolean;
    procedure ProcessProject(const Project: IXMLNode; const SinglePathModifier,
      MultiplePathModifier: TPathModifier);
    procedure ProcessItemGroup(const ItemGroup: IXMLNode; const SinglePathModifier: TPathModifier);
    procedure ProcessPropertyGroup(const PropertyGroup: IXMLNode;
      const SinglePathModifier, MultiplePathModifier: TPathModifier);
    function IsFileItem(const NodeName: string): boolean;
    procedure LoopOverAllChildNodes(const Node: IXMLNode;
      const RootPath: string; const ParentIsBase: boolean;
      const NodeModifier: TFunc<string, string, boolean, string>; const BasePropertyGroup: TProc<IInterface>);
    function IsBase(const NodeName: string; const v: OLEVariant): boolean;
    function FindNodeRecursive(const Node: IXmlNode;
      const Sections: TArray<string>; const index: integer): IXmlNode;
    function FindBaseProjectNode: IXMLNode;
  public
    function IsOutput(const NodeName: string): boolean;
    function IsExe(const NodeName: string): boolean;
    function ISCPPOnlyNode(const NodeName: string): boolean;
  public
    const
      AllOutputNodes: array[0..9] of string = (
        'DCC_BplOutput',
        'DCC_DcpOutput',
        'DCC_ExeOutput',
        'DCC_DcuOutput',
        'BRCC_OutputDir',
        'DCC_BpiOutput',
        'DCC_HppOutput',
        'DCC_ObjOutput',
        //Cpp
        'FinalOutputDir',
        'IntermediateOutputDir'
        );

    constructor Create(const ASourceFileName: string;
      const AIDEName: TIDEName);

    procedure Save(const DestFileName: string);

    procedure AdaptAllPaths(const SinglePathModifier, MultiplePathModifier: TPathModifier);
    procedure LoopOverAllNodes(const NodeModifier: TFunc<string, string, boolean, string>; const BasePropertyGroup: TProc<IInterface>);
    procedure AddChildNode(const Parent: IInterface; const Name,
      Value: string);
    procedure AddBaseProjectNode(const Name,
      Value: string);
    procedure SetAttIfExists(const Path: string; const AttName: string; const AttValue: string);
  end;

  function GetNullableNodeValue(const Node: IXMLNode): Nullable<string>;

{$ENDIF}
  function IntegerToPlatforms(Value: Integer): TPlatformSet;
  function PlatformsToInteger(Platforms: TPlatformSet): Integer;

implementation
uses
  System.IOUtils, Xml.XMLDoc, System.Variants, Deget.DelphiInfo, Deget.IDETypes, UTmsBuildSystemUtils;

const
  XmlBoolStr: array[boolean] of string = ('false', 'true');
  PlatformMap: array[TPlatform] of Integer = (pidWin32, pidWin64, pidOSX32, pidOSX64, pidOSXArm64,
    pidiOSSimulator32, pidiOSDevice32, pidiOSDevice64, pidAndroidArm32, pidAndroidArm64, pidLinux64,
    pidiOSSimulatorArm64, pidWin64x);

function IntegerToPlatforms(Value: Integer): TPlatformSet;
begin
  Result := [];
  for var PlatType := Low(TPlatform) to High(TPlatform) do
    if (Value and PlatformMap[PlatType]) <> 0 then
      Include(Result, PlatType);

end;

function PlatformsToInteger(Platforms: TPlatformSet): Integer;
begin
  Result := 0;
  for var PlatType in Platforms do
    Result := Result or PlatformMap[PlatType];
end;

{$IFDEF MSWINDOWS}

{ TPackageWriter }

function TDprojFiler.BaseConfigNode: IXmlNode;
begin
  Result := PropertyGroupNode(BaseConfigCondition);
end;

constructor TDprojFiler.Create(const AFileName: string; AIDEName: TIDEName);
begin
  inherited Create;
  FFileName := AFileName;
  FIDEName := AIDEName;
  FIDEInfo := TDelphiIDEInfo.Create(AIDEName);
  CoInitialize(nil);
end;

function TDprojFiler.DelphiPersonalityNode: IXmlNode;
begin
  case IDEName of
    delphi2007:
      Result := ProjectNode
        .ChildNodes.FindNode('ProjectExtensions')
          .ChildNodes.FindNode('BorlandProject')
            .ChildNodes.FindNode('BorlandProject')
              .ChildNodes.FindNode('Delphi.Personality');
  else
    Result := ProjectNode
      .ChildNodes.FindNode('ProjectExtensions')
        .ChildNodes.FindNode('BorlandProject')
          .ChildNodes.FindNode('Delphi.Personality');
  end;
end;

destructor TDprojFiler.Destroy;
begin
  FXml := nil;
  CoUninitialize;
  inherited;
end;

function TDprojFiler.FindNodeByAtt(const Node: IXMLNode; const SubNodeName,
  AttName, AttValue: string): IXMLNode;
var
  i: integer;
  n: IXMLNode;
begin
  if Node = nil then Exit(nil);
  for i := 0 to Node.ChildNodes.Count - 1 do
  begin
    n := Node.ChildNodes[i];
    if (n.NodeName = SubNodeName) and (n.Attributes[AttName] = AttValue) then
      Exit(n);
  end;
  Result := nil;
end;

class procedure TDprojFiler.GenerateEmptyFile(const TargetDprojFile: string; IDEName: TIDEName; DeleteExisting: Boolean);
var
  EmptyDProjFile: TBytes;
  Writer: TDprojWriter;
begin
  CoInitializeEx(nil, COINIT_MULTITHREADED);
  try
    var PackageName := TPath.GetFileNameWithoutExtension(TargetDprojFile);
    var TargetFolder := TPath.GetDirectoryName(TargetDprojFile);

    // Load default empty files
    begin
      var Res := TResourceStream.Create(HInstance, 'EMPTY_PACKAGES_ZIP', RT_RCDATA);
      try
        var Zip := TZipFile.Create;
        try
          Zip.Open(Res, zmRead);
          Zip.Read(Format('%s/Empty.dproj', [IDEId[IDEName]]), EmptyDProjFile);
        finally
          Zip.Free;
        end;
      finally
        Res.Free;
      end;
    end;

    if TFile.Exists(TargetDProjFile) then
      if not DeleteExisting then
        raise Exception.CreateFmt('Cannot create package "%s" in folder "%s": Package already exists', [PackageName, TargetFolder]);

    TDirectory_CreateDirectory(TargetFolder);

    // Create based on an empty package
    TFile.WriteAllBytes(TargetDprojFile, EmptyDprojFile);

    // Now replace info in the package that we can already do
    Writer := TDprojWriter.Create(TargetDProjFile, IDEName);
    try
      Writer.UpdateProjectGuid(TGuid.NewGuid);
      Writer.UpdatePackageName(PackageName);
      Writer.Flush;
    finally
      Writer.Free;
    end;
  finally
    CoUninitialize;
  end;
end;

function TDprojFiler.GetNodeText(Node: IXmlNode): string;
begin
  if Node <> nil then
    Result := Node.Text
  else
    Result := '';
end;

function TDprojFiler.GetXml: IXmlDocument;
begin
  if FXml = nil then
  begin
    FXml := TXMLDocument.Create(nil);
    FXml.Options := Xml.Options + [doNodeAutoIndent];
    try
      FXml.LoadFromFile(FileName);
    except on ex: Exception do
      begin
        var process := FindProcessUsing(FileName);
        if process <> '' then process := ' (locked by: "' + process + '")';

        raise Exception.Create(ex.Message + ' File: ' + FileName + process);
      end;
    end;
 end;
  Result := FXml;
end;

function TDprojFiler.ItemGroupNode: IXmlNode;
begin
  Result := ProjectNode.ChildNodes.FindNode('ItemGroup');
end;

procedure TDprojFiler.IteratePropertyGroups(Func: TFunc<TPropGroupInfo, Boolean>);
var
  Node: IXmlNode;
  Info: TPropGroupInfo;
begin
  Node := ProjectNode.ChildNodes.First;
  while Node <> nil do
  begin
    if (Node.NodeName = 'PropertyGroup') and (Node.Attributes['Condition'] <> null) then
    begin
      Info := TPropGroupInfo.Create;
      try
        Info.Node := Node;
        Info.Condition := Node.Attributes['Condition'];
        if IDEName >= TIDEName.delphi2009 then
          Info.IsBase := Info.Condition = BaseConfigCondition
        else
          Info.IsBase := (Info.Condition = D2007DebugConfigCondition) or (Info.Condition = D2007ReleaseConfigCondition);
        Info.Is2007Debug := (IDEName <= TIDEName.delphi2009) and (Info.Condition = D2007DebugConfigCondition);
        Info.IsWin32 := Info.Condition = BaseWin32ConfigCondition;
        Info.IsWin64 := (Info.Condition = BaseWin64ConfigCondition);
        Info.IsWin64x := (Info.Condition = BaseWin64xConfigCondition);
        if Func(Info) then Exit;
      finally
        Info.Free;
      end;
    end;
    Node := Node.NextSibling;
  end;
end;

function TDprojFiler.MainPropertyGroupNode: IXmlNode;
begin
  Result := ProjectNode.ChildNodes.First;
end;

function TDprojFiler.PlatformsNode: IXmlNode;
begin
  Result := ProjectNode
    .ChildNodes.FindNode('ProjectExtensions')
      .ChildNodes.FindNode('BorlandProject')
        .ChildNodes.FindNode('Platforms');
end;

function TDprojFiler.ProjectNode: IXMLNode;
begin
  Result := Xml.ChildNodes.FindNode('Project');
end;

function TDprojFiler.PropertyGroupNode(const Condition: string): IXmlNode;
begin
  Result := FindNodeByAtt(ProjectNode, 'PropertyGroup', 'Condition', Condition);
end;

{ TPackageWriter }

procedure TDprojWriter.UpdateProjectGuid(const Guid: TGuid);
begin
  MainPropertyGroupNode.ChildNodes.FindNode('ProjectGuid').NodeValue := Guid.ToString;
end;

procedure TDprojWriter.UpdateSupportedPlatforms(Platforms: TPlatformSet);
var
  Plats: integer;
  PlatType: TPlatform;
  Node, N2: IXmlNode;
begin
  Platforms := Platforms * PlatformsInDelphi[IDEName];

  Plats := PlatformsToInteger(Platforms);

  // Update TargetedPlatforms node
  Node := MainPropertyGroupNode.ChildNodes.FindNode('TargetedPlatforms');
  if Node <> nil then
    Node.NodeValue := Plats;

  // Update Platforms node
  if PlatformsNode <> nil then
    for PlatType := Low(TPlatform) to High(TPlatform) do
    begin
      N2 := FindNodeByAtt(PlatformsNode, 'Platform', 'value', IDEInfo.GetPlatform(PlatType).BuildName);
      if PlatType in Platforms then
      begin
        // enable platform
        if N2 = nil then
        begin
          N2 := PlatformsNode.AddChild('Platform');
          N2.Attributes['value'] := IDEInfo.GetPlatform(PlatType).BuildName;
        end;
        N2.NodeValue := 'True'
      end
      else
      begin
        // disable platform
        if N2 <> nil then
          N2.NodeValue := 'False';
      end;
    end;
end;

procedure TDprojWriter.UpdateUnitScopeNames(const Value: string);
begin
  if (IDEName < TIDEName.delphixe2) then
    raise Exception.Create('Cannot set unit scope names for Delphi versions lower than XE2');

//  UpdateBaseConfigProperty('DCC_Namespace', Value + ';$(DCC_Namespace)');
  IteratePropertyGroups(function(Info: TPropGroupInfo): Boolean
    begin
      Result := False;
      if Info.IsBase then
      begin
        UpdateNamespacesNode(Info.Node, Value);
        Result := True;
      end;
    end
  );
end;

procedure TDprojWriter.UpdateUsage(const Usage: TPackageUsage);
begin
  IteratePropertyGroups(function(Info: TPropGroupInfo): Boolean
    begin
      Result := False;
      case Usage of
        TPackageUsage.Runtime:
          begin
            if Info.IsBase then
              CreateOrSetNode(Info.Node, 'RuntimeOnlyPackage', 'true')
            else
              RemoveNode(Info.Node, 'RuntimeOnlyPackage');
            RemoveNode(Info.Node, 'DesignOnlyPackage');
          end;
        TPackageUsage.DesignTime:
          begin
            if Info.IsBase then
              CreateOrSetNode(Info.Node, 'DesignOnlyPackage', 'true')
            else
              RemoveNode(Info.Node, 'DesignOnlyPackage');
            RemoveNode(Info.Node, 'RuntimeOnlyPackage');
          end;
        TPackageUsage.RuntimeAndDesignTime:
          begin
            RemoveNode(Info.Node, 'RuntimeOnlyPackage');
            RemoveNode(Info.Node, 'DesignOnlyPackage');
          end;
      end;
    end
  );
end;

procedure TDprojWriter.UpdateWarnings(const Value: TProjectWarnings);
begin
  case Value of
    TProjectWarnings.Disabled:
      UpdateBaseConfigProperty('DCC_Warnings', 'false');
    TProjectWarnings.AsErrors:
      UpdateBaseConfigProperty('DCC_Warnings', 'error');
  else
    UpdateBaseConfigProperty('DCC_Warnings', '');
  end;
end;

procedure TDprojWriter.UpdateWin32UnitScopeNames(const Value: string);
begin
  if (IDEName < TIDEName.delphixe2) then
    raise Exception.Create('Cannot set unit scope names for Delphi versions lower than XE2');

  IteratePropertyGroups(function(Info: TPropGroupInfo): Boolean
    begin
      Result := False;
      if Info.IsWin32 then
      begin
        UpdateNamespacesNode(Info.Node, Value);
        Result := True;
      end;
    end
  );
end;

procedure TDprojWriter.UpdateWin64UnitScopeNames(const Value: string);
begin
  if (IDEName < TIDEName.delphixe2) then
    raise Exception.Create('Cannot set unit scope names for Delphi versions lower than XE2');

  IteratePropertyGroups(function(Info: TPropGroupInfo): Boolean
    begin
      Result := False;
      if Info.IsWin64 then
      begin
        UpdateNamespacesNode(Info.Node, Value);
        Result := True;
      end;
    end
  );
end;

procedure TDprojWriter.UpdateWin64xUnitScopeNames(const Value: string);
begin
  if (IDEName < TIDEName.delphi12) then
    exit;

  IteratePropertyGroups(function(Info: TPropGroupInfo): Boolean
    begin
      Result := False;
      if Info.IsWin64x then
      begin
        UpdateNamespacesNode(Info.Node, Value);
        Result := True;
      end;
    end
  );
end;

procedure TDprojWriter.WriteData(Data: TPackageWriteData);
var
  Plats: TPlatformSet;
begin
  // this was already done when package was originaly created so if existing package is working, we wouldn't need
  // to call this method to update package name again. But let's just repeat and keep it here just to make sure package will be ok
  UpdateProjectGuid(Data.ProjectGuid);
  UpdatePackageName(TPath.GetFileNameWithoutExtension(FileName));
  UpdateDescription(Data.Description);
  UpdateFrameworkType(Data.FrameworkType);
  UpdateExplicitRebuild(Data.ExplicitRebuild);
  UpdateCBuilderOutput(Data.CBuilderOutputMode);
  UpdateUnitScopeNames(Data.Namespaces);

  UpdateUsage(Data.Usage);

  UpdateSupportedPlatforms(Plats);

  UpdateDLLSuffix(Data.LibSuffix);
  UpdateWarnings(Data.EnableWarnings);
  UpdateHints(Data.EnableHints);

  // Output directories
  UpdateDcuOutput(Data.DcuOutputDir);
  UpdateExeOutput(Data.ExeOutputDir);
  UpdateBplOutput(Data.BplOutputDir);
  UpdateBpiOutput(Data.BpiOutputDir);
  UpdateDcpOutput(Data.DcpOutputDir);
  UpdateHppOutput(Data.HppOutputDir);
  UpdateObjOutput(Data.ObjOutputDir);
  UpdateXmlOutput(Data.XmlOutputDir);
  UpdateBrccOutput(Data.BrccOutputDir);

  // Package files
  RemoveRcCompiles;
  RemoveDCCIncludes;
  RemoveNoneIncludes;

  for var DCCInclude in Data.DcrFiles do
    AddDCCInclude(DCCInclude.FileName);

  for var DCCInclude in Data.DcpFiles do
    AddDCCInclude(DCCInclude.FileName);

  for var DCCInclude in Data.PasFiles do
    AddDCCInclude(DCCInclude.FileName, DCCInclude.FormName);

  for var DCCInclude in Data.RcFiles do
    AddRcCompile(DCCInclude.FileName, DCCInclude.FormName);

  for var NoneInclude in Data.NoneFiles do
    AddNoneInclude(NoneInclude.FileName);
end;

//class procedure TDprojWriter.WritePackageData(Target: TDelphiPackage; Data: TPackageWriteData);
//var
//  Writer: TDprojWriter;
//begin
//  Writer := TDprojWriter.Create(Target);
//  try
//    Writer.WriteData(Data);
//    Target.SaveDProj;
//  finally
//    Writer.Free;
//  end;
//end;

procedure TDprojWriter.RemoveDCCIncludes;
begin
  RemoveNodes(ItemGroupNode.ChildNodes, 'DCCReference');
end;

procedure TDprojWriter.AddDCCInclude(const IncludeName, FormName: string);
var
  IndexToInsert: integer;
  IncludeNode, FormNode: IXMLNode;
begin
  // This could be more optimized to allow adding various items at once,
  // but let's keep it simple and safe for now, not sure if we need optimization here
  IndexToInsert := NextDCCIncludeIndex;


  IncludeNode := ItemGroupNode.AddChild('DCCReference', IndexToInsert);
  IncludeNode.Attributes['Include'] := IncludeName;
  if FormName <> '' then
  begin
    FormNode := IncludeNode.AddChild('Form');
    FormNode.NodeValue := FormName;
  end;
end;

procedure TDprojWriter.AddNoneInclude(const IncludeName, FormName: string);
var
  IndexToInsert: integer;
  IncludeNode, FormNode: IXmlNode;
begin
  // This could be more optimized to allow adding various items at once,
  // but let's keep it simple and safe for now, not sure if we need optimization here
  IndexToInsert := NextNoneIncludeIndex;

  IncludeNode := ItemGroupNode.AddChild('None', IndexToInsert);
  IncludeNode.Attributes['Include'] := IncludeName;
  if FormName <> '' then
  begin
    FormNode := IncludeNode.AddChild('Form');
    FormNode.NodeValue := FormName;
  end;
end;

//procedure TDprojWriter.AddProjectImports(Imports: TEnumerable<TProjectImport>);
//var
//  OutNodes: IXmlNodeList;
//  I: integer;
//  Node: IXMLNode;
//  ProjPos: integer;
//  Import: TProjectImport;
//  NewNode: IXMLNode;
//begin
//  ProjPos := -1;
//  OutNodes := ProjectNode.ChildNodes;
//  for i := 0 to OutNodes.Count - 1 do
//  begin
//    if OutNodes[i].NodeName = 'Import' then
//      ProjPos := i;
//  end;
//
//  if ProjPos < 0 then
//    raise Exception.Create('Cannot add Project Imports. No <Import> node.');
//
//  for Import in Imports do
//  begin
//    Node := FindNodeByAtt(ProjectNode, 'Import', 'Project', Import.Project);
//    if Node = nil then
//    begin
//      NewNode := ProjectNode.AddChild('Import',  ProjPos + 1);
//      NewNode.Attributes['Project'] := Import.Project;
//      NewNode.Attributes['Condition'] := Import.Condition;
//    end;
//  end;
//end;

procedure TDprojWriter.AddRcCompile(const IncludeName, FormName: string);
var
  IndexToInsert: integer;
  IncludeNode, FormNode: IXmlNode;
begin
  // This could be more optimized to allow adding various items at once,
  // but let's keep it simple and safe for now, not sure if we need optimization here
  IndexToInsert := NextRcCompileIndex;

  IncludeNode := ItemGroupNode.AddChild('RcCompile', IndexToInsert);
  IncludeNode.Attributes['Include'] := IncludeName;
  if FormName <> '' then
  begin
    FormNode := IncludeNode.AddChild('Form');
    FormNode.NodeValue := FormName;
  end;
end;

procedure TDprojWriter.CreateOrSetNode(ParentNode: IXMLNode; const NodeName, NodeValue: string);
var
  Node: IXMLNode;
begin
  Node := ParentNode.ChildNodes.FindNode(NodeName);

  if Node = nil then
    Node := ParentNode.AddChild(NodeName);
  Node.NodeValue := NodeValue;
end;

function TDprojWriter.IsModified: boolean;
begin
  if not TFile.Exists(FileName) then exit(true);
  var OldText: IXmlDocument := TXMLDocument.Create(nil);
  OldText.ParseOptions :=[];
  OldText.Options := [];
  OldText.LoadFromFile(FileName);

  var NewText: IXmlDocument := TXMLDocument.Create(nil);
  NewText.ParseOptions :=[];
  NewText.Options := [];
  NewText.LoadFromXML(Xml.XML.Text);


  Result :=OldText.XML.Text <> NewText.Xml.Text;
end;

procedure TDprojWriter.Flush(const OnlyIfModified: boolean);
begin
  if FXml = nil then Exit;
  
  RemoveExcludedPackages;
  Xml.XML.Text := FormatXMLData(Xml.XML.Text);
  Xml.Active := true;
  try
    if not OnlyIfModified or IsModified then
      Xml.SaveToFile(FileName);
  except on ex: Exception do
    begin
      var process := FindProcessUsing(FileName);
      if process <> '' then process := ' (locked by: "' + process + '")';

        raise Exception.Create(ex.Message + ' File: ' + FileName + process);
    end;
  end;
end;

function TDprojWriter.NextDCCIncludeIndex: integer;
var
  Nodes: IXmlNodeList;
  I: integer;
begin
  // "default" value is as the second node (index 1),
  // right after the node <DelphiCompile> which is the first and single one
  Result := 1;

  // then try to find the last <DCCReference> node in <ItemGroup> (if any)
  // if found, the insert place will be right after that last dccreference
  Nodes := ItemGroupNode.ChildNodes;
  for I := 0 to Nodes.Count - 1 do
  begin
    if Nodes[I].LocalName = 'DCCReference' then
      Result := I + 1;
  end;
end;

function TDprojWriter.NextNoneIncludeIndex: integer;
var
  Nodes: IXmlNodeList;
  I: integer;
begin
  // "default" value is the index to insert next <DCCReference> node
  // (<None> nodes will come after <RcCompile> nodes
  Result := NextRcCompileIndex;

  // then try to find the last <None> node in <ItemGroup> (if any)
  // if found, the insert place will be right after that last RcInclude
  Nodes := ItemGroupNode.ChildNodes;
  for I := 0 to Nodes.Count - 1 do
  begin
    if Nodes[I].LocalName = 'None' then
      Result := I + 1;
  end;
end;

function TDprojWriter.NextRcCompileIndex: integer;
var
  Nodes: IXmlNodeList;
  I: integer;
begin
  // "default" value is the index to insert next <DCCReference> node
  // (<RcCompile> nodes will come after <DCCReference> nodes
  Result := NextDCCIncludeIndex;

  // then try to find the last <RcCompile> node in <ItemGroup> (if any)
  // if found, the insert place will be right after that last dccreference
  Nodes := ItemGroupNode.ChildNodes;
  for I := 0 to Nodes.Count - 1 do
  begin
    if Nodes[I].LocalName = 'RcCompile' then
      Result := I + 1;
  end;
end;

procedure TDprojWriter.RemoveExcludedPackages;
var
  Node: IXmlNode;
  ParentNode: IXmlNode;
begin
  ParentNode := DelphiPersonalityNode;
  while true do
  begin
    Node := ParentNode.ChildNodes.FindNode('Excluded_Packages');
    if (Node = nil) then
      break;
    ParentNode.ChildNodes.Remove(Node);
  end;
end;

procedure TDprojWriter.RemoveNode(ParentNode: IXMLNode;
  const NodeName: string);
var
  Node: IXMLNode;
begin
  Node := ParentNode.ChildNodes.FindNode(NodeName);
  if Node <> nil then
    ParentNode.ChildNodes.Remove(Node);
end;

procedure TDprojWriter.RemoveNodes(Nodes: IXMLNodeList; const NodeName: string);
var
  Node: IXMLNode;
begin
  if Nodes = nil then Exit;
  repeat
    Node := Nodes.FindNode(NodeName);
    if Node <> nil then
      Nodes.Remove(Node);
  until Node = nil;
end;

procedure TDprojWriter.RemoveNoneIncludes;
begin
  RemoveNodes(ItemGroupNode.ChildNodes, 'None');
end;

procedure TDprojWriter.RemoveRcCompiles;
begin
  RemoveNodes(ItemGroupNode.ChildNodes, 'RcCompile');
end;

procedure TDprojWriter.UpdateBaseConfigProperty(const Name: string; Value: string);
begin
  IteratePropertyGroups(function(Info: TPropGroupInfo): Boolean
    begin
      Result := False;
      if Info.IsBase and (Value <> '') then
      begin
        if IDEName = delphi2007 then
        begin
          if Info.Is2007Debug then
            Value := StringReplace(Value, '$(Config)', 'Debug', [rfReplaceAll, rfIgnoreCase])
          else
            Value := StringReplace(Value, '$(Config)', 'Release', [rfReplaceAll, rfIgnoreCase]);
        end;
        CreateOrSetNode(Info.Node, Name, Value)
      end
      else
        RemoveNode(Info.Node, Name);
    end
  );
end;

procedure TDprojWriter.UpdateConfigProperties(const Name: string; const Action: TFunc<string, string>);
begin
  IteratePropertyGroups(function(Info: TPropGroupInfo): Boolean
    begin
      Result := False;

      var Node := Info.Node.ChildNodes.FindNode(Name);
      if (Node <> nil) and (Node.NodeValue <> '') then
      begin
        var Value := Node.NodeValue;
        if IDEName = delphi2007 then
        begin
          if Info.Is2007Debug then
            Value := StringReplace(Value, '$(Config)', 'Debug', [rfReplaceAll, rfIgnoreCase])
          else
            Value := StringReplace(Value, '$(Config)', 'Release', [rfReplaceAll, rfIgnoreCase]);
        end;
        Node.NodeValue := Action(Value);
      end
    end
  );
end;

procedure TDprojWriter.UpdateBpiOutput(const Value: string);
begin
  UpdateBaseConfigProperty('DCC_BpiOutput', Value);
end;

procedure TDprojWriter.UpdateBplOutput(const Value: string);
begin
  UpdateBaseConfigProperty('DCC_BplOutput', Value);
end;

procedure TDprojWriter.UpdateBrccOutput(const Value: string);
begin
  UpdateBaseConfigProperty('BRCC_OutputDir', Value);
end;

procedure TDprojWriter.UpdateCBuilderOutput(const Value: TCBuilderOutputMode);
begin
  case Value of
    TCBuilderOutputMode.All:
      UpdateBaseConfigProperty('DCC_CBuilderOutput', 'All')
  else
    UpdateBaseConfigProperty('DCC_CBuilderOutput', '');
  end;
end;

procedure TDprojWriter.UpdateDcpOutput(const Value: string);
begin
  UpdateBaseConfigProperty('DCC_DcpOutput', Value);
end;

procedure TDprojWriter.UpdateExeOutput(const Value: string);
begin
  UpdateBaseConfigProperty('DCC_ExeOutput', Value);
end;

procedure TDprojWriter.UpdateDCUOutput(const Value: string);
begin
  UpdateBaseConfigProperty('DCC_DcuOutput', Value);
end;

function TDprojWriter.CreatePropertyGroupCache(const Values: TPropertyGroupEntryList): TDictionary<string, TPropertyGroupEntry>;
begin
  Result :=  TDictionary<string, TPropertyGroupEntry>.Create;
  try
    for var v in Values.List do
    begin
      Result.Add(TDProjConditionConverter.Convert(v.PlatformAndConfig), v);
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TDProjWriter.CreateCfgNode(const LastPropertyGroup: IXMLNode; var LastPropertyIndex: integer; const Conf: TPlatformAndConfig);
begin
  var NewPropertyGroup := Xml.CreateElement('PropertyGroup', LastPropertyGroup.NamespaceURI);

  NewPropertyGroup.Attributes['Condition'] := TDProjConditionConverter.ParentCondition(Conf);
  LastPropertyGroup.ParentNode.ChildNodes.Insert(LastPropertyIndex + 1, NewPropertyGroup);
  inc(LastPropertyIndex);

  if not Conf.Platform.HasValue then
  begin
    if not Conf.BuildConfig.HasValue then
    begin
      var Base := NewPropertyGroup.AddChild('Base');
      Base.NodeValue := true;
      exit;
    end;
    case Conf.BuildConfig.Value of
      TBuildConfig.Debug:
      begin
        var Cfg_1 := NewPropertyGroup.AddChild('Cfg_1');
        Cfg_1.NodeValue := true;
      end;
      TBuildConfig.Release:
      begin
        var Cfg_2 := NewPropertyGroup.AddChild('Cfg_2');
        Cfg_2.NodeValue := true;
      end
      else raise Exception.Create('Internal error.');

      var CfgParent := NewPropertyGroup.AddChild('CfgParent');
      CfgParent.NodeValue := 'Base';
      var Base := NewPropertyGroup.AddChild('Base');
      Base.NodeValue := true;
      exit;
    end;

  end;
  var IDEInfo: IDelphiIDEInfo := TDelphiIDEInfo.Create(Conf.IDEName);
  var PlatformId := IDEInfo.GetPlatform(Conf.Platform.Value).BuildName;
  if not Conf.BuildConfig.HasValue then
  begin
    //<Base_Linux64>true</Base_Linux64>
    var Base_Plat :=  NewPropertyGroup.AddChild('<Base_' + PlatformId);
    Base_Plat.NodeValue := true;
    //<CfgParent>Base</CfgParent>
    var CfgParent :=  NewPropertyGroup.AddChild('CfgParent');
    CfgParent.NodeValue := 'Base';
    //<Base>true</Base>
    var Base := NewPropertyGroup.AddChild('Base');
    Base.NodeValue := true;
    exit;
  end;

  var Cfg: string;
  case Conf.BuildConfig.Value of
    TBuildConfig.Debug: Cfg := 'Cfg_1';
    TBuildConfig.Release: Cfg := 'Cfg_2';
    else raise Exception.Create('Internal error.');
  end;

  //<Cfg_1_Linux64>true</Cfg_1_Linux64>
  var Cfg_Plat :=  NewPropertyGroup.AddChild(Cfg + '_' + PlatformId);
  Cfg_Plat.NodeValue := true;
  //<CfgParent>Cfg_1</CfgParent>
  var CfgParent :=  NewPropertyGroup.AddChild('CfgParent');
  CfgParent.NodeValue := Cfg;
  //<Cfg_1>true</Cfg_1>
  var CfgNode :=  NewPropertyGroup.AddChild(Cfg);
  CfgNode.NodeValue := true;

  //<Base>true</Base>
  var Base := NewPropertyGroup.AddChild('Base');
  Base.NodeValue := true;


end;

procedure TDprojWriter.UpdateDCCUnitSearchPath(const Values: TPropertyGroupEntryList);
begin
  if Values.Count = 0 then exit;

  var ValuesCache := CreatePropertyGroupCache(Values);
  try
    var WrittenNodes := THashSet<string>.Create;
    try
      var ExistingNodes := THashSet<string>.Create;
      try
        var LastPropertyGroup: IXmlNode := nil;
        IteratePropertyGroups(function(Info: TPropGroupInfo): Boolean
          begin
            Result := False;
            ExistingNodes.Add(Info.Condition);
            var PropGroup: TPropertyGroupEntry;
            if ValuesCache.TryGetValue(Info.Condition, PropGroup) and (PropGroup.UnitSearchPath.ValueOrDefault <> '') then
            begin
              WrittenNodes.Add(Info.Condition);
              CreateOrSetNode(Info.Node, 'DCC_UnitSearchPath', PropGroup.UnitSearchPath)
            end
            else RemoveNode(Info.Node, 'DCC_UnitSearchPath');

            LastPropertyGroup := Info.Node;
          end
        );

        if LastPropertyGroup = nil then raise Exception.Create('DProj doesn''t have PropertyGroups');


        var LastPropertyIndex := LastPropertyGroup.ParentNode.ChildNodes.IndexOf(LastPropertyGroup);
        for var Value in Values.List do
        begin
          var Condition := TDProjConditionConverter.Convert(Value.PlatformAndConfig);
          if WrittenNodes.Contains(Condition) then continue;
          if Value.UnitSearchPath.ValueOrDefault = '' then continue;

          if not ExistingNodes.Contains(TDProjConditionConverter.ParentCondition(Value.PlatformAndConfig))
            then CreateCfgNode(LastPropertyGroup, LastPropertyIndex, value.PlatformAndConfig);


          var NewPropertyGroup := Xml.CreateElement('PropertyGroup', LastPropertyGroup.NamespaceURI);
          NewPropertyGroup.Attributes['Condition'] := Condition;
          var NewUnitSearchPath := NewPropertyGroup.AddChild('DCC_UnitSearchPath');
          NewUnitSearchPath.NodeValue := Value.UnitSearchPath;

          LastPropertyGroup.ParentNode.ChildNodes.Insert(LastPropertyIndex + 1, NewPropertyGroup);
          Inc(LastPropertyIndex);
        end;
      finally
        ExistingNodes.Free;
      end;

    finally
      WrittenNodes.Free;
    end;
  finally
    ValuesCache.Free;
  end;
end;

procedure TDprojWriter.UpdateDebugInformation(const Value: TDebugInformation);
begin
  if (IDEName >= TIDEName.delphixe5) then
  begin
    case Value of
      TDebugInformation.None:
        UpdateBaseConfigProperty('DCC_DebugInformation', '0');
      TDebugInformation.Limited:
        UpdateBaseConfigProperty('DCC_DebugInformation', '1');
    else
      UpdateBaseConfigProperty('DCC_DebugInformation', '');
    end;
  end;
end;

procedure TDprojWriter.UpdateDescription(const Description: string);
begin
  UpdateBaseConfigProperty('DCC_Description', Description);
end;

procedure TDprojWriter.UpdateDLLSuffix(const Value: string);
begin
    if SameText(Value, 'auto') or SameText(Value, '$(auto)') then
    UpdateBaseConfigProperty('DllSuffix', '$(Auto)')
  else
    UpdateBaseConfigProperty('DllSuffix', Value);
end;

procedure TDprojWriter.UpdateExplicitRebuild(ExplicitRebuild: boolean);
begin
  IteratePropertyGroups(function(Info: TPropGroupInfo): Boolean
    begin
      Result := False;
      if Info.IsBase and ExplicitRebuild then
        CreateOrSetNode(Info.Node, 'DCC_OutputNeverBuildDcps', 'true')
      else
        RemoveNode(Info.Node, 'DCC_OutputNeverBuildDcps');
    end
  );
end;

procedure TDprojWriter.UpdateFrameworkType(const FrameworkType: string);
var
  Node: IXmlNode;
begin
  if (IDEName < TIDEName.delphixe2) and SameText(FrameworkType, 'FMX') then
    raise Exception.Create('Cannot set framework type to FMX for Delphi versions lower than XE2');

  Node := MainPropertyGroupNode.ChildNodes.FindNode('FrameworkType');
  if Node <> nil then
    Node.NodeValue := FrameworkType;
end;

procedure TDprojWriter.UpdateHints(const Value: TProjectHints);
begin
  case Value of
    TProjectHints.Disabled:
      UpdateBaseConfigProperty('DCC_Hints', 'false');
  else
    UpdateBaseConfigProperty('DCC_Hints', '');
  end;
end;

procedure TDprojWriter.UpdateHppOutput(const Value: string);
begin
  UpdateBaseConfigProperty('DCC_HppOutput', Value);
end;

procedure TDprojWriter.UpdateNamespacesNode(Parent: IXmlNode; Value: string);
begin
  if (Value <> '') and not Value.ToUpper.Contains('$(DCC_NAMESPACE)') then
    Value := Value + ';$(DCC_Namespace)';

  if Value <> '' then
    CreateOrSetNode(Parent, 'DCC_Namespace', Value)
  else
    RemoveNode(Parent, 'DCC_Namespace');
end;

procedure TDprojWriter.UpdateDccObjPath(const Action: TFunc<string, string>);
begin
  UpdateConfigProperties('DCC_ObjPath', Action);
end;

procedure TDprojWriter.UpdateObjOutput(const Value: string);
begin
  UpdateBaseConfigProperty('DCC_ObjOutput', Value);
end;

procedure TDprojWriter.UpdateOutputXmlDocumentation(const Enable: Boolean);
begin
  if Enable then
    UpdateBaseConfigProperty('DCC_OutputXmlDocumentation', 'true')
  else
    UpdateBaseConfigProperty('DCC_OutputXmlDocumentation', '');
end;

procedure TDprojWriter.UpdateXmlOutput(const Value: string);
begin
  UpdateBaseConfigProperty('DCC_XmlOutput', Value);
end;

procedure TDprojWriter.UpdatePackageName(const PackageName: string);
var
  Node: IXmlNode;
begin
  // This unit replaces the package name in several places of project
  // Each version of DProj package has the package name in different places
  // so we need to check the version to know exactly where to change it.

  // for now (up to delphi xe7) this must be changed in all delphi versions
  MainPropertyGroupNode.ChildNodes.FindNode('MainSource').NodeValue := PackageName + '.dpk';

  //D12.1
  var ProjectName := MainPropertyGroupNode.ChildNodes.FindNode('ProjectName');
  if ProjectName <> nil then ProjectName.NodeValue := PackageName;


  if IDEName in [delphi2007] then
  begin
    // DCC_DependencyCheckOutputName has hard-coded path and package name.
    // It seems we can just delete it and it will be recreated if needed
    Node := MainPropertyGroupNode.ChildNodes.FindNode('DCC_DependencyCheckOutputName');
    if Node <> nil then
      MainPropertyGroupNode.ChildNodes.Remove(Node);
  end;

  if IDEName in [delphi2009, delphi2010] then
  begin
    // Still delete DCC_DependencyCheckOutputName but now it's in Base property group
    Node := BaseConfigNode.ChildNodes.FindNode('DCC_DependencyCheckOutputName');
    if Node <> nil then
      BaseConfigNode.ChildNodes.Remove(Node);
  end;

  if IDEName in [delphi2007, delphi2009, delphi2010, delphixe] then
  begin
    // Replace package name in this structure:
    // <ItemGroup>
    //    <DelphiCompile Include="Empty.dpk">
    //      <MainSource>MainSource</MainSource>
    //    </DelphiCompile>
    Node := FindNodeByAtt(ItemGroupNode, 'DelphiCompile', 'Include', 'Empty.dpk');
    if Node <> nil then
      Node.Attributes['Include'] := PackageName + '.dpk';
  end;

  if IDEName >= delphi2007 then
  begin
    // Look for this structure in <Delphi.Personality> node:
    // <Source><Source Name="MainSource">Empty.dpk</Source></Source>
    Node := FindNodeByAtt(DelphiPersonalityNode.ChildNodes.FindNode('Source'), 'Source', 'Name', 'MainSource');
    if (Node <> nil) and (Node.NodeValue = 'Empty.dpk') then
      Node.NodeValue := PackageName + '.dpk';
  end;

  if IDEName >= delphixe6 then
  begin
    // Look for this strucure:
    // <PropertyGroup Condition="'$(Base)'!=''">
    //    <SanitizedProjectName>Empty</SanitizedProjectName>
    Node := BaseConfigNode.ChildNodes.FindNode('SanitizedProjectName');
    if (Node <> nil) and (Node.NodeValue = 'Empty') then
      Node.NodeValue := PackageName;
  end;
end;

{ TPackageReadData }

constructor TPackageReadData.Create;
begin
  inherited;
end;

destructor TPackageReadData.Destroy;
begin
  inherited;
end;

class function TDprojReader.PlatformIsSupported(const AllPlats: integer;
  const dp: TPlatform): boolean;
begin
  var pId := PlatformMap[dp];
  Result := (pId and AllPlats) <> 0;
end;


//procedure TPackageReadData.Require(APackageName: string);
//begin
//  if TPath.GetExtension(APackageName) = '' then
//    APackageName := APackageName + '.dcp';
//  if RequiresOnly.IndexOf(APackageName) = -1 then
//    RequiresOnly.Add(APackageName);
//end;

//procedure TPackageReadData.Unrequire(APackageName: string);
//var
//  I: Integer;
//begin
//  if TPath.GetExtension(APackageName) = '' then
//    APackageName := APackageName + '.dcp';
//  I := RequiresOnly.IndexOf(APackageName);
//  if I > 0 then
//    RequiresOnly.Delete(I);
//end;

function GetNullableNodeValue(const Node: IXMLNode): Nullable<string>;
begin
  if Node = nil then exit(SNull);
  if VarIsNull(Node.NodeValue) then
    Result := SNull
  else
    Result := Node.NodeValue;
end;

{ TPackageReader }

procedure TDprojReader.ReadData(Data: TPackageReadData);
var
  Node: IXMLNode;
begin
  Data.PackageName := TPath.GetFileNameWithoutExtension(FileName);

  Node := MainPropertyGroupNode.ChildNodes.FindNode('FrameworkType');
  if (Node <> nil) then
    Data.FrameworkType := Node.Text;

  Node := MainPropertyGroupNode.ChildNodes.FindNode('TargetedPlatforms');
  if (Node <> nil) then
    Data.TargetedPlatforms := Node.Text;

  Node := MainPropertyGroupNode.ChildNodes.FindNode('ProjectGuid');
  Data.ProjectGuid := StringToGuid(Node.NodeValue);

  Data.CBuilderOutputMode := TCBuilderOutputMode.None;
  IteratePropertyGroups(function(Info: TPropGroupInfo): Boolean
    begin
      Result := False;
      if Info.IsBase then
      begin
        Node := Info.Node.ChildNodes.FindNode('DCC_Description');
        if Node <> nil then
          Data.Description := Node.NodeValue;

        Node := Info.Node.ChildNodes.FindNode('DCC_Namespace');
        if Node <> nil then
          Data.Namespaces := Node.NodeValue;

        Node := Info.Node.ChildNodes.FindNode('RuntimeOnlyPackage');
        if (Node <> nil) and SameText(Node.Text, 'true') then
          Data.Usage := TPackageUsage.Runtime;

        Node := Info.Node.ChildNodes.FindNode('DesignOnlyPackage');
        if (Node <> nil) and SameText(Node.Text, 'true') then
          Data.Usage := TPackageUsage.DesignTime;

        Node := Info.Node.ChildNodes.FindNode('DCC_CBuilderOutput');
        if (Node <> nil) and SameText(Node.Text, 'All') then
          Data.CBuilderOutputMode := TCBuilderOutputMode.All;
      end
      else
      if Info.IsWin32 or Info.IsWin64 or Info.IsWin64x then
      begin
        Node := Info.Node.ChildNodes.FindNode('DCC_Namespace');
        if Node <> nil then
          if Info.IsWin32 then
            Data.Win32Namespaces := Node.NodeValue
          else  if Info.IsWin64 then
            Data.Win64Namespaces := Node.NodeValue
          else  if Info.IsWin64x then
            Data.Win64xNamespaces := Node.NodeValue;
      end;


      var UnitSearchPath := Info.Node.ChildNodes.FindNode('DCC_UnitSearchPath');
      var ExeOutputPath := Info.Node.ChildNodes.FindNode('DCC_ExeOutput');
      var Defines := Info.Node.ChildNodes.FindNode('DCC_Define');
      if (UnitSearchPath <> nil) or (ExeOutputPath <> nil) or (Defines <> nil) then
      begin
        var c := TDProjConditionConverter.Convert(IDEName, Info.Condition);
        if TDProjConditionConverter.Convert(c) <> info.Condition then raise Exception.Create('The parsed condition must be the same as the original condition. ' + TDProjConditionConverter.Convert(c));

        Data.PropertyGroups.Add(TPropertyGroupEntry.Create(c, GetNullableNodeValue(UnitSearchPath), GetNullableNodeValue(ExeOutputPath), GetNullableNodeValue(Defines)));
      end;


    end
  );

  Node := MainPropertyGroupNode.ChildNodes.FindNode('TargetedPlatforms');
  if Node <> nil then
    Data.SupportedPlatforms := IntegerToPlatforms(Node.NodeValue);

  Node := ItemGroupNode.ChildNodes.First;
  while (Node <> nil) do
  begin
    if (Node.NodeName = 'RcCompile') then
    begin
      var FileName := Node.Attributes['Include'];
      if ExtractFileExt(FileName).ToLower <> '.rc' then
        raise Exception.Create('Unsupported RC compile item: ' + FileName);

      var ResName := GetNodeText(Node.ChildNodes.FindNode('Form'));
      if (ResName <> '') and (ExtractFileExt(ResName).ToLower <> '.res') then
        raise Exception.Create('Unsupported RC compile resource name: ' + ResName);

      Data.RcFiles.Add(FileName, ResName);
    end
    else
    if (Node.NodeName = 'None') then
    begin
      Data.NoneFiles.Add(Node.Attributes['Include']);
    end
    else
    if (Node.NodeName = 'DCCReference') then
    begin
      var FileName := Node.Attributes['Include'];
      var FormName := GetNodeText(Node.ChildNodes.FindNode('Form'));

      if ExtractFileExt(FileName).ToLower = '.pas' then
        Data.PasFiles.Add(FileName, FormName)
      else
      if ExtractFileExt(FileName).ToLower = '' then //Shouldn't happen. We assume it is a pas file.
        Data.PasFiles.Add(FileName + '.pas', FormName)
      else
      if ExtractFileExt(FileName).ToLower = '.dcr' then
        Data.DcrFiles.Add(FileName)
      else
      if ExtractFileExt(FileName).ToLower = '.dcp' then
        Data.DcpFiles.Add(FileName)
      else
      if ExtractFileExt(FileName).ToLower = '.res' then
        begin end
      else
        raise Exception.Create('Unsupported DCC reference file type: ' + FileName);
    end;

    Node := Node.NextSibling;
  end;

  // read project imports
//  Node := ProjectNode.ChildNodes.First;
//  while Node <> nil do
//  begin
//    if Node.NodeName = 'Import' then
//      Data.ProjectImports.Add(TProjectImport.Create(Node.Attributes['Project'], Node.Attributes['Condition']));
//    Node := Node.NextSibling;
//  end;
end;

{ TPackageWriteData }

constructor TPackageWriteData.Create;
begin
  inherited;
  FEnableWarnings := TProjectWarnings.Enabled;
  FEnableHints := TProjectHints.Enabled;
end;

destructor TPackageWriteData.Destroy;
begin
  inherited;
end;

function TPackageWriteData.GetLibSuffix: string;
begin
  Result := FCustomLibSuffix;
end;

{ TDprojModifier }

constructor  TDprojModifier.Create(const ASourceFileName: string; const AIDEName: TIDEName);
begin
  inherited Create(ASourceFileName, AIDEName);
end;

function TDProjModifier.IsSingleReplacement(const NodeName: string): boolean;
begin
  if NodeName = 'DCC_BplOutput' then exit(true);
  if NodeName = 'DCC_DcpOutput' then exit(true);
  if NodeName = 'DCC_ExeOutput' then exit(true);
  if NodeName = 'DCC_DcuOutput' then exit(true);
  if NodeName = 'BRCC_OutputDir' then exit(true);
  if NodeName = 'DCC_BpiOutput' then exit(true);
  if NodeName = 'DCC_HppOutput' then exit(true);
  if NodeName = 'DCC_ObjOutput' then exit(true);
  if NodeName = 'Icon_MainIcon' then exit(true);

  //Cpp
  if NodeName = 'FinalOutputDir' then exit(true);
  if NodeName = 'IntermediateOutputDir' then exit(true);

  Result := false;
end;

function TDProjModifier.ISCPPOnlyNode(const NodeName: string): boolean;
begin
  if NodeName = 'FinalOutputDir' then exit(true);
  if NodeName = 'IntermediateOutputDir' then exit(true);
  Result := false;
end;

function TDProjModifier.IsExe(const NodeName: string): boolean;
begin
  if NodeName = 'DCC_ExeOutput' then exit(true);
  Result := false;
end;

function TDProjModifier.IsOutput(const NodeName: string): boolean;
begin
  for var OutputNode in AllOutputNodes do if NodeName = OutputNode then exit(true);
  Result := false;
end;


function TDProjModifier.IsMultipleReplacement(const NodeName: string): boolean;
begin
  if NodeName = 'DCC_ObjPath' then exit(true);
    //Cpp
  if NodeName = 'IncludePath' then exit(true);
  if NodeName = 'ILINK_LibraryPath' then exit(true);
  if NodeName = 'ILINK_TranslatedLibraryPath' then exit(true);
  if NodeName = 'DCC_UnitSearchPath' then exit(true);
  Result := false;
end;

function TDProjModifier.IsFileItem(const NodeName: string): boolean;
begin
  if NodeName = 'DCCReference' then exit(true);
  if NodeName = 'None' then exit(true);
  if NodeName = 'RcCompile' then exit(true);
  if NodeName = 'RcItem' then exit(true);
  //Cpp
  if NodeName = 'CppCompile' then exit(true);
  if NodeName = 'FormResources' then exit(true);

  Result := false;
end;

procedure TDprojModifier.ProcessPropertyGroup(const PropertyGroup: IXMLNode; const SinglePathModifier, MultiplePathModifier: TPathModifier);
begin
  var PropGroupIsBase := IsBase(PropertyGroup.LocalName, PropertyGroup.Attributes['Condition']);
  for var i := 0 to PropertyGroup.ChildNodes.Count - 1 do
  begin
    var Node := PropertyGroup.ChildNodes[i];
    var LocalName := Node.LocalName;
    if IsSingleReplacement(LocalName) then Node.Text := SinglePathModifier(Node.Text, Node.LocalName, IsOutput(LocalName), IsExe(LocalName), PropGroupIsBase)
    else if IsMultipleReplacement(LocalName) then Node.Text := MultiplePathModifier(Node.Text, Node.LocalName, false, false, PropGroupIsBase);
  end;
end;

procedure TDprojModifier.ProcessItemGroup(const ItemGroup: IXMLNode; const SinglePathModifier: TPathModifier);
begin
  for var i := 0 to ItemGroup.ChildNodes.Count - 1 do
  begin
    var Node := ItemGroup.ChildNodes[i];
    var LocalName := Node.LocalName;
    if IsFileItem(LocalName) then
    begin
      if Node.HasAttribute('Include') then
      begin
        var Text := Node.Attributes['Include'];
        Node.Attributes['Include'] := SinglePathModifier(Text, LocalName, false, false, false);
      end;
    end;
  end;
end;

procedure TDprojModifier.ProcessProject(const Project: IXMLNode; const SinglePathModifier, MultiplePathModifier: TPathModifier);
begin
  for var i := 0 to Project.ChildNodes.Count - 1 do
  begin
    if Project.ChildNodes[i].LocalName = 'PropertyGroup' then
    begin
      ProcessPropertyGroup(Project.ChildNodes[i], SinglePathModifier, MultiplePathModifier);
    end;

    if Project.ChildNodes[i].LocalName = 'ItemGroup' then
    begin
      ProcessItemGroup(Project.ChildNodes[i], SinglePathModifier);
    end;
  end;
end;

procedure TDprojModifier.AdaptAllPaths(const SinglePathModifier, MultiplePathModifier: TPathModifier);
begin
  for var i := 0 to Xml.ChildNodes.Count - 1 do
  begin
    if Xml.ChildNodes[i].LocalName = 'Project' then
    begin
      ProcessProject(Xml.ChildNodes[i], SinglePathModifier, MultiplePathModifier);
    end;
  end;
end;

function TDProjModifier.IsBase(const NodeName: string; const v: OLEVariant): boolean;
begin
  if NodeName <> 'PropertyGroup' then exit(False);
  if IDEName >= TIDEName.delphi2009 then
    Result := v = BaseConfigCondition
  else
    Result := (v = D2007DebugConfigCondition) or (v = D2007ReleaseConfigCondition);
end;

procedure TDprojModifier.LoopOverAllNodes(
  const NodeModifier: TFunc<string, string, boolean, string>; const BasePropertyGroup: TProc<IInterface>);
begin
  LoopOverAllChildNodes(Xml.Node, '', false, NodeModifier, BasePropertyGroup);
end;

procedure TDprojModifier.LoopOverAllChildNodes(
  const Node: IXMLNode; const RootPath: string; const ParentIsBase: boolean;
  const NodeModifier: TFunc<string, string, boolean, string>; const BasePropertyGroup: TProc<IInterface>);
begin
  if Node = nil then exit;
  var NodePath := RootPath + '/' + Node.NodeName;
  var PropGroupBase := IsBase(Node.LocalName, Node.Attributes['Condition']);
  if PropGroupBase then BasePropertyGroup(Node);
  
  var NodeIsBase := ParentIsBase or PropGroupBase;
  if (Node.NodeType = TNodeType.ntText) and Assigned(NodeModifier) then Node.Text := NodeModifier(NodePath, Node.Text, ParentIsBase);

  for var i := 0 to Node.ChildNodes.Count - 1 do
  begin
    LoopOverAllChildNodes(Node.ChildNodes[i], NodePath, NodeIsBase, NodeModifier, BasePropertyGroup);
  end;

end;

procedure TDprojModifier.AddChildNode(const Parent: IInterface; const Name, Value: string);
begin
  var ParentNode := Parent as IXMLNode;
  if (ParentNode = nil) then exit;
  var NewNode := ParentNode.AddChild(Name);
  NewNode.NodeValue := Value;
end;

function TDprojModifier.FindBaseProjectNode: IXMLNode;
begin
  for var i := 0 to Xml.ChildNodes.Count - 1 do
  begin
    var Project := Xml.ChildNodes[i];
    if Project.LocalName = 'Project' then
    begin
      for var j := 0 to Project.ChildNodes.Count - 1 do
      begin
        var PropertyGroup := Project.ChildNodes[j];
        if PropertyGroup.LocalName = 'PropertyGroup' then
        begin
          if IsBase(PropertyGroup.LocalName, PropertyGroup.Attributes['Condition']) then exit(PropertyGroup);
        end;
      end;
    end;
  end;
  Result := nil;
end;

procedure TDprojModifier.AddBaseProjectNode(const Name, Value: string);
begin
  var Base := FindBaseProjectNode;
  if Base <> nil then AddChildNode(Base, Name, Value);


end;
procedure TDprojModifier.Save(const DestFileName: string);
begin
  if FXml = nil then Exit;

  Xml.XML.Text := FormatXMLData(Xml.XML.Text);
  Xml.Active := true;
  try
    Xml.SaveToFile(DestFileName);
  except on ex: Exception do
    begin
      var process := FindProcessUsing(DestFileName);
      if process <> '' then process := ' (locked by: "' + process + '")';

        raise Exception.Create(ex.Message + ' File: ' + DestFileName + process);
    end;
  end;
end;

function TDprojModifier.FindNodeRecursive(const Node: IXmlNode; const Sections: TArray<string>; const index: integer): IXmlNode;
begin
  if (index >= Length(Sections)) then exit(nil);

  for var i := 0 to Node.ChildNodes.Count - 1 do
  begin
    var Child := Node.ChildNodes[i];
    if Child.NodeName = Sections[index] then
    begin
      if (index = Length(Sections) - 1) then exit(Child);
      exit(FindNodeRecursive(Child, Sections, index + 1));
    end;
  end;

  exit(nil);

end;

procedure TDprojModifier.SetAttIfExists(const Path, AttName, AttValue: string);
begin
  var Sections := Path.Split(['/']);
  var index := 0;
  if (Length(Sections) <= index) or (Sections[index] <> '') then exit;
  inc(index);
  if (Length(Sections) <= index) or (Sections[index] <> Xml.Node.NodeName) then exit;
  inc(index);
  var Node := FindNodeRecursive(Xml.Node, Sections, index);
  if Node <> nil then
  begin
    Node.Attributes[AttName] := AttValue;
  end;
end;


//all:             <PropertyGroup Condition="'$(Base)'!=''">
//all debug:       <PropertyGroup Condition="'$(Cfg_1)'!=''">
//all release:     <PropertyGroup Condition="'$(Cfg_2)'!=''">
//linux64:         <PropertyGroup Condition="'$(Base_Linux64)'!=''">
//linux64 debug:   <PropertyGroup Condition="'$(Cfg_1_Linux64)'!=''">
//linux64 release: <PropertyGroup Condition="'$(Cfg_2_Linux64)'!=''">
const

ConditionAll = '''''
'$(Base)'!=''
''''';
ConditionAllDebug = '''''
'$(Cfg_1)'!=''
''''';
ConditionAllRelease = '''''
'$(Cfg_2)'!=''
''''';
ConditionPlatAll = '''''
'$(Base_%s)'!=''
''''';
ConditionPlatDebug = '''''
'$(Cfg_1_%s)'!=''
''''';
ConditionPlatRelease = '''''
'$(Cfg_2_%s)'!=''
''''';

ParentConditionAll = '''''
'$(Config)'=='Base' or '$(Base)'!=''
''''';
ParentConditionAllDebug = '''''
'$(Config)'=='Debug' or '$(Cfg_1)'!=''
''''';
ParentConditionAllRelease = '''''
'$(Config)'=='Release' or '$(Cfg_2)'!=''
''''';
ParentConditionPlatAll = '''''
('$(Platform)'=='%0:s' and '$(Base)'=='true') or '$(Base_%0:s)'!=''
''''';
ParentConditionPlatDebug = '''''
('$(Platform)'=='%0:s' and '$(Cfg_1)'=='true') or '$(Cfg_1_%0:s)'!=''
''''';
ParentConditionPlatRelease = '''''
('$(Platform)'=='%0:s' and '$(Cfg_2)'=='true') or '$(Cfg_2_%0:s)'!=''
''''';

{ TDProjConditionConverter }

class function TDProjConditionConverter.Convert(const AIDEName: TIDEName; const DprojCondString: string): TPlatformAndConfig;
begin
  var s := DprojCondString.Trim;
  if s = ConditionAll.Trim then
  begin
    exit;
  end;

  if s = ConditionAllDebug.Trim then
  begin
    exit(TPlatformAndConfig.Create(AIDEName, SNull, TBuildConfig.Debug));
  end;

  if s = ConditionAllRelease.Trim then
  begin
    exit(TPlatformAndConfig.Create(AIDEName, SNull, TBuildConfig.Release));
  end;

  var IDEInfo: IDelphiIDEInfo := TDelphiIDEInfo.Create(AIDEName);

  for var Plat := Low(TPlatform) to High(TPlatform) do
  begin
    var PlatformId := IDEInfo.GetPlatform(Plat).BuildName;
    if s = Format(ConditionPlatAll,[PlatformId]) then
    begin
      exit(TPlatformAndConfig.Create(AIDEName, Plat, SNull));
    end;
    if s = Format(ConditionPlatDebug,[PlatformId]) then
    begin
      exit(TPlatformAndConfig.Create(AIDEName, Plat, TBuildConfig.Debug));
    end;
    if s = Format(ConditionPlatRelease,[PlatformId]) then
    begin
      exit(TPlatformAndConfig.Create(AIDEName, Plat, TBuildConfig.Release));
    end;

  end;

  raise Exception.Create('The condition string: "' + DprojCondString + '" is not a string we understand.');
end;

class function TDProjConditionConverter.InternalConvert(const PlatformAndConfig: TPlatformAndConfig;
   const AConditionAll, AConditionAllDebug, AConditionAllRelease, AConditionPlatAll, AconditionPlatDebug, AConditionPlatRelease: string): string;
begin
  if not PlatformAndConfig.Platform.HasValue then
  begin
    if not PlatformAndConfig.BuildConfig.HasValue then
    begin
      exit(AConditionAll);
    end;
    case PlatformAndConfig.BuildConfig.Value of
      TBuildConfig.Debug: exit(AConditionAllDebug);
      TBuildConfig.Release: exit(AConditionAllRelease);
      else raise Exception.Create('Internal error.');
    end;

  end;
  var IDEInfo: IDelphiIDEInfo := TDelphiIDEInfo.Create(PlatformAndConfig.IDEName);
  var PlatformId := IDEInfo.GetPlatform(PlatformAndConfig.Platform.Value).BuildName;
  if not PlatformAndConfig.BuildConfig.HasValue then
  begin
    exit(Format(AConditionPlatAll,[PlatformId]));
  end;

  case PlatformAndConfig.BuildConfig.Value of
    TBuildConfig.Debug: exit(Format(AConditionPlatDebug,[PlatformId]));
    TBuildConfig.Release: exit(Format(AConditionPlatRelease,[PlatformId]));
    else raise Exception.Create('Internal error.');
  end;
end;

class function TDProjConditionConverter.Convert(const PlatformAndConfig: TPlatformAndConfig): string;
begin
  Result := InternalConvert(PlatformAndConfig, ConditionAll, ConditionAllDebug, ConditionAllRelease,
     ConditionPlatAll, ConditionPlatDebug, ConditionPlatRelease);
end;


class function TDProjConditionConverter.ParentCondition(
  const PlatformAndConfig: TPlatformAndConfig): string;
begin
  Result := InternalConvert(PlatformAndConfig, ParentConditionAll, ParentConditionAllDebug, ParentConditionAllRelease,
     ParentConditionPlatAll, ParentConditionPlatDebug, ParentConditionPlatRelease);
end;

{ TBasePackageReadData }
constructor TBasePackageReadData.Create;
begin
  FRcFiles := TRcCompileFiles.Create('.rc');
  FDcrFiles := TIncludeFiles.Create('.dcr');
  FPasFiles := TPasIncludeFiles.Create('.pas');
  FNoneFiles := TIncludeFiles.Create('');
  FDcpFiles := TIncludeFiles.Create('.dcp');
  FPropertyGroups := TPropertyGroupEntryList.Create;

end;

destructor TBasePackageReadData.Destroy;
begin
  FRcFiles.Free;
  FDcrFiles.Free;
  FPasFiles.Free;
  FNoneFiles.Free;
  FDcpFiles.Free;

  FPropertyGroups.Free;
  inherited;
end;

{$ENDIF}

end.

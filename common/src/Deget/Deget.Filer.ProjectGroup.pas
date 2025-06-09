unit Deget.Filer.ProjectGroup;

interface

uses
  Generics.Collections, System.SysUtils, System.Classes, System.Types, Xml.XMLIntf, Win.ComObj, WinApi.ActiveX,
  Deget.CoreTypes;

type
  TProjectGroupItem = class
  strict private
    FProjectName: string;
    FDependencies: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    property ProjectName: string read FProjectName write FProjectName;
    property Dependencies: TStrings read FDependencies;
  end;

  TGroupProjFiler = class
  strict private
    FXml: IXmlDocument;
    FFileName: string;
    function GetXml: IXmlDocument;
  strict protected
    function ProjectNode: IXMLNode;
    property Xml: IXmlDocument read GetXml;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    property FileName: string read FFileName;
  end;

  TGroupProjReader = class(TGroupProjFiler)
  public
    procedure ReadItems(AItems: TList<TProjectGroupItem>);
    function ReadProjectGuid: TGUID;
  end;

  TGroupProjWriter = class(TGroupProjFiler)
  strict private
    procedure BuildTargetNode(Node: IXmlNode; Item: TProjectGroupItem; const TargetSuffix: string);
    procedure BuildCallTargetNode(Node: IXmlNode; AItems: TList<TProjectGroupItem>; const TargetSuffix: string);
    function BuildTargetName(const Name, Suffix: string): string;
  strict protected
    procedure RemoveNodes(Nodes: IXMLNodeList; const NodeName: string);
  public
    class procedure GenerateEmptyFile(const TargetGroupFile: string; IDEName: TIDEName; DeleteExisting: Boolean = False); overload;
  public
    procedure WriteItems(AItems: TList<TProjectGroupItem>);
    procedure WriteProjectGuid(const Guid: TGUID);
    procedure Flush;
  end;

implementation

uses
  System.IOUtils, System.Zip, Xml.XMLDoc;

{ TGroupProjFiler }

constructor TGroupProjFiler.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  CoInitialize(nil);
end;

destructor TGroupProjFiler.Destroy;
begin
  FXml := nil;
  CoUninitialize;
  inherited;
end;

function TGroupProjFiler.GetXml: IXmlDocument;
begin
  if FXml = nil then
  begin
    FXml := TXMLDocument.Create(nil);
    FXml.Options := Xml.Options + [doNodeAutoIndent];
    FXml.LoadFromFile(FileName);
  end;
  Result := FXml;
end;

function TGroupProjFiler.ProjectNode: IXMLNode;
begin
  Result := Xml.ChildNodes.FindNode('Project');
end;

{ TProjectGroupItem }

constructor TProjectGroupItem.Create;
begin
  inherited Create;
  FDependencies := TStringList.Create;
  FDependencies.Delimiter := ';'
end;

destructor TProjectGroupItem.Destroy;
begin
  FDependencies.Free;
end;

{ TGroupProjReader }

procedure TGroupProjReader.ReadItems(AItems: TList<TProjectGroupItem>);
var
  ItemGroupNode: IXmlNode;
  ProjectsNode: IXmlNode;
  Item: TProjectGroupItem;
begin
  AItems.Clear;
  if ProjectNode = nil then Exit;
  
  ItemGroupNode := ProjectNode.ChildNodes.First;
  while (ItemGroupNode <> nil) do
  begin
    if (ItemGroupNode.NodeName = 'ItemGroup') then
    begin
      ProjectsNode := ItemGroupNode.ChildNodes.First;
      while ProjectsNode <> nil do
      begin
        if (ProjectsNode.NodeName = 'Projects') then
        begin
          var IncludeName := ProjectsNode.Attributes['Include'];
          if IncludeName <> '' then
          begin
            Item := TProjectGroupItem.Create;
            AItems.Add(Item);
            Item.ProjectName := IncludeName;
            var DependenciesNode := ProjectsNode.ChildNodes['Dependencies'];
            if DependenciesNode <> nil then
              Item.Dependencies.DelimitedText := DependenciesNode.Text;
          end;
        end;
        ProjectsNode := ProjectsNode.NextSibling;
      end;
    end;
    ItemGroupNode := ItemGroupNode.NextSibling;
  end;
end;

function TGroupProjReader.ReadProjectGuid: TGUID;
begin
  Result := TGUID.Empty;
  if (ProjectNode = nil) or (ProjectNode.ChildNodes = nil) then Exit;

  var PropertyGroupNode := ProjectNode.ChildNodes.FindNode('PropertyGroup');
  if (PropertyGroupNode = nil) or (PropertyGroupNode.ChildNodes = nil) then Exit;

  var ProjectGuidNode := PropertyGroupNode.ChildNodes.FindNode('ProjectGuid');
  if (ProjectGuidNode = nil) or (ProjectGuidNode.Text = '') then Exit;

  Result := TGUID.Create(ProjectGuidNode.Text);
end;

{ TGroupProjWriter }

procedure TGroupProjWriter.BuildCallTargetNode(Node: IXmlNode; AItems: TList<TProjectGroupItem>; const TargetSuffix: string);
begin
  if TargetSuffix <> '' then
    Node.Attributes['Name'] := TargetSuffix
  else
    Node.Attributes['Name'] := 'Build';

  var CallTargetNode := Node.AddChild('CallTarget');
  var Targets := '';
  for var Item in AItems do
    Targets := Targets + BuildTargetName(Item.ProjectName, TargetSuffix) + ';';
  if Targets <> '' then
    CallTargetNode.Attributes['Targets'] := copy(Targets, 1, Length(Targets) - 1);
end;

function TGroupProjWriter.BuildTargetName(const Name, Suffix: string): string;
begin
  Result := TPath.GetFileNameWithoutExtension(Name);
  if Suffix <> '' then
    Result := Result + ':' + Suffix;
end;

procedure TGroupProjWriter.BuildTargetNode(Node: IXmlNode; Item: TProjectGroupItem; const TargetSuffix: string);
begin
  var TargetName := BuildTargetName(Item.ProjectName, TargetSuffix);
  Node.Attributes['Name'] := TargetName;

  var DependsOn := '';
  for var Dependency in Item.Dependencies do
    DependsOn := DependsOn + BuildTargetName(Dependency, TargetSuffix) + ';';
  if DependsOn <> '' then
    Node.Attributes['DependsOnTargets'] := Copy(DependsOn, 1, Length(DependsOn) - 1);

  var MSBuildNode := Node.AddChild('MSBuild');
  MSBuildNode.Attributes['Projects'] := Item.ProjectName;
  if TargetSuffix <> '' then
    MSBuildNode.Attributes['Targets'] := TargetSuffix;
end;

procedure TGroupProjWriter.Flush;
begin
  Xml.XML.Text := FormatXMLData(Xml.XML.Text);
  Xml.Active := true;
  Xml.SaveToFile(FileName);
end;

class procedure TGroupProjWriter.GenerateEmptyFile(const TargetGroupFile: string; IDEName: TIDEName; DeleteExisting: Boolean);
var
  Xml: IXMLDocument;
  EmptyGroupFile: TBytes;
  Writer: TGroupProjWriter;
begin
  if IDEName <= delphi7 then Exit;

  var GroupName := TPath.GetFileNameWithoutExtension(TargetGroupFile);
  var TargetFolder := TPath.GetDirectoryName(TargetGroupFile);

  CoInitializeEx(nil, COINIT_MULTITHREADED);
  try
    // Load default empty files
    begin
      var Res := TResourceStream.Create(HInstance, 'EMPTY_PACKAGES_ZIP', RT_RCDATA);
      try
        var Zip := TZipFile.Create;
        try
          Zip.Open(Res, zmRead);
          Zip.Read(Format('%s/EmptyGroup.groupproj', [IDEId[IDEName]]), EmptyGroupFile);
        finally
          Zip.Free;
        end;
      finally
        Res.Free;
      end;
    end;

    if TFile.Exists(TargetGroupFile) then
      if not DeleteExisting then
        raise Exception.CreateFmt('Cannot create package group "%s" in folder "%s": File already exists', [GroupName, TargetFolder]);

    ForceDirectories(TargetFolder);

    Xml := TXMLDocument.Create(nil);
    Xml.Options := Xml.Options + [doNodeAutoIndent];

    // Create based on an empty package
    Xml.LoadFromXML(TEncoding.UTF8.GetString(EmptyGroupFile));

    // Save Dproj file
    Xml.XML.Text := FormatXMLData(Xml.XML.Text);
    Xml.Active := True;
    Xml.SaveToFile(TargetGroupFile);
    Xml := nil;

    // Now replace initial info in the project that
    Writer := TGroupProjWriter.Create(TargetGroupFile);
    try
      Writer.WriteProjectGuid(TGuid.NewGuid);
      Writer.Flush;
    finally
      Writer.Free;
    end;
  finally
    CoUninitialize;
  end;
end;

procedure TGroupProjWriter.RemoveNodes(Nodes: IXMLNodeList; const NodeName: string);
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

procedure TGroupProjWriter.WriteItems(AItems: TList<TProjectGroupItem>);
begin
  if ProjectNode = nil then Exit;
  RemoveNodes(ProjectNode.ChildNodes, 'ItemGroup');
  RemoveNodes(ProjectNode.ChildNodes, 'Target');

  // Add ItemGroup items
  var ItemGroupNode := ProjectNode.AddChild('ItemGroup', 1); // add as second node after PropertyGroup
  for var Item in AItems do
  begin
    var ProjectsNode := ItemGroupNode.AddChild('Projects');
    ProjectsNode.Attributes['Include'] := Item.ProjectName;
    var DependenciesNode := ProjectsNode.AddChild('Dependencies');
    if Item.Dependencies.Count > 0 then
      DependenciesNode.Text := Item.Dependencies.DelimitedText;
  end;

  // Add Targets
  var TargetIndex := ProjectNode.ChildNodes.Count - 1; // insert before the last node (Imports)
  for var Item in AItems do
  begin
    var TargetNode := ProjectNode.AddChild('Target', TargetIndex);
    BuildTargetNode(TargetNode, Item, '');

    Inc(TargetIndex);
    TargetNode := ProjectNode.AddChild('Target', TargetIndex);
    BuildTargetNode(TargetNode, Item, 'Clean');

    Inc(TargetIndex);
    TargetNode := ProjectNode.AddChild('Target', TargetIndex);
    BuildTargetNode(TargetNode, Item, 'Make');

    Inc(TargetIndex);
  end;

  // Add final call targets
  var CallTargetNode := ProjectNode.AddChild('Target', TargetIndex);
  BuildCallTargetNode(CallTargetNode, AItems, '');

  Inc(TargetIndex);
  CallTargetNode := ProjectNode.AddChild('Target', TargetIndex);
  BuildCallTargetNode(CallTargetNode, AItems, 'Clean');

  Inc(TargetIndex);
  CallTargetNode := ProjectNode.AddChild('Target', TargetIndex);
  BuildCallTargetNode(CallTargetNode, AItems, 'Make');
end;

procedure TGroupProjWriter.WriteProjectGuid(const Guid: TGUID);
begin
  if (ProjectNode = nil) or (ProjectNode.ChildNodes = nil) then Exit;

  var PropertyGroupNode := ProjectNode.ChildNodes.FindNode('PropertyGroup');
  if (PropertyGroupNode = nil) then
    PropertyGroupNode := ProjectNode.AddChild('PropertyGroup', 0);

  var ProjectGuidNode := PropertyGroupNode.ChildNodes.FindNode('ProjectGuid');
  if (ProjectGuidNode = nil) then
    ProjectGuidNode := PropertyGroupNode.AddChild('ProjectGuidNode', 0);

  ProjectGuidNode.Text := Guid.ToString;
end;

end.

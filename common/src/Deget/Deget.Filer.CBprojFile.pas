unit Deget.Filer.CBprojFile;
{$IFDEF MSWINDOWS}

{$SCOPEDENUMS ON}

interface
uses Deget.Filer.DprojFile, Deget.Filer.Types, Deget.CoreTypes;

type
  TCBProjPackageReadData = class(TBasePackageReadData)
  private
    FCppFiles: TCppIncludeFiles;
    FAsmFiles: TIncludeFiles;
    FPackageImportFiles: TIncludeFiles;
    FPCHFiles: TIncludeFiles;
    FLibFiles: TIncludeFiles;
    FDefFiles: TIncludeFiles;
    FObjFiles: TIncludeFiles;
    FResFiles: TIncludeFiles;
    FFormResourceFiles: TIncludeFiles;

    FBaseIncludePath: string;
    FBaseILINK_LibraryPath: string;
    FWin32ClangCompiler: boolean;

  public
    constructor Create;
    destructor Destroy; override;

    property CppFiles: TCppIncludeFiles read FCppFiles;
    property AsmFiles: TIncludeFiles read FAsmFiles;

    property PackageImportFiles: TIncludeFiles read FPackageImportFiles;
    property PCHFiles: TIncludeFiles read FPCHFiles;
    property LibFiles: TIncludeFiles read FLibFiles;
    property DefFiles: TIncludeFiles read FDefFiles;
    property ObjFiles: TIncludeFiles read FObjFiles;
    property ResFiles: TIncludeFiles read FResFiles;

    property FormResourceFiles: TIncludeFiles read FFormResourceFiles;

    property BaseIncludePath: string read FBaseIncludePath write FBaseIncludePath;
    property BaseILINK_LibraryPath: string read FBaseILINK_LibraryPath write FBaseILINK_LibraryPath;

    //Applies only to win32
    property Win32ClangCompiler: boolean read FWin32ClangCompiler write FWin32ClangCompiler;
  end;


type
  TCBprojReader = class(TDprojFiler)
  public
    procedure ReadData(Data: TCBProjPackageReadData);
  end;

implementation
uses Classes, SysUtils, IOUtils;

{ TCBprojReader }

procedure TCBprojReader.ReadData(Data: TCBProjPackageReadData);
begin
  Data.PackageName := TPath.GetFileNameWithoutExtension(FileName);

  var Node := MainPropertyGroupNode.ChildNodes.FindNode('FrameworkType');
  if (Node <> nil) then
    Data.FrameworkType := Node.Text;

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

        Node := Info.Node.ChildNodes.FindNode('IncludePath');
        if (Node <> nil) then
          Data.BaseIncludePath := Node.NodeValue;

        Node := Info.Node.ChildNodes.FindNode('ILINK_LibraryPath');
        if (Node <> nil) then
          Data.BaseILINK_LibraryPath := Node.NodeValue;

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

      //This could be set in debug or release only. We will require a single setting for all the project.
      Node := Info.Node.ChildNodes.FindNode('BCC_UseClassicCompiler');
      if Node <> nil then
        Data.Win32ClangCompiler := not Node.NodeValue;

      var UnitSearchPath := Info.Node.ChildNodes.FindNode('DCC_UnitSearchPath');
      var ExeOutputPath := Info.Node.ChildNodes.FindNode('DCC_ExeOutput');
      var Defines := Info.Node.ChildNodes.FindNode('DCC_Define');
      if (UnitSearchPath <> nil) or (ExeOutputPath <> nil) then
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
    if (Node.NodeName = 'ResourceCompile') then
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
    if (Node.NodeName = 'CppCompile') then
    begin
      var FileName := Node.Attributes['Include'];
      var FormName := GetNodeText(Node.ChildNodes.FindNode('Form'));
      var HeaderName := GetNodeText(Node.ChildNodes.FindNode('DependentOn'));

      Data.CppFiles.Add(FileName, FormName, HeaderName);
    end
    else
    if (Node.NodeName = 'AsmCompile') then
    begin
      Data.AsmFiles.Add(Node.Attributes['Include']);
    end
    else
    if (Node.NodeName = 'PackageImport') then
    begin
      Data.PackageImportFiles.Add(Node.Attributes['Include']);
    end
    else
    if (Node.NodeName = 'PCHCompile') then
    begin
      Data.PCHFiles.Add(Node.Attributes['Include']);
    end
    else
    if (Node.NodeName = 'LibFiles') then
    begin
      Data.LibFiles.Add(Node.Attributes['Include']);
    end
    else
    if (Node.NodeName = 'DefFile') then
    begin
      Data.DefFiles.Add(Node.Attributes['Include']);
    end
    else
    if (Node.NodeName = 'None') then
    begin
      Data.NoneFiles.Add(Node.Attributes['Include']);
    end
    else
    if (Node.NodeName = 'ObjFiles') then
    begin
      Data.ObjFiles.Add(Node.Attributes['Include']);
    end
    else
    if (Node.NodeName = 'ResFiles') then
    begin
      Data.ResFiles.Add(Node.Attributes['Include']);
    end
    else
    if (Node.NodeName = 'FormResources') then
    begin
      Data.FormResourceFiles.Add(Node.Attributes['Include']);
    end
    else
    if (Node.NodeName = 'DelphiCompile') then
    begin
      var FileName := Node.Attributes['Include'];
      var FormName := GetNodeText(Node.ChildNodes.FindNode('Form'));

      if ExtractFileExt(FileName).ToLower = '.pas' then
        Data.PasFiles.Add(FileName, FormName)
      else
      if ExtractFileExt(FileName).ToLower = '.dcr' then
        Data.DcrFiles.Add(FileName)
      else
      if ExtractFileExt(FileName).ToLower = '.dcp' then
        Data.DcpFiles.Add(FileName)
      else
        raise Exception.Create('Unsupported DCC refrence file type: ' + FileName);
    end;

    Node := Node.NextSibling;
  end;
end;

{ TCbProjPackageReadData }

constructor TCbProjPackageReadData.Create;
begin
  inherited;
  FCppFiles := TCppIncludeFiles.Create('');
  FAsmFiles := TIncludeFiles.Create('.asm');

  FPackageImportFiles := TIncludeFiles.Create('.bpi');
  FPCHFiles := TIncludeFiles.Create('.h');
  FLibFiles := TIncludeFiles.Create('');
  FDefFiles := TIncludeFiles.Create('.def');
  FObjFiles := TIncludeFiles.Create('');
  FResFiles := TIncludeFiles.Create('.res');
  FFormResourceFiles := TIncludeFiles.Create('');

end;

destructor TCbProjPackageReadData.Destroy;
begin
  FCppFiles.Free;
  FAsmFiles.Free;

  FPackageImportFiles.Free;
  FPCHFiles.Free;
  FLibFiles.Free;
  FDefFiles.Free;
  FObjFiles.Free;
  FResFiles.Free;
  FFormResourceFiles.Free;

  inherited;
end;
{$ELSE}
interface
implementation
{$ENDIF}

end.

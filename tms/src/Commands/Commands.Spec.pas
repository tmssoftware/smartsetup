unit Commands.Spec;
interface

uses
  System.SysUtils, UCommandLine, VSoft.CommandLine.Options, Commands.CommonOptions, UMultiLogger, ULogger;

procedure RegisterSpecCommand;

implementation

uses
  USpecWriter, UTmsBuildSystemUtils, UProjectDefinition, UProjectLoader, IOUtils, Deget.Filer.DprojFile,
  Deget.Version, Deget.Filer.Types,
  Generics.Collections,
{$IFDEF MSWINDOWS}
  Windows, ActiveX,
{$ENDIF}
  Deget.CoreTypes, BBYaml.Writer, XMLDoc, XMLIntf;

type
  TPackageData = class
  public
    FileName: string;
    Data: TPackageReadData;

    constructor Create(const aFileName: string);
    destructor Destroy; override;
  end;

  TPackageDataList = TObjectDictionary<string, TPackageData>;

  TPackageManager = class
  private
    FExes: TList<string>;
    FPackages: TObjectOrderedDictionary<string, TPackageDataList>;
    function HasUnsolvedDependencies(const Deps: THashSet<string>; const AllPackages: TObjectOrderedDictionary<string, THashSet<string>>; const SortedPackages: TOrderedDictionary<string, bool>): boolean;
  public
    property Exes: TList<string> read FExes;
    property Packages: TObjectOrderedDictionary<string,TPackageDataList> read FPackages;

    procedure Load;
    procedure LoadPackageData(const RootFolder: string);
    function SortPackages(const RootFolder: string): TArray<string>;

    function GetPackageData(const DProj: string): TPackageReadData;

    constructor Create;
    destructor Destroy; override;
  end;

var
  Interactive: boolean;
  Template: string;
  Specs: TArray<string>;
  UseJSON: boolean;
  CmdSyntax: boolean;

function Question(const Ask, Description: string; const DefaultAnswer: string = ''): string;
begin
  if not Interactive then exit(DefaultAnswer);
  if Description <> '' then Logger.Message(TLogMessageKind.Comment, Description);
  Logger.Message(TLogMessageKind.Question, Ask, false, false);
  if DefaultAnswer <> '' then Logger.Message(TLogMessageKind.Caption, ' (' + DefaultAnswer + ')', false, false);
  Logger.Message(TLogMessageKind.Question, ':');
  Readln(Result);
  Result := Result.Trim;
  if Result = '' then Result := DefaultAnswer;

end;

function Join(const Options: TArray<String>): string;
begin
  Result := '';
  for var i :=Low(Options) to High(Options) do
  begin
    if Result <> '' then
    begin
      if i < High(Options) then Result := Result + ', ' else Result := Result + ' or ';
    end;
    Result := Result + '''' + Options[i] + '''';
  end;
end;

function QuestionChoose(const Ask, Description: string; const DefaultAnswer: string; const Options: TArray<String>): String;
begin
  while True do
  begin
    var Answer := Question(Ask, Description, DefaultAnswer).Trim.ToLowerInvariant;
    for var c in Options do if(SameText(Answer, c)) then exit(c);
    Logger.Message(TLogMessageKind.Text, 'Please answer ' + Join(Options));
    if not Interactive then raise Exception.Create('Internal error: There is no default option for non-interactive mode.');
  end;
end;

function QuestionNumber(const Ask, Description: string; const DefaultAnswer: integer; const Options: TArray<String>): integer;
begin
  for var i := Low(Options) to High(Options) do
  begin
    Logger.Message(TLogMessageKind.Caption, IntToStr(i + 1) + '. ' + Options[i]);
  end;
  while True do
  begin
    var Answer := Question(Ask, Description, IntToStr(DefaultAnswer)).Trim.ToLowerInvariant;
    Result := -1;
    if TryStrToInt(Answer, Result) and (Result > 0) and (Result <= Length(Options)) then exit;
    Logger.Message(TLogMessageKind.Text, 'Please write a number between 1 and ' + IntToStr(Length(Options)));
    if not Interactive then raise Exception.Create('Internal error: There is no default option for non-interactive mode.');
  end;
end;


function DefaultVar(const v1, v2: string): string;
begin
  if v1.Trim = '' then exit(v2);
  exit(v1);
end;

function GetIDE(const IDE: string): TIDEName;
begin
  for var i := Low(TIDEName) to High(TIDEName) do if SameText(IDEId[i], IDE) then exit(i);
  exit(High(TIDEName));
end;

function GetBestDelphi(const ProjectVersion: string): TIDEName;
const
  //Just so it doesn't compile when we add a new version.
  //Make sure to review the code in this function every time there is a new delphi release.
  Dummy: Array[TIDEName] of bool =
    (true, true, true, true, true, true, true, true, true, true, true, true,
     true, true, true, true, true, true, true, true, true, true, true, true);
begin
  var Version: TVersion;
  if not TVersion.TryFromString(ProjectVersion, Version) then exit(High(TIDEName));
  //From https://delphi.fandom.com/wiki/How_to_find_out_which_Delphi_version_was_used_to_create_a_project%3F
  if ProjectVersion < TVersion('13.0') then exit(TIDEName.delphixe);
  if ProjectVersion < TVersion('14.0') then exit(TIDEName.delphixe2);
  if ProjectVersion < TVersion('14.4') then exit(TIDEName.delphixe3);
  if ProjectVersion < TVersion('15.0') then exit(TIDEName.delphixe4);
  if ProjectVersion < TVersion('15.4') then exit(TIDEName.delphixe5);
  if ProjectVersion < TVersion('16.0') then exit(TIDEName.delphixe6);
  if ProjectVersion < TVersion('17.0') then exit(TIDEName.delphixe7);
  if ProjectVersion < TVersion('18.0') then exit(TIDEName.delphixe8);
  if ProjectVersion < TVersion('18.1') then exit(TIDEName.delphiseattle);
  if ProjectVersion < TVersion('18.2') then exit(TIDEName.delphiberlin);
  if ProjectVersion < TVersion('18.5') then exit(TIDEName.delphitokyo);
  if ProjectVersion < TVersion('19') then exit(TIDEName.delphirio);
  if ProjectVersion < TVersion('19.3') then exit(TIDEName.delphisydney);
  if ProjectVersion < TVersion('20.0') then exit(TIDEName.delphi11);
  if ProjectVersion < TVersion('20.3') then exit(TIDEName.delphi12);

  Result := High(TIDEName); //TIDEName.delphi13;

end;

function GetIdeSince(const AppStr, ProjectVersion: string): TIDEName;
begin
  var DelphiVersions: TArray<string> := nil;
  SetLength(DelphiVersions, Length(IDEId) - ord(TIDEName.delphixe));
  for var IDEIndex := TIDEName.delphixe to High(TIDEName) do DelphiVersions[ord(IDEIndex) - ord(TIDEName.delphixe)] := IDEId[IDEIndex];

  Result := GetIDE(QuestionChoose('Minimum Rad Studio supported by the ' + AppStr, 'Write one of ' + Join(DelphiVersions), IDEId[GetBestDelphi(ProjectVersion)], DelphiVersions));
end;

procedure ConfigureExe(const Product: TProjectDefinition; const AppDproj: string; const PackageData: TPackageReadData);
begin
  var FrameworkName := PackageData.FrameworkType.ToLower.Trim;
  if (FrameworkName = '') or (FrameworkName = 'none') then FrameworkName := 'app';

  Product.RegisterFramework(FrameworkName, '');
  var Framework := Product.GetFramework(FrameworkName);

  Framework.IdeSince := GetIdeSince('App', PackageData.ProjectVersion);

  Framework.Platforms := PackageData.SupportedPlatforms;
  Framework.SupportsCppBuilder := PackageData.CBuilderOutputMode <> TCBuilderOutputMode.None;

  Product.Packages.Add(TPackage.Create(TPath.GetFileNameWithoutExtension(AppDProj)));
  var Pack := Product.Packages[0];
  Pack.PackageType := TPackageType.Exe;
  Pack.Frameworks := [0];
end;

function ActualPlatforms(const Platforms: TPlatformSet; const Usage: TPackageUsage; const IDEName: TIDEName): TPlatformSet;
begin
  if Usage = TPackageUsage.Runtime then exit(Platforms);
  Result := [];
  if (TPlatform.win32intel in Platforms) then Result := Result + [TPlatform.win32intel];
  if IDEName < TIDEName.delphi12 then exit;
  if (TPlatform.win64intel in Platforms) then Result := Result + [TPlatform.win64intel];
end;

function RegisterFramework(const Product: TProjectDefinition; const BaseFrameworkName: string; const PackageData: TPackageReadData; const IdeSince: TIDEName): string;
begin
  var Index := -1;
  while True do
  begin
    inc(Index);
    var FrameworkName := BaseFrameworkName;
    if (Index > 0) then FrameworkName := FrameworkName + IntToStr(Index);

    if Product.HasFramework(FrameworkName) then
    begin
      var Framework := Product.GetFramework(FrameworkName);
//      if (ActualPlatforms(Framework.Platforms, PackageData.Usage, IDESince) = PackageData.SupportedPlatforms)
      if (Framework.Platforms = PackageData.SupportedPlatforms)
        and (Framework.SupportsCppBuilder = (PackageData.CBuilderOutputMode <> TCBuilderOutputMode.None))
        then exit(FrameworkName);
    end else
    begin
      Product.RegisterFramework(FrameworkName, '');
      var Framework := Product.GetFramework(FrameworkName);
      Framework.IdeSince := IDESince;

      Framework.Platforms := PackageData.SupportedPlatforms;
      Framework.SupportsCppBuilder := PackageData.CBuilderOutputMode <> TCBuilderOutputMode.None;
      exit(FrameworkName);
    end;
  end;
end;

function FindPackage(const PackageName: string; const Packages: TListOfPackages): TPackage;
begin
  for var Pack in Packages do
  begin
    if SameText(Pack.Name, PackageName) then exit(Pack);
    
  end;

  Packages.Add(TPackage.Create(PackageName));
  Result := Packages.Last;

end;

procedure ConfigurePackages(const Product: TProjectDefinition; const SortedPackages: TArray<string>; const Packages: TDictionary<string, TPackageData>);
begin
  var IDESince := TIDEName(-1);
  for var SortedPackage in SortedPackages do
  begin
    var Package: TPackageData := nil;
    if not Packages.TryGetValue(SortedPackage, Package) then continue;

    var FrameworkName := Package.Data.FrameworkType.ToLower.Trim;
    if (FrameworkName = '') or (FrameworkName = 'none') then FrameworkName := 'pkg';
    if (IDESince = TIDEName(-1)) then IDESince := GetIdeSince('Packages', Package.Data.ProjectVersion);
    var FinalFrameworkName := RegisterFramework(Product, FrameworkName, Package.Data, IDESince);

    var Pack := FindPackage(TPath.GetFileNameWithoutExtension(Package.FileName), Product.Packages);
    Pack.PackageType := TPackageType.Package;
    case Package.Data.Usage of
      TPackageUsage.Runtime: Pack.IsRuntime := true;
      TPackageUsage.DesignTime : Pack.IsDesign := true;
      TPackageUsage.RuntimeAndDesignTime: begin Pack.IsRuntime := true; Pack.IsDesign := true; end;
    end;

    Pack.Frameworks := Pack.Frameworks + [Product.GetFramework(FinalFrameworkName).Id];
  end;
end;

function SimilarPackages(const Packages1, Packages2: TPackageDataList): boolean;
begin
  for var p1 in Packages1 do
  begin
    if Packages2.ContainsKey(p1.Key)then exit(true);
  end;
  Result := false;
end;

function GetRoot(const PkgFolder: string; const Packages: TObjectOrderedDictionary<string, TPackageDataList>): string;
begin
  var RootFolder := TPath.GetDirectoryName(PkgFolder);
  for var OtherPackage in Packages.Keys do
  begin
    if OtherPackage = PkgFolder then continue;
    if (TPath.GetDirectoryName(OtherPackage) = RootFolder) and SimilarPackages(Packages[PkgFolder], Packages[OtherPackage]) then exit(RootFolder);
  end;
  Result := PkgFolder;
end;

procedure SelectPackages(const Product: TProjectDefinition; const PackageManager: TPackageManager);
begin
  var Folders := TDictionary<string, bool>.Create;
  try
    for var PkgFolder in PackageManager.Packages.Keys do
    begin
      Folders.AddOrSetValue(GetRoot(PkgFolder, PackageManager.Packages), true);
    end;

    var PackageFolder := '';
    if Folders.Count = 1 then
    begin
      PackageFolder := Folders.Keys.ToArray[0];
    end
    else
    begin
      var Answer := QuestionNumber('Select folder with packages to use', 'We''ve found more than one folder with suitable packages', 0, Folders.Keys.ToArray) - 1;
      PackageFolder := Folders.Keys.ToArray[Answer];
    end;

    Logger.Message(TLogMessageKind.Text, 'Using Packages in folder ' + PackageFolder);
    PackageManager.LoadPackageData(PackageFolder);
    var SortedPackages := PackageManager.SortPackages(PackageFolder);
    for var Folder in PackageManager.Packages do
    begin
      if Folder.Key.StartsWith(PackageFolder) then
      begin
        ConfigurePackages(Product, SortedPackages, Folder.Value);
      end;
    end;


  finally
    Folders.Free;
  end;
end;

procedure AskQuestions(const Product: TProjectDefinition; const PackageManager: TPackageManager);
begin
  Product.Application.Id := Question('Id for the product',
     'The product id is an unique identifier that Smart Setup will use to refer to your product'#10
   + 'The preferred format is of the type "organization.product.name"', DefaultVar(Product.Application.Id, 'product_id'));

  Product.Application.Name := Question('Product name',
      '', DefaultVar(Product.Application.Name, 'product_name'));

  Product.Application.Url := Question('Product URL',
      'If you are going to host this product in a server, this is the url where you can do a git clone of it.', DefaultVar(Product.Application.Url, ''));

  if (PackageManager.Exes.Count > 0) and (PackageManager.Packages.Count > 0) then
  begin
    var IsExe := QuestionChoose('Is this definition for an (E)xe or a (P)ackage', 'The folder contains dprojs for both Applications and Packages. You can either have a single exe, or multiple packages.', 'P', ['E', 'P']) = 'e';
    if IsExe then PackageManager.Packages.Clear else PackageManager.Exes.Clear;
  end;

  if (PackageManager.Exes.Count > 0) then
  begin
    var ExeIndex := 0;
    if PackageManager.Exes.Count > 1 then ExeIndex := QuestionNumber('Select the application to include', '', 1, PackageManager.Exes.ToArray) - 1;
    Logger.Message(TLogMessageKind.Text, 'Using Application ' + PackageManager.Exes[ExeIndex]);

    var PackageData := PackageManager.GetPackageData(PackageManager.Exes[ExeIndex]);
    try
      ConfigureExe(Product, PackageManager.Exes[ExeIndex], PackageData);
    finally
      PackageData.Free;
    end;
  end
  else
  begin
    SelectPackages(Product, PackageManager);
  end;


  var DepMsg := 'Enter a product id that this product depends on. You only need to write direct dependencies.';
  while (True) do
  begin
    var Dep := Question('Dependency (leave empty if no more)', DepMsg);
    if Dep.Trim = '' then break;
    DepMsg := '';
    Product.Dependencies.Add(TDependency.Create(Dep.Trim, Dep.Trim));
  end;
end;

procedure VerifyWeCanSave(const FileName: string);
begin
  TDirectory.CreateDirectory(TPath.GetDirectoryName(FileName));
  TFile.WriteAllText(FileName, 'tms setup writing test. this file is temporary.');
  TFile.Delete(FileName);
end;

function ChangeExtension(const FileName: string): string;
begin
  if UseJSON then exit(TPath.ChangeExtension(FileName, '.json'));
  if CmdSyntax then exit(TPath.ChangeExtension(FileName, '.cmd'));
  Result := FileName;
end;

procedure RunSpecCommand;
begin
  var Product: TProjectDefinition := nil;
  var PackageManager: TPackageManager := nil;
  try
    if Template = ''
      then Product := TProjectDefinition.Create('')
      else Product := TProjectLoader.LoadProjectDefinitionFromFile(Template, '', true, Specs);

    PackageManager := TPackageManager.Create;
    var TmsBuildFileName := TPath.Combine(TDirectory.GetCurrentDirectory, ChangeExtension('tmsbuild.yaml'));

    TmsBuildFileName := Question('File to be created: ',
      '', TmsBuildFileName);

    VerifyWeCanSave(TmsBuildFileName); //We don't want to ask all the questions and at the end crash because the folder is invalid.

    if Interactive then AskQuestions(Product, PackageManager);
    var SpecWriter := TSpecWriter.Create(Product);
    try
      var WritingFormat := TWritingFormat.Minimal;
      if CmdSyntax or UseJSON then WritingFormat := TWritingFormat.NoComments;

      SpecWriter.Save(TmsBuildFileName, WritingFormat, UseJSON, CmdSyntax);
    finally
      SpecWriter.Free;
    end;

    Logger.Message(TLogMessageKind.Conclusion, 'Spec file created at: "' + TmsBuildFileName + '"');

  finally
    PackageManager.Free;
    Product.Free;
  end;
end;

procedure RegisterSpecCommand;
begin
  Interactive := true;
  var cmd := TOptionsRegistry.RegisterCommand('spec', '', 'Creates a new tmsbuild.yaml file for a new product',
    'Creates a new tmsbuild file that can be used to adapt an existing product to be used with smartsetup.',
    'spec [<options>]');

  var optionNonInteractive := cmd.RegisterOption<Boolean>('non-interactive', '', 'Creates a default file and doesn''t prompt for information. You will have to manually edit the file later, or provide the values with `-s:` parameters.',
    procedure(const Value : Boolean)
    begin
      Interactive := not Value;
    end);
  optionNonInteractive.HasValue := False;

  var optionTemplate := cmd.RegisterOption<string>('template', '', 'Loads the original values from an existing tmsbuild.yaml',
    procedure(const Value : String)
    begin
      Template := Value;
    end);

  var optionSpec := cmd.RegisterOption<string>('spec', 's', 'This is similar to the -p parameter, but it modifies the values in tmsbuild.yaml instead of tms.config.yaml',
    procedure(const Value : String)
    begin
      Specs := Specs + [Value];
    end);

  UseJSON := false;
  var optionJSON := cmd.RegisterOption<Boolean>('json', '', 'Saves the file in JSON format. SmartSetup won''t be able to read it, but you can use the file to query values using a JSON parser.',
    procedure(const Value : Boolean)
    begin
      UseJSON := Value;
    end);
  optionJSON.HasValue := False;

  CmdSyntax := false;
  var optionCmd := cmd.RegisterOption<Boolean>('cmd', '', 'Saves the file with the commands you need to regenerate the file. SmartSetup won''t be able to read it, but you can use the file to see what "-s" parameters to pass.',
    procedure(const Value : Boolean)
    begin
      CmdSyntax := Value;
    end);
  optionCmd.HasValue := False;


  AddCommand(cmd.Name, CommandGroups.Config, RunSpecCommand);
end;

{ TPackageManager }

constructor TPackageManager.Create;
begin
{$IFDEF MSWINDOWS}
  CoInitialize(nil);
{$ENDIF}
  FExes := TList<string>.Create;
  FPackages := TObjectOrderedDictionary<string, TPackageDataList>.Create([doOwnsValues]);
  Load;
end;

destructor TPackageManager.Destroy;
begin
  FExes.Free;
  FPackages.Free;
{$IFDEF MSWINDOWS}
  CoUninitialize;
{$ENDIF}
  inherited;
end;

function TPackageManager.GetPackageData(const DProj: string): TPackageReadData;
begin
  var Reader := TDprojReader.Create(DProj, TIDEName.lazarus);
  try
    var DData := TPackageReadData.Create;
    Reader.ReadData(DData);
    exit(DData);
  finally
    Reader.Free;
  end;
end;

function IsExe(const F: string): boolean;
begin
{This code works, but it is slow
  var Xml: IXmlDocument := TXMLDocument.Create(nil);
  Xml.LoadFromFile(F);
  var Project := Xml.ChildNodes.FindNode('Project');
  if Project = nil then raise Exception.Create('Invalid dproj file');

  for var pg := 0 to Project.ChildNodes.Count - 1 do
  begin
    var PropertyGroup := Project.ChildNodes[pg];
    if PropertyGroup.LocalName <> 'PropertyGroup' then continue;
    var AppType := PropertyGroup.ChildNodes.FindNode('AppType');
    if (AppType <> nil) then
    begin
      if AppType.NodeValue = 'Package' then exit(false);
      if (AppType.NodeValue = 'Application') or (AppType.NodeValue = 'Console') or (AppType.NodeValue = 'Library') then exit(true);
      raise Exception.Create('Unknown project type: ' + AppType.NodeValue);
    end;
  end;
  raise Exception.Create('No apptype in file');
}
  Result := TFile.Exists(TPath.ChangeExtension(F, '.dpr'));
end;

procedure TPackageManager.Load;
begin
  var Files := TDirectory.GetFiles(TDirectory.GetCurrentDirectory, '*.dproj', TSearchOption.soAllDirectories);
  if Length(Files) = 0 then raise Exception.Create('The folder "' + '" doesn''t containt any .dproj file on any of its subfolders. Run this command from a folder that has .dproj files somewhere below it.');

  for var F in Files do
  begin
    var FileIsExe: boolean;
    try
      FileIsExe := IsExe(F);
    except
      on ex: Exception do
      begin
        Logger.Error('Error in file: ' + F + ' ' + ex.Message);
        continue;
      end;
    end;

    if FileIsExe then
    begin
      FExes.Add(F);
    end else
    begin
      var Pack := TPath.GetDirectoryName(F);
      var ExistingPackages: TPackageDataList;
      if not FPackages.TryGetValue(Pack, ExistingPackages) then
      begin
        ExistingPackages := TPackageDataList.Create([doOwnsValues]);
        FPackages.Add(Pack, ExistingPackages);
      end;
      ExistingPackages.Add(TPath.GetFileNameWithoutExtension(F), TPackageData.Create(F));
    end;
  end;

end;

procedure TPackageManager.LoadPackageData(const RootFolder: string);
begin
  for var Folder in Packages do
  begin
    if Folder.Key.StartsWith(RootFolder) then
    begin
      for var Value in Folder.Value.Values do
      begin
        Value.Data := GetPackageData(Value.FileName);
      end;
    end;
  end;
end;

function TPackageManager.HasUnsolvedDependencies(const Deps: THashSet<string>; const AllPackages: TObjectOrderedDictionary<string, THashSet<string>>; const SortedPackages: TOrderedDictionary<string, bool>): boolean;
begin
  for var dep in Deps do
  begin
    if not AllPackages.ContainsKey(dep) then continue;
    if not SortedPackages.ContainsKey(dep) then exit(true);
  end;
  Result := false;
end;

function TPackageManager.SortPackages(const RootFolder: string): TArray<string>;
begin
  Result := nil;
  var AllPackages := TObjectOrderedDictionary<string, THashSet<string>>.Create([doOwnsValues]);
  try
    for var Folder in Packages do
    begin
      if not Folder.Key.StartsWith(RootFolder) then continue;
      for var Value in Folder.Value.Values do
      begin
        var PkgList: THashSet<string> := nil;
        if not AllPackages.TryGetValue(TPath.GetFileNameWithoutExtension(Value.FileName), PkgList) then
        begin
          PkgList := THashSet<string>.Create;
          AllPackages.Add(TPath.GetFileNameWithoutExtension(Value.FileName), PkgList);
        end;
        for var dep in Value.Data.DcpFiles do PkgList.Add(TPath.GetFileNameWithoutExtension(dep.FileName));
      end;
    end;

    var SortedPackages := TOrderedDictionary<string, bool>.Create;
    try
      while SortedPackages.Count < AllPackages.Count do
      begin
        var OldCount := SortedPackages.Count;
        for var Pack in AllPackages do
        begin
          if SortedPackages.ContainsKey(Pack.Key) then continue;
          if not HasUnsolvedDependencies(Pack.Value, AllPackages, SortedPackages) then SortedPackages.Add(Pack.Key, true);
        end;
        if OldCount = SortedPackages.Count then raise Exception.Create('Cannot solve package dependencies.');

      end;

      Result := SortedPackages.Keys.ToArray;
    finally
      SortedPackages.Free;
    end;
  finally
    AllPackages.Free;
  end;

end;

{ TPackageData }

constructor TPackageData.Create(const aFileName: string);
begin
  FileName := aFileName;
end;

destructor TPackageData.Destroy;
begin
  Data.Free;
  inherited;
end;

end.

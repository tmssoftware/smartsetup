unit UFileHasher;
{$i ../../tmssetup.inc}

interface
uses UProjectDefinition, UConfigDefinition, Hash, SysUtils, Types, Generics.Defaults,
     Generics.Collections, UMultiLogger, UNaming, Deget.CoreTypes, UPackageCache, UPersistence,
     UFileSystemPersistence, Deget.IDEInfo, Deget.PackageConfig, Deget.Filer.Types, Deget.Filer.ProjectFactory;
type
  TProductHash = record
  private
    const SerializedSeparator = #10;
  public
    SourceCodeHash: string;
    BinaryHash: string;
    constructor Create(const aSourceCodeHash, aBinaryHash: string);
    constructor CreateFromSerialized(const Serialized: string);

    function Serialize: string;
    class operator Equal(a, b: TProductHash) : Boolean;
    class operator NotEqual(a, b: TProductHash) : Boolean;
    function Empty: boolean;
    function HasAllHashes: boolean;
  end;

  TFileHasher = class
  private
  const
    HashExtension = '.hash';
  var
    Persist: TPersistence;
    Config: TConfigDefinition;
  private

    function HashMultipleFiles(const ExtraData: string; const Files: array of TArray<string>): String;
    function GetBestProject(const PackageCache: TPackageCache; const Project: TProjectDefinition;
      const Package: TPackage; const Naming: TNaming; out IDEName: TIDEName): string;
    procedure HashBinaryFolder(const Hasher: THashSHA2; const Folder: string);
    function GetAllDpks(const PackageCache: TPackageCache;
      const Project: TProjectDefinition; const Packages: TObjectList<TPackage>;
      const Naming: TNaming): TArray<string>;
    function GetFilesFromProject(const ProjectFileName: string; const IDEName: TIDEName): TArray<string>;
{$IFDEF MSWINDOWS}
    procedure AddIncludedFiles<T: TIncludeFile, constructor>(var Result: TArray<string>; const Root: string;
      const Files: TIncludeFiles<T>; var k: integer; const GetExtraExtensions: TFunc<T, TArray<string>> = nil);
    function GetFilesFromDProj(const ProjectFileName: string; const IDEName: TIDEName): TArray<string>;
    function GetFilesFromCBProj(const ProjectFileName: string;
      const IDEName: TIDEName): TArray<string>;
{$ENDIF}
  public
    constructor Create(const aConfig: TConfigDefinition);
    destructor Destroy; override;

    function GetStoredHash(
      const Project: TProjectDefinition; const IDE: TIDEName; const Platform: TPlatform; const Package: string): TProductHash;

    function GenerateSourceCodeHash(const PackageCache: TPackageCache;
      const Project: TProjectDefinition): string;

{$IFDEF MSWINDOWS}
    function GenerateBinaryHash(const Project: TProjectDefinition; const PackagesFolder: string; PlatformInfo: IDelphiPlatformInfo): string;
{$ENDIF}

    procedure SaveHash(const Project: TProjectDefinition;
            ProductHash: TProductHash; const IDE: TIDEName; const Platform: TPlatform; const Package: string);

    procedure CleanHashes;

    procedure RemoveHashesForPlatform(const ProjectId, IdeId,
      PlatformId: string);

    function ContainsHash(const ProductHash: string; const Project: TProjectDefinition;
             const IDE: TIDEName; const Platform: TPlatform): boolean;

    function ProductModified(const ProductHash: TProductHash;
             const Project: TProjectDefinition; const Config: TConfigDefinition;
             const dv: TIDEName; const dp: TPlatform; const Package: string): boolean;

  end;
implementation
uses IOUtils, Classes, Deget.DpkFileFormat, UPackageFinder,
     UTmsBuildSystemUtils, UAppTerminated,
     Deget.DelphiInfo, Deget.Filer.DprojFile, Deget.Filer.CBProjFile;

type TBinaryFileHash = record
  Name: string;
  Size: Int64;

  constructor Create(const aName: string; const aSize: Int64);
  function ToString: string;
end;


{ TFileHasher }

constructor TFileHasher.Create(const aConfig: TConfigDefinition);
begin
  Config := aConfig;
  Persist := TFileSystemPersistence.Create(Config.Folders.HashesFolder, HashExtension);
end;

destructor TFileHasher.Destroy;
begin
  Persist.Free;

  inherited;
end;


function TFileHasher.HashMultipleFiles(const ExtraData: string; const Files: array of TArray<string>): String;
const
  BUFFERSIZE = 4096;
var
  Buffer: TBytes;
  Hasher: THashSHA2;
  BytesRead: Longint;
  Stream: TFileStream;
  i, k: Integer;
begin
  var UsedFiles := TDictionary<string, boolean>.Create(); //do not enter a file twice.
  try
    //Depending on the input, we might need to sort here.
    //We are getting the list from the packages, so no need as
    //it will not change. But a TDirectory.GetFiles might indeed change everytime.

    Hasher := THashSHA2.Create;
    SetLength(Buffer, BUFFERSIZE);

    for i := Low(Files) to High(Files) do
    begin
      for k := Low(Files[i]) to High(Files[i]) do
      begin
        CheckAppTerminated;
        if UsedFiles.ContainsKey(Files[i, k]) then continue;
        UsedFiles.Add(Files[i, k], true);
        Stream := TFileStream.Create(Files[i, k], fmOpenRead or fmShareDenyNone);
        try
          while True do
          begin
            BytesRead := Stream.ReadData(Buffer, BUFFERSIZE);
            if BytesRead = 0 then
              Break;
            Hasher.Update(Buffer, BytesRead);
          end;
        finally
          Stream.Free;
        end;
      end;
    end;
  finally
    UsedFiles.Free;
  end;

  Hasher.Update(ExtraData);

  Result := Hasher.HashAsString;
end;


function TFileHasher.ProductModified(const ProductHash: TProductHash;
  const Project: TProjectDefinition;
  const Config: TConfigDefinition; const dv: TIDEName; const dp: TPlatform;
  const Package: string): boolean;
begin
  var StoredHash := GetStoredHash(Project, dv, dp, Package);
  if ProductHash.Empty then exit(True);
  if StoredHash.Empty then exit(True);

  Result := ProductHash <> StoredHash;

end;

function TFileHasher.GetBestProject(const PackageCache: TPackageCache; const Project: TProjectDefinition; const Package: TPackage; const Naming: TNaming; out IDEName: TIDEName): string;
begin
{$message 'review for better laz support.Project we could read lazarus packages and delphi packages, but we will need to parse the lpk'}
  IDEName := TIDEName.delphi6;

  for var dv := High(TIDEName) downto Low(TIDEName) do
  begin
    if dv = TIDEName.lazarus then continue; // for products supporting lazarus, we will anyway look in the delphi projs.

    if not Project.SupportsIDE(dv) then continue;

    Result := TPackageFinder.GetProjectToBuild(PackageCache, dv, Project, Package, Naming, false, Project.FileNameExtension);
    if (Result <> '') then
    begin
      IDEName := dv;
      exit;
    end;
  end;

  Result := '';
end;

function TFileHasher.GetAllDpks(const PackageCache: TPackageCache; const Project: TProjectDefinition; const Packages: TObjectList<TPackage>; const Naming: TNaming): TArray<string>;
begin
  var ResultList := TList<string>.Create;
  try
    for var i := 0 to Packages.Count - 1 do
    begin
      var Package := Packages[i];
      for var dv := High(TIDEName) downto Low(TIDEName) do
      begin
        if dv = TIDEName.lazarus then continue; // for products supporting lazarus, we will anyway look in the delphi projs.
        if not Project.SupportsIDE(dv) then continue;

        var ProjectFileName := TPackageFinder.GetProjectToBuild(PackageCache, dv, Project, Package, Naming, false, Project.FileNameExtension);
        if (ProjectFileName <> '') then
        begin
          ResultList.Add(ProjectFileName);
          if TPath.GetExtension(ProjectFileName) = '.dproj' then
          begin
            var DProj := TPath.ChangeExtension(ProjectFileName, '.dpr');
            if TFile.Exists(Dproj) then ResultList.Add(DProj);
          end;

        end;
      end;
    end;
    Result := ResultList.ToArray;
  finally
    ResultList.Free;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TFileHasher.AddIncludedFiles<T>(var Result: TArray<string>; const Root: string;
  const Files: TIncludeFiles<T>; var k: integer; const GetExtraExtensions: TFunc<T, TArray<string>>);
begin
  for var i := 0 to Files.Count - 1 do
  begin
    var FileName := TPath.GetFullPath(TPath.Combine(Root, Files[i].FileName));
    {.$IFNDEF MSWINDOWS}
    FileName := FileName.Replace('\', '/');
    {.$ENDIF}
    Result[k] := FileName;
    inc(k);

    if Assigned(GetExtraExtensions) then
    begin
      for var ExtraExtension in GetExtraExtensions(Files[i]) do
      begin
        if (ExtraExtension <> '') and (TFile.Exists(TPath.ChangeExtension(FileName, ExtraExtension))) then
        begin
          Result[k] := TPath.ChangeExtension(FileName, ExtraExtension);
          inc(k);
        end;
      end;
    end;
  end;

end;

function TFileHasher.GetFilesFromDProj(const ProjectFileName: string; const IDEName: TIDEName): TArray<string>;
begin
  var PackData := TDelphiProjectFactory.GetPackageReadData(ProjectFileName, IDEName);
  Result := nil;
  SetLength(Result, PackData.RcFiles.Count + PackData.DcrFiles.Count
           + PackData.PasFiles.Count * 2 + PackData.NoneFiles.Count);

   var Root := TPath.GetDirectoryName(ProjectFileName);
   var k := 0;
   AddIncludedFiles<TPasIncludeFile>(Result, Root, PackData.RcFiles, k);
   AddIncludedFiles<TIncludeFile>(Result, Root, PackData.DcrFiles, k);
   AddIncludedFiles<TPasIncludeFile>(Result, Root, PackData.PasFiles, k,
     function(Item: TPasIncludeFile): TArray<string>
     begin
       Result := ['.dfm'];
     end);
   AddIncludedFiles<TIncludeFile>(Result, Root, PackData.NoneFiles, k);
   SetLength(Result, k);
end;

function TFileHasher.GetFilesFromCBProj(const ProjectFileName: string; const IDEName: TIDEName): TArray<string>;
begin
  var PackData := TDelphiProjectFactory.GetPackageReadData(ProjectFileName, IDEName)  as TCBProjPackageReadData;
  Result := nil;
  SetLength(Result, PackData.RcFiles.Count
           + PackData.DcrFiles.Count
           + PackData.PasFiles.Count * 2
           + PackData.NoneFiles.Count
           + PackData.CppFiles.Count * 3
           + PackData.AsmFiles.Count
           //+ PackData.PackageImportFiles.Count
           //+ PackData.PCHFiles.Count
           //+ PackData.LibFiles.Count
           + PackData.DefFiles.Count
           //+ PackData.ObjFiles.Count
           + PackData.ResFiles.Count
           );

   var Root := TPath.GetDirectoryName(ProjectFileName);
   var k := 0;
   AddIncludedFiles<TPasIncludeFile>(Result, Root, PackData.RcFiles, k);
   AddIncludedFiles<TIncludeFile>(Result, Root, PackData.DcrFiles, k);
   AddIncludedFiles<TPasIncludeFile>(Result, Root, PackData.PasFiles, k,
     function(Item: TPasIncludeFile): TArray<string>
     begin
       Result := ['.dfm'];
     end);

   AddIncludedFiles<TIncludeFile>(Result, Root, PackData.NoneFiles, k);

   AddIncludedFiles<TCppIncludeFile>(Result, Root, PackData.CppFiles, k,
     function(Item: TCppIncludeFile): TArray<string>
     begin
       Result := [TPath.GetFileName(Item.HeaderFile), '.dfm'];
     end);

   AddIncludedFiles<TIncludeFile>(Result, Root, PackData.AsmFiles, k);
//       AddIncludedFiles<TIncludeFile>(Result, Root, PackData.PackageImportFiles, k);
//       AddIncludedFiles<TIncludeFile>(Result, Root, PackData.PCHFiles, k);
//       AddIncludedFiles<TIncludeFile>(Result, Root, PackData.LibFiles, k);
   AddIncludedFiles<TIncludeFile>(Result, Root, PackData.DefFiles, k);
//       AddIncludedFiles<TIncludeFile>(Result, Root, PackData.ObjFiles, k);
   AddIncludedFiles<TIncludeFile>(Result, Root, PackData.ResFiles, k);

   SetLength(Result, k);

end;
{$ENDIF}


function TFileHasher.GetFilesFromProject(const ProjectFileName: string; const IDEName: TIDEName): TArray<string>;
begin
  var ext := TPath.GetExtension(ProjectFileName).ToLowerInvariant;
  if ext = '.dpr' then exit(GetFilesFromDpk(ProjectFileName, TPath.GetDirectoryName(ProjectFileName), true));
{$IFDEF MSWINDOWS}
  if ext = '.dproj' then exit(GetFilesFromDproj(ProjectFileName, IDEName));
  if ext = '.cbproj' then exit(GetFilesFromCBproj(ProjectFileName, IDEName));
{$ELSE}
  if (ext = '.dproj') then exit(GetFilesFromDpk(TPath.ChangeExtension(ProjectFileName, '.dpk'), TPath.GetDirectoryName(ProjectFileName), true));
{$ENDIF}
  if ext = BinprojExtension then exit(nil);
  

  raise Exception.Create('Unknown package extension in file: ' + ProjectFileName);
end;

function TFileHasher.GenerateSourceCodeHash(const PackageCache: TPackageCache;
  const Project: TProjectDefinition): string;
var
  ListFiles: TArray<TArray<string>>;
begin
  var Packages := Project.Packages;

  SetLength(ListFiles, Packages.Count + 2);
  for var i := 0 to Project.Packages.Count - 1 do
  begin
    var IDEName: TIDEName;
    var ProjectFileName := GetBestProject(PackageCache, Project, Packages[i], Config.GetNaming(Project.Naming, Project.FullPath), IDEName);
    if ProjectFileName <> '' then //we eat the errors here, but it doesn't matter since if this doesn't work it will crash later with a better error message. And the worse if this doesn't work is that the hash will be invalid.
    begin
      ListFiles[i] := GetFilesFromProject(ProjectFileName, IDEName);
    end;
  end;

  // We won't add the config file to the hash, because you might change it for example
  // to add a new delphi version and you don't want to recompile everything because of that.
  // So we will only add the settings from the config file that require a rebuild, like adding rtti.
  // The project definition file shouldn't change, so there is no need to add it, but just in case.
   ListFiles[Length(ListFiles) - 2] := [Project.FullPath];

   ListFiles[Length(ListFiles) - 1] := GetAllDpks(PackageCache, Project, Packages, Config.GetNaming(Project.Naming, Project.FullPath));

  //we can send duplicated files here, HashMultipleFiles will ignore them.
  Result := HashMultipleFiles(Config.GetSettingsThatNeedRecompile(Project.Application.Id), ListFiles);
end;

{$IFDEF MSWINDOWS}
function TFileHasher.GenerateBinaryHash(const Project: TProjectDefinition; const PackagesFolder: string; PlatformInfo: IDelphiPlatformInfo): string;
begin
  var Hasher := THashSHA2.Create;
  var DepPackInfo: IDelphiPackageInfo := TPackageConfig.Create('DummyPackName', Project.RootFolder, PlatformInfo,
     PackagesFolder, Project.IsExe, '.dummyext');

  HashBinaryFolder(Hasher, DepPackInfo.ExpandedDcuOutputDir('Debug'));
  HashBinaryFolder(Hasher, DepPackInfo.ExpandedDcuOutputDir('Release'));
  Result := Hasher.HashAsString;
end;
{$ENDIF}

procedure TFileHasher.HashBinaryFolder(const Hasher: THashSHA2; const Folder: string);
var
  F: SysUtils.TSearchRec;
begin
  //For performance reasons, we won't hash the full files, just name and size. Date is not simple, so we won't care.
  var Files := TList<TBinaryFileHash>.Create;
  try
    if SysUtils.FindFirst(TPath.Combine(Folder, '*'), faAnyFile, F) = 0 then begin
      try
        repeat
          if (F.Attr and faDirectory <> 0) then begin
            //currently not following subfolders.
          end else
          begin
            Files.Add(TBinaryFileHash.Create(F.Name, F.Size));
          end;
        until SysUtils.FindNext(F) <> 0;
      finally
        SysUtils.FindClose(F);
      end;
    end;

    Files.Sort(TComparer<TBinaryFileHash>.Construct(
      function (const L, R: TBinaryFileHash): integer
      begin
        Result := CompareStr(L.Name, R.Name, loInvariantLocale);
      end
    ));

     for var a in Files do Hasher.Update(a.ToString);

  finally
    Files.Free;
  end;


end;


procedure TFileHasher.CleanHashes;
begin
  Persist.RemoveAll;
end;

procedure TFileHasher.RemoveHashesForPlatform(const ProjectId, IdeId, PlatformId: string);
begin
  Persist.RemoveAndBelow(ProjectId, IdeId, PlatformId);
end;

function TFileHasher.GetStoredHash(const Project: TProjectDefinition;
    const IDE: TIDEName; const Platform: TPlatform; const Package: string): TProductHash;
begin
  Result := TProductHash.CreateFromSerialized(Persist.Retrieve(Project.Application.Id, IDEId[IDE], PlatformId[Platform], Package));
end;

procedure TFileHasher.SaveHash(const Project: TProjectDefinition; ProductHash: TProductHash; const IDE: TIDEName; const Platform: TPlatform; const Package: string);
begin
  Persist.Store(ProductHash.Serialize, Project.Application.Id, IDEId[IDE], PlatformId[Platform], Package);
end;

function TFileHasher.ContainsHash(const ProductHash: string; const Project: TProjectDefinition;
  const IDE: TIDEName; const Platform: TPlatform): boolean;
begin
  var Hashes := Persist.List(Project.Application.Id, IDEId[IDE], PlatformId[Platform]);
  for var f in Hashes do
  begin
    if f.Data = ProductHash then exit(true);
  end;
  Result := false;
end;


{ TProductHash }

constructor TProductHash.Create(const aSourceCodeHash, aBinaryHash: string);
begin
  SourceCodeHash := aSourceCodeHash;
  BinaryHash := aBinaryHash;
end;

constructor TProductHash.CreateFromSerialized(const Serialized: string);
begin
  var Idx := Serialized.IndexOf(SerializedSeparator);
  if (idx < 0) then
  begin
    SourceCodeHash := Serialized;
    BinaryHash := '';
    exit;
  end;
  SourceCodeHash := Serialized.Substring(0, Idx).Trim;
  BinaryHash := Serialized.Substring(Idx + Length(SerializedSeparator)).Trim;
end;

function TProductHash.Serialize: string;
begin
  Result := SourceCodeHash + SerializedSeparator + BinaryHash;
end;


function TProductHash.Empty: boolean;
begin
  Result := (SourceCodeHash = '') and (BinaryHash = '');
end;

function TProductHash.HasAllHashes: boolean;
begin
  Result := (SourceCodeHash <> '') and (BinaryHash <> '');
end;

class operator TProductHash.Equal(a, b: TProductHash): Boolean;
begin
  Result :=
        (a.SourceCodeHash = b.SourceCodeHash)
    and (a.BinaryHash = b.BinaryHash);
end;

class operator TProductHash.NotEqual(a, b: TProductHash): Boolean;
begin
  Result := not (a = b);
end;

{ TBinaryFileHash }

constructor TBinaryFileHash.Create(const aName: string;
  const aSize: Int64);
begin
  Name := aName;
  Size := aSize;
end;

function TBinaryFileHash.ToString: string;
begin
  Result := Name + #0 + IntToStr(Size);
end;

end.

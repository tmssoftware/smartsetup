unit UFileSystemPersistence;
{$i ../../tmssetup.inc}
interface
uses UPersistence, Types;

type

TFileSystemPersistence = class(TPersistence)
private
    FRootFolder: string;
    FFileExtension: string;
    const
     ProjectFileName = 'tmsbuild_project';

    function GetFullFileName(const Last: string;
      const Previous: array of string): string;
    function Join(const s: string): string;
    function CleanFileName(const s: string): string;
    function RemoveExtraInfo(const s: string): string;
    function GetFileName(const Project: string; const IDE: string; const Platform: string; const Package: string): string;
    function ReadContents(const FileName: string): string;
    function ProcessFiles(const files: TStringDynArray; const ProjFolder: string; const Previous: Array of string): TArray<TStrUninstallInfo>;
    function GetLevel(const s: string; const Previous: Array of string): string;
public
  property RootFolder: string read FRootFolder;
  property FileExtension: string read FFileExtension;

  constructor Create(const aRootFolder, aFileExtension: string);

  procedure Store(const Data: string; const Project: string; const IDE: string = ''; const Platform: string = ''; const Package: string = ''); override;
  function Retrieve(const Project: string; const IDE: string = ''; const Platform: string = ''; const Package: string = ''): string; override;
  procedure Remove(const Project: string; const IDE: string = ''; const Platform: string = ''; const Package: string = ''); override;
  procedure RemoveAndBelow(const Project: string; const IDE: string = ''; const Platform: string = ''); override;
  procedure RemoveAll(const ForceExcluded: boolean; out AllIncluded: boolean); override;
  function List(const Project: string; const IDE: string = ''; const Platform: string = ''): TArray<TStrUninstallInfo>; override;

end;

implementation
uses SysUtils, IOUtils, UTmsBuildSystemUtils, Generics.Collections, UMultiLogger, Commands.GlobalConfig;

{ TFileSystemPersistence }

function TFileSystemPersistence.CleanFileName(const s: string): string;
begin
  Result := '';
  for var c in s do
  begin
    if CharInSet(c, ['A'..'Z', 'a'..'z', '0'..'9']) then
    begin
      Result := Result + c;
    end
    else Result := Result + '_';
  end;
end;

constructor TFileSystemPersistence.Create(const aRootFolder, aFileExtension: string);
begin
  FRootFolder := aRootFolder;
  FFileExtension := aFileExtension;
end;

function TFileSystemPersistence.GetFileName(const Project, IDE, Platform,
  Package: string): string;
begin
  if Project = '' then raise Exception.Create('Internal error persisting data. Project cannot be empty');
  var main := IDE;
  if SameText(main, ProjectFileName) then raise Exception.Create('"' + ProjectFileName + '" is a reserved IDE Name.');
  if main = '' then main := ProjectFileName;

  Result := TPath.Combine(
               TPath.Combine(RootFolder, Project),
               main + Join(Platform) + Join(CleanFileName(Package)) + FileExtension
               );
end;

function TFileSystemPersistence.Join(const s: string): string;
begin
  if s = '' then exit('');
  exit ('.' + s);
end;

function TFileSystemPersistence.ReadContents(const FileName: string): string;
begin
  if not TFile.Exists(FileName) then exit('');
  Result := TFile.ReadAllText(FileName, TEncoding.UTF8);
end;

procedure TFileSystemPersistence.Remove(const Project, IDE, Platform,
  Package: string);
begin
  TryDeleteFileAndRemoveParentFolderIfEmpty(Config.Folders.LockedFilesFolder, GetFileName(Project, IDE, Platform, Package));
end;

procedure TFileSystemPersistence.RemoveAll(const ForceExcluded: boolean; out AllIncluded: boolean);
begin
  AllIncluded := true;
  if not TDirectory.Exists(RootFolder) then Exit;

  var Products := TDirectory.GetDirectories(RootFolder);
  for var Product in Products do
  begin
    if not ForceExcluded and not Config.IsIncluded(TPath.GetFileName(Product)) then
    begin
      AllIncluded := true;
      continue;
    end;
    
    var Files := TDirectory.GetFiles(Product, '*' + FileExtension, TSearchOption.soAllDirectories);
    for var f in Files do
    begin
      TryDeleteFileAndRemoveParentFolderIfEmpty(Config.Folders.LockedFilesFolder, f);
    end;
    System.SysUtils.RemoveDir(Product);
  end;
end;

procedure TFileSystemPersistence.RemoveAndBelow(const Project, IDE, Platform: string);
begin
  try
    if Project = '' then
    begin
      var AllIncluded: boolean;
      RemoveAll(false, AllIncluded);
      exit;
    end;

    var ProjFolder := TPath.Combine(RootFolder, Project);
    if not TDirectory.Exists(ProjFolder) then exit;

    var FileMask : string;
    if IDE = '' then
    begin
      FileMask := '*' + FileExtension;
    end else
    begin
      // we can't use '*' as in the other cases, because XE* would match XE2 too.
      FileMask := IDE + Join(Platform) + '.*' + FileExtension;
      TryDeleteFileAndRemoveParentFolderIfEmpty(Config.Folders.LockedFilesFolder, TPath.Combine(ProjFolder, IDE + Join(Platform) + FileExtension));

    end;


    var Files := TDirectory.GetFiles(ProjFolder, FileMask, TSearchOption.soTopDirectoryOnly);
    for var f in Files do
    begin
      TryDeleteFileAndRemoveParentFolderIfEmpty(Config.Folders.LockedFilesFolder, f);
    end;

  except on ex: Exception do
    Logger.Trace('Error removing hashes: ' + ex.Message);
  end;

end;

function TFileSystemPersistence.Retrieve(const Project, IDE, Platform,
  Package: string): string;
begin
  var f := GetFileName(Project, IDE, Platform, Package);
  Result := ReadContents(f);
end;

procedure TFileSystemPersistence.Store(const Data, Project, IDE, Platform,
  Package: string);
begin
  if Data = '' then exit;
  var f := GetFileName(Project, IDE, Platform, Package);
  TDirectory_CreateDirectory(TPath.GetDirectoryName(f));

  TFile.WriteAllText(GetFileName(Project, IDE, Platform, Package), Data, TEncoding.UTF8)
end;

function TFileSystemPersistence.RemoveExtraInfo(const s: string): string;
begin
  Result := TPath.GetFileName(s);
  if not Result.EndsWith(FileExtension) then raise Exception.Create('Internal error. The uninstall file extension is wrong.');
  Result := Result.Substring(0, Result.Length - FileExtension.Length);
end;

function TFileSystemPersistence.GetLevel(const s: string; const Previous: Array of string): string;
begin
  Result := s;

  var lvl := 0;
  while lvl < Length(Previous) do
  begin
    var idx := Result.IndexOf('.');
    if (idx < 0) or (idx = Result.Length - 1) then exit('');
    if Result.Substring(0, idx) <> Previous[lvl] then exit('');

    Result := Result.Substring(idx + 1);
    inc(lvl);
  end;

  var idx := Result.IndexOf('.');
  if idx > 0 then Result := Result.Substring(0, idx);
end;

function TFileSystemPersistence.GetFullFileName(const Last: string; const Previous: Array of string): string;
begin
  Result := String.Join('.', Previous);
  if (Result <> '') then Result := Result + '.';
  Result := Result + Last;
end;

function TFileSystemPersistence.ProcessFiles(const files: TStringDynArray; const ProjFolder: string; const Previous: Array of string): TArray<TStrUninstallInfo>;
begin
  Result := nil;
  var AlreadyUsed := TDictionary<string, boolean>.Create;
  try
    for var s in files do
    begin
      var f := RemoveExtraInfo(s);
      var f2 := GetLevel(f, Previous);
      if (f2 = '') or (f2 = ProjectFileName) then continue;
      if AlreadyUsed.ContainsKey(f2) then continue;
      AlreadyUsed.Add(f2, true);
    end;

    var k := 0;
    SetLength(Result, AlreadyUsed.Count);
    for var key in AlreadyUsed.Keys do
    begin
      Result[k] := TStrUninstallInfo.Create(key, ReadContents(TPath.Combine(ProjFolder, GetFullFileName(key, Previous) + FileExtension)));
      inc(k);
    end;
  finally
    AlreadyUsed.Free;
  end;

end;

function TFileSystemPersistence.List(const Project: string; const IDE: string = ''; const Platform: string = ''): TArray<TStrUninstallInfo>;
begin
  Result := nil;
  if not TDirectory.Exists(RootFolder) then exit;

  if Project = '' then
  begin
    var projs := TDirectory.GetDirectories(RootFolder);
    SetLength(Result, Length(projs));
    for var i := Low(Projs) to High(Projs) do
    begin
      Result[i] := TStrUninstallInfo.Create(TPath.GetFileName(projs[i]), ReadContents(TPath.Combine(projs[i], ProjectFileName + FileExtension)));
    end;
    exit;
  end;

  var ProjFolder := TPath.Combine(RootFolder, Project);
  if not TDirectory.Exists(ProjFolder) then exit(nil);

  var files := TDirectory.GetFiles(ProjFolder, '*' + FileExtension, TSearchOption.soTopDirectoryOnly);

  if IDE = '' then
  begin
    exit(ProcessFiles(files, ProjFolder, []));
  end;

  if Platform = '' then
  begin
    exit(ProcessFiles(files, ProjFolder, [IDE]));
  end;

  exit(ProcessFiles(files, ProjFolder, [IDE, Platform]));
end;



end.

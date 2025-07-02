unit Megafolders.Manager;
{$i ../../tmssetup.inc}

interface
uses Megafolders.Definition, Generics.Collections, Deget.CoreTypes, Deget.IDETypes;
type
  TMegafolderManager = class
  public
    class function GetFolderName(const Root: string; const IDEName: TIDEName; const Platform: TPlatform; const BuildConfig: TBuildConfig): string;
    class procedure UpdateFolder(const Source, Target: string; out Count: integer); static;
    class procedure UpdateFile(const Source, Target: string; var Count: integer); static;
    class procedure Cleanup(const Folder: string); static;
    class procedure CleanupAll(const BaseFolder: string; const UsedMegafolders: TUsedMegafolders); static;
    class procedure RemoveAll(const Folder: string); static;
    class procedure RemoveUnused(const BaseFolder: string; const Megafolders: TMegafolderList); static;

    class function MetaMegafolderFileName(const Folder: string): string;
    class function ChangedMegafolders(const NewMegafolders: TMegafolderList; const ConfigFolder: string): boolean; static;
    class procedure Save(const Megafolders: TMegafolderList; const ConfigFolder: string); static;
    class procedure Load(const Megafolders: TMegafolderList; const ConfigFolder: string); static;
  end;

implementation
uses Classes, SysUtils, IOUtils, UTmsBuildSystemUtils, Commands.GlobalConfig, UOSFileLinks, UMultiLogger;

{ TMegafolderManager }

class function TMegafolderManager.ChangedMegafolders(
  const NewMegafolders: TMegafolderList; const ConfigFolder: string): boolean;
begin
  var OldMegafolders := TMegafolderList.Create;
  try
    Load(OldMegafolders, ConfigFolder);
    Result := not OldMegafolders.Equals(NewMegafolders);
  finally
    OldMegafolders.Free;
  end;
end;

class procedure TMegafolderManager.Cleanup(const Folder: string);
var
  SearchRec: TSearchRec;
begin
  if SysUtils.FindFirst(TPath.Combine(Folder, '*'), faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if  (SearchRec.Attr and faDirectory = 0) and ((SearchRec.Attr and SysUtils.faSymLink) <> 0) then
        begin
          var FilePath := TPath.Combine(Folder, SearchRec.Name);
          if not SysUtils.FileExists(FilePath) then DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, FilePath);
        end;

      until SysUtils.FindNext(SearchRec) <> 0;
    finally
      SysUtils.FindClose(SearchRec);
    end;
  end;
end;

class procedure TMegafolderManager.CleanupAll(const BaseFolder: string;
  const UsedMegafolders: TUsedMegafolders);
begin
  if not TDirectory.Exists(BaseFolder) then exit;
  var NameFolders := TDirectory.GetDirectories(BaseFolder, '*', TSearchOption.soTopDirectoryOnly);
  for var NameFolder in NameFolders do
  begin
    var RelativeNameFolder := TPath.GetFileName(NameFolder);
    var DelphiVersionFolders := TDirectory.GetDirectories(NameFolder, '*', TSearchOption.soTopDirectoryOnly);
    for var DelphiVersionFolder in DelphiVersionFolders do
    begin
      var PlatformFolders := TDirectory.GetDirectories(DelphiVersionFolder, '*', TSearchOption.soTopDirectoryOnly);
      for var PlatformFolder in PlatformFolders do
      begin
        var RelativePlatformFolder := SysUtils.ExtractRelativePath(IncludeTrailingPathDelimiter(BaseFolder), PlatformFolder);
        if UsedMegafolders.UnsafeContains(RelativePlatformFolder) or UsedMegafolders.UnsafeContains(RelativeNameFolder) then
        begin
          Logger.Info('Synchronizing ' + RelativePlatformFolder + '...');
          Cleanup(PlatformFolder);
        end;
      end;
    end;
  end;
end;

class function TMegafolderManager.GetFolderName(const Root: string;
  const IDEName: TIDEName; const Platform: TPlatform; const BuildConfig: TBuildConfig): string;
begin
  Result := TPath.Combine(Root, DelphiSuffixes[IDEName], PlatformId[Platform], BuildConfigs[BuildConfig]);
end;

class procedure TMegafolderManager.RemoveAll(const Folder: string);
begin
  DeleteFolderMovingToLocked(Config.Folders.LockedFilesFolder, Folder, true);
end;

class procedure TMegafolderManager.RemoveUnused(const BaseFolder: string;
  const Megafolders: TMegafolderList);
begin
  var UsedMegafolders := TUsedMegafolders.Create;
  try
    for var Megafolder in Megafolders do
    begin
      if not Megafolder.IsNone then UsedMegafolders.Add(Megafolder.Folder);
    end;
    if not TDirectory.Exists(BaseFolder) then exit;
    var Folders := TDirectory.GetDirectories(BaseFolder, '*', TSearchOption.soTopDirectoryOnly);
    for var Folder in Folders do
    begin
      if UsedMegafolders.UnsafeContains(TPath.GetFileName(Folder)) then continue;
      DeleteFolderMovingToLocked(Config.Folders.LockedFilesFolder, Folder, true);
    end;

  finally
    UsedMegafolders.Free;
  end;
end;

class procedure TMegafolderManager.UpdateFile(const Source, Target: string; var Count: integer);
begin
  //Those are symlinks, so if they exist they will be updated when you recompile. No need to recreate them (which would be slower).
  if FileExists(Target, false) then exit;
  DoSymLink(Source, Target, false);
  Inc(Count);
end;

class procedure TMegafolderManager.UpdateFolder(const Source, Target: string; out Count: integer);
begin
  var LocalCount := 0;
  ScanFiles(Source, nil, nil, nil, nil,
    procedure(FileName, RelPath: string)
    begin
      UpdateFile(FileName, TPath.Combine(Target, TPath.GetFileName(FileName)), LocalCount);
    end, false, false);
    Count := LocalCount;
end;

class procedure TMegafolderManager.Load(const Megafolders: TMegafolderList;
  const ConfigFolder: string);
begin
  var ConfigFileName := MetaMegafolderFileName(ConfigFolder);
  if not TFile.Exists(ConfigFileName) then exit;

  var Reader := TStreamReader.Create(ConfigFileName, TEncoding.UTF8);
  try
    while not Reader.EndOfStream do
    begin
      var Line := Reader.ReadLine;
      var Index := Line.IndexOf(':');
      if Index < 1 then raise Exception.Create('Error reading metafolder file. Try removing the megafolder cache.');

      Megafolders.Add(TMegafolder.Create(Line.Substring(0, Index).Trim, Line.Substring(Index + 1).Trim));
    end;
  finally
    Reader.Free;
  end;
end;

class function TMegafolderManager.MetaMegafolderFileName(
  const Folder: string): string;
begin
  Result := TPath.Combine(Folder, 'megafolders.txt');
end;

class procedure TMegafolderManager.Save(const Megafolders: TMegafolderList;
  const ConfigFolder: string);
begin
  var MetaFile := MetaMegafolderFileName(ConfigFolder);
  if Megafolders = nil then
  begin
    if not TDirectory.Exists(ConfigFolder) then exit;
    DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, MetaFile);
  end;

  TDirectory_CreateDirectory(ConfigFolder);
  var Writer := TStreamWriter.Create(MetaFile, false, TEncoding.UTF8);
  try
    for var Megafolder in Megafolders do
    begin
      Writer.WriteLine(Megafolder.Folder + ':' + Megafolder.Mask);
    end;
  finally
    Writer.Free;
  end;
end;


end.

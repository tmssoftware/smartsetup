unit UProjectInstaller;
{$i ../../tmssetup.inc}

interface

uses
{$IFDEF MSWINDOWS}
  WinApi.Windows, Deget.Registry, Deget.IDEUtils,
{$ENDIF}
  UCoreTypes, Deget.CoreTypes, UInstaller, UConfigDefinition, UProjectBuildInfo, UFullBuildInfo,
  UPersistence, UOSShortcuts, UUninstallInfo, UFileHasher, SyncObjs,
  UProjectDefinition, Generics.Collections, Generics.Defaults, UIDEBuildInfo;

type
  TProjectInstaller = class
  private
    procedure DoRegisterFullProject(const BuildInfo: TProjectBuildInfo;
      const UninstallInfo: IUninstallInfo);
    procedure DoUnRegisterFullProject(const UninstallInfo: IUninstallInfo);
    procedure RegisterWebCore(const WebCorePath: string; const BuildInfo: TProjectBuildInfo; const UninstallInfo: IUninstallInfo);
    procedure UnRegisterWebCore(const ProjectId: string; const UninstallInfo: IUninstallInfo);
    function GetLinkedFile(const BuildInfo: TProjectBuildInfo; const FileToLink,
      LinkToFolder: string): string;
    procedure AddRegistryEntries(
      const BuildInfo: TProjectBuildInfo;
      const UninstallInfo: IUninstallInfo);
    procedure AddRegistryUninstall(const EntryKeyName, EntryValueName: string;
      const UninstallInfo: IUninstallInfo);
    function ReplaceRegistryVariables(const BuildInfo: TProjectBuildInfo; const s: string): string;
    procedure RemoveRegistryEntries(const ProjectId: string;
      const UninstallInfo: IUninstallInfo);
    function GetCredentials(const ServerName: string; const RegCode: boolean): string;
    function ReplaceRegistryKeys(const KeyName: string; const IDEBuildInfos: TObjectList<TIDEBuildInfo>): TArray<string>;
  var
    Persist: TPersistence;
    Config: TConfigDefinition;
  public
    constructor Create(const aConfig: TConfigDefinition);
    destructor Destroy; override;
    procedure RegisterFullProject(const BuildInfo: TProjectBuildInfo);
    procedure UnRegisterFullProject(const DryRun: boolean; const ProjectId: string);

    procedure UnRegisterAtIDELevel(const DryRun: boolean; const Installer: TInstaller; const ProjectId: string; const IDEName: TIDEName);
    procedure RegisterAtIDELevel(const Installer: TInstaller; const BuildInfo: TFullBuildInfo);

    procedure UnRegisterAtPlatformLevel(const DryRun: boolean; const Installer: TInstaller; const ProjectId: string; const IDEName: TIDEName; const Platform: TPlatform);
    procedure RegisterAtPlatformLevel(const Installer: TInstaller; const BuildInfo: TFullBuildInfo);

    procedure CleanAllBuildTemporaryFilesInPlat(const DryRun: boolean; const Installer: TInstaller; const FileHasher: TFileHasher; const ProjectId: string; const IDEName: TIDEName; const Platform: TPlatform);
    procedure CleanAllBuildTemporaryFiles(const DryRun: boolean; const Installer: TInstaller; const FileHasher: TFileHasher; const ProjectId: string; const IDEName: TIDEName; const Platform: TPlatform; const Package: string);
    procedure Build(const Installer: TInstaller; const BuildInfo: TFullBuildInfo);

    procedure CreateTempProjects(const Installer: TInstaller; const BuildInfo: TFullBuildInfo);
    procedure MoveDataFromTempProjects(const Installer: TInstaller; const BuildInfo: TFullBuildInfo);
    procedure RemoveTempProjects(const Installer: TInstaller; const BuildInfo: TFullBuildInfo);

    function GetAllProjects: TArray<IUninstallInfo>;
    function GetAllIDEs(const ProjectId: string): TArray<IUninstallInfo>;
    function GetAllPlatforms(const ProjectId: string; const IDE: TIDEName): TArray<IUninstallInfo>;

    function HasIDEUninstallInfo(const ProjectId: string; const IDEName: TIDEName): Boolean;
    function HasPlatformUninstallInfo(const ProjectId: string; const IDEName: TIDEName; const Plat: TPlatform): Boolean;
  end;

implementation

uses
  IOUtils, UFileSystemPersistence, SysUtils, ULogger, UMultiLogger, JSON,
  UProjectInstallerConstants, UTmsBuildSystemUtils, UOSFileLinks, Util.Replacer,
  UCredentials, UConfigFolders, Commands.GlobalConfig, Commands.Logging, Deget.DelphiInfo, Deget.IDEInfo;

type
  TWebCoreInfo = class
  public const
    RegistryKey = 'Software\tmssoftware\TMS WEB Core';
    LibraryPath = 'LibraryPaths';
    UninstallWebCorePath = 'WebCorePath';
  end;

  TRegistryInfo = class
  public const
    KeysToDelete = 'RegistryKeysToDelete';
    ValuesToDelete = 'RegistryValuesToDelete';
    KeyName = 'KeyName';
    ValueName = 'ValueName';
  end;


constructor TProjectInstaller.Create(const aConfig: TConfigDefinition);
begin
  Config := aConfig;
  Persist := TFileSystemPersistence.Create(Config.Folders.UninstallFolder, InstallerConstants.UninstallExtension);
end;

destructor TProjectInstaller.Destroy;
begin
   Persist.Free;
  inherited;
end;

function MatchesOS(const plats: TOperatingSystemSet): boolean;
begin
  if plats = [] then exit(True);

{$IFDEF MSWINDOWS}
  exit (TOperatingSystem.windows in plats);
{$ENDIF}
{$IFDEF LINUX}
  exit (TOperatingSystem.linux in plats);
{$ENDIF}
{$IFDEF MACOS}
  exit (TOperatingSystem.mac in plats);
{$ENDIF}
end;

function  TProjectInstaller.GetLinkedFile(const BuildInfo: TProjectBuildInfo; const FileToLink, LinkToFolder: string):string;
begin
  var ActualLinkToFolder := LinkToFolder;
  if Trim(LinkToFolder) = '' then ActualLinkToFolder := TPath.Combine(Config.Folders.BplFolder, 'Win32');

  Result := TPath.Combine(ActualLinkToFolder, TPath.GetFileName(FileToLink));
end;

procedure TProjectInstaller.DoRegisterFullProject(const BuildInfo: TProjectBuildInfo; const UninstallInfo: IUninstallInfo);
begin
  UninstallInfo.Value.WriteStr(InstallerConstants.ProjectNameJsonId, BuildInfo.Project.Application.NameAndVersion);
  if not BuildInfo.SkipRegistering.StartMenu then
  begin
    var StartMenu := GetStartMenuPath;
    if StartMenu <> '' then
    begin
      var JSONArray := TJSONArray.Create;
      UninstallInfo.Value.AddPair(InstallerConstants.ProjectLinksJsonId, JSONArray);
      for var shortcut in BuildInfo.Shortcuts do
      begin
        var FileName := TPath.Combine(StartMenu, shortcut.Name) + '.lnk';
        if not UninstallInfo.DryRun then CreateShortcut(FileName, shortcut.Target, shortcut.Description, shortcut.WorkingFolder);
        Logger.Info('Added shortcut to start menu: ' + shortcut.Target + ' -> ' + FileName);

        JSONArray.Add(FileName);
      end;
    end
    else
    begin
    {$IFDEF MSWINDOWS}
      Logger.Info('Can''t find the start menu folder');
    {$ENDIF}
    end;
  end;

  if not BuildInfo.SkipRegistering.Packages then
  begin
    var JSONArray := TJSONArray.Create;
    UninstallInfo.Value.AddPair(InstallerConstants.ProjectFileLinksJsonId, JSONArray);
    for var filelink in BuildInfo.FileLinks do
    begin
      if not MatchesOS(filelink.OS) then continue;

      var LinkToFile := GetLinkedFile(BuildInfo, filelink.FileToLink, filelink.LinkToFolder);
      if not UninstallInfo.DryRun then CreateFileLink(Config.Folders.LockedFilesFolder, filelink.FileToLink, LinkToFile, Config.FileLinkType(BuildInfo.ProjectId));
      Logger.Trace('Created file link: ' + filelink.FileToLink + ' -> ' + LinkToFile);

      JSONArray.Add(LinkToFile);
    end;
  end;
end;

procedure TProjectInstaller.DoUnRegisterFullProject(const UninstallInfo: IUninstallInfo);
begin
  var LinksPair := UninstallInfo.Value.Get(InstallerConstants.ProjectLinksJsonId);
  if (LinksPair <> nil) and (LinksPair.JsonValue is TJSONArray) then  //it can be nil at least in linux.
  begin
    for var Key in TJSONArray(LinksPair.JsonValue) do
    begin
      try
        var Link := Key.GetValue<string>;
        if not UninstallInfo.DryRun then
        begin
          DeleteShortcut(Config.Folders.LockedFilesFolder, GetStartMenuPath, Link);
        end;
        Logger.Info(Format('Removed link "%s" from start menu.', [Link]));
      except on ex: Exception do
        Logger.Error('Error removing shortcut :' + ex.Message);
      end;

    end;
  end;

  var FileLinksPair := UninstallInfo.Value.Get(InstallerConstants.ProjectFileLinksJsonId);
  if (FileLinksPair <> nil) and (FileLinksPair.JsonValue is TJSONArray) then  //it can be nil at least in linux.
  begin
    for var Key in TJSONArray(FileLinksPair.JsonValue) do
    begin
      try
        var FileLink := Key.GetValue<string>;
        if not UninstallInfo.DryRun then
        begin
          DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, FileLink);
        end;
        Logger.Info(Format('Removed linked file "%s".', [FileLink]));
      except on ex: Exception do
        Logger.Error('Error removing linked file :' + ex.Message);
      end;

    end;
  end;

end;

procedure TProjectInstaller.RegisterFullProject(const BuildInfo: TProjectBuildInfo);
begin
  var UninstallInfo: IUninstallInfo := TUninstallInfo.Create('', BuildInfo.DryRun,
    TEngineLevel.Project, BuildInfo.ProjectId, TIDEName(-1), TPlatform(-1), '', '', Config.Folders.LockedFilesFolder);
  try
    DoRegisterFullProject(BuildInfo, UninstallInfo);
    if not BuildInfo.SkipRegistering.WebCore then RegisterWebCore(BuildInfo.ExtraPaths.GetWebCorePaths(TPlatform.win32intel), BuildInfo, UninstallInfo);
    if not BuildInfo.SkipRegistering.Packages then AddRegistryEntries(BuildInfo, UninstallInfo);
  except
    try
      DoUnRegisterFullProject(UninstallInfo);
    except
      //lost hope.
    end;
    raise;
  end;
  if not BuildInfo.DryRun then Persist.Store(UninstallInfo.Value.ToString, BuildInfo.ProjectId);
end;

procedure TProjectInstaller.RegisterWebCore(const WebCorePath: string; const BuildInfo: TProjectBuildInfo;
  const UninstallInfo: IUninstallInfo);
begin
{$IFDEF MSWINDOWS}
  if WebCorePath = '' then Exit;

  Logger.Info(Format('Registering %s to TMS Web Core', [BuildInfo.ProjectId]));

  var Lock := TMutex.Create(nil, false, Mutex_WebCore);
  try
    Lock.Acquire;
    try
      if not UninstallInfo.DryRun then
      begin
        if RegKeyExists(HKEY_CURRENT_USER, TWebCoreInfo.RegistryKey) then
        begin
          UninstallInfo.Value.WriteStr(TWebCoreInfo.UninstallWebCorePath, WebCorePath);
          var WebCoreLibraryPath: string;
          RegReadString(HKEY_CURRENT_USER, TWebCoreInfo.RegistryKey, TWebCoreInfo.LibraryPath, WebCoreLibraryPath);
          WebCoreLibraryPath := AddPaths(WebCoreLibraryPath, WebCorePath);
          RegWriteString(HKEY_CURRENT_USER, TWebCoreInfo.RegistryKey, TWebCoreInfo.LibraryPath, WebCoreLibraryPath);
        end;
      end;
    finally
      Lock.Release;
    end;
  finally
    Lock.Free;
  end;
  Logger.Trace(Format('%s added to "%s": %s', [
    TWebCoreInfo.UninstallWebCorePath,
    BuildInfo.ProjectId,
    WebCorePath
  ]));
{$ENDIF}
end;

procedure TProjectInstaller.AddRegistryUninstall(const EntryKeyName, EntryValueName: string; const UninstallInfo: IUninstallInfo);
begin
{$IFDEF MSWINDOWS}
  //We want to delete all registry keys we added. Say we add an entry:
  //  Software\key1\key2\key3
  //  If key1 and key2 didn't exist, they will be automatically created. But we need to remove them when we uninstall.

  var JKeysToDelete := UninstallInfo.Value.GetArray(TRegistryInfo.KeysToDelete);
  var JValuesToDelete := UninstallInfo.Value.GetArray(TRegistryInfo.ValuesToDelete);

  var KeyName := EntryKeyName;
  while not RegKeyExists(HKEY_CURRENT_USER, KeyName) do
  begin
    JKeysToDelete.Add(KeyName);
    KeyName := TPath.GetDirectoryName(KeyName);
  end;

  var v := TJSONObject.Create;
  v.AddPair(TRegistryInfo.KeyName, EntryKeyName);
  v.AddPair(TRegistryInfo.ValueName, EntryValueName);
  JValuesToDelete.Add(v);
{$ENDIF}
end;

function TProjectInstaller.GetCredentials(const ServerName: string; const RegCode: boolean): string;
begin
  var Folders: IBuildFolders := TBuildFolders.Create(TPath.GetDirectoryName(ConfigFileName));

  var Manager := CreateCredentialsManager(Folders.CredentialsFile(ServerName), FetchOptions);
  try
    var Credentials := Manager.ReadCredentials;
    try
      if RegCode then exit(Credentials.Code);
      exit(Credentials.Email);
    finally
      Credentials.Free;
    end;
  finally
    Manager.Free;
  end;

end;

function TProjectInstaller.ReplaceRegistryVariables(const BuildInfo: TProjectBuildInfo; const s: string): string;
begin
  Result := ParseString(s, function(varName: string): string
    begin
      if varName = 'product-id' then exit(BuildInfo.ProjectId);
      if varName = 'install-path' then exit(TPath.GetDirectoryName(BuildInfo.Project.FullPath));
      if varName = 'install-date' then exit(FormatDateTime('yyyy-mm-dd', Now, TFormatSettings.Invariant)); //localize as yyyy-mm-dd?
      if varName = 'install-time' then exit(FormatDateTime('hh:nn:ss', Now, TFormatSettings.Invariant));
      if varName = 'version' then exit(BuildInfo.Project.Application.Version);
      if varName = 'alternate-registry-key' then exit(Config.AlternateRegistryKey);
      if varName = 'reg-code' then exit(GetCredentials('tms', true)); //This is tms produts like webcore  which write the credentials in the registry. It is always the credential from tms source.
      if varName = 'reg-email' then exit(GetCredentials('tms', false));


      raise Exception.Create('Unknown variable: ' + varName);
    end);
end;

function GetRegistryBranch(const IDEName: TIDEName): string;
begin
{$IFDEF MSWINDOWS}
  var IDEInfo: IDelphiIDEInfo := TDelphiIDEInfo.Create(IDEName, Config.AlternateRegistryKey);
  Result := IDEInfo.BaseKey();
{$ELSE}
  Result := '';
{$ENDIF}
end;

function TProjectInstaller.ReplaceRegistryKeys(const KeyName: string; const IDEBuildInfos: TObjectList<TIDEBuildInfo>): TArray<string>;
begin
  var HasBDS := false;
  var Replaced := ParseString(KeyName, function(varName: string): string
    begin
      if varName = 'bds' then begin; HasBDS := true; exit('%bds%'); end;
      raise Exception.Create('Unknown variable: ' + varName);
    end);

  if not HasBDS then exit(TArray<string>.Create(Replaced));

  Result := nil;
  SetLength(Result, IDEBuildInfos.Count);
  var i := 0;
  for var IDEBuildInfo in IDEBuildInfos do
  begin
    Result[i] := ParseString(Replaced, function(varName: string): string
    begin
      if varName = 'bds' then exit(GetRegistryBranch(IDEBuildInfo.Name));
      raise Exception.Create('Unknown variable: ' + varName);
    end);
    inc(i);
  end;
end;

procedure TProjectInstaller.AddRegistryEntries(const BuildInfo: TProjectBuildInfo;
  const UninstallInfo: IUninstallInfo);
begin
{$IFDEF MSWINDOWS}
  var RegistryEntries := BuildInfo.Project.RegistryEntries;
  if (RegistryEntries = nil) or (RegistryEntries.Count = 0) then Exit;

  Logger.Info(Format('Adding registry entries for %s', [BuildInfo.ProjectId]));

  var Lock := TMutex.Create(nil, false, Mutex_WebCore);
  try
    Lock.Acquire;
    try
      if not UninstallInfo.DryRun then
      begin
        for var Entry in RegistryEntries do
        begin
          var EntryKeyNames := ReplaceRegistryKeys(Entry.KeyName, BuildInfo.IDEsBuildInfo);
          for var EntryKeyName in EntryKeyNames do
          begin
            AddRegistryUninstall(EntryKeyName, Entry.ValueName, UninstallInfo);

            var ValueDataExpanded := ReplaceRegistryVariables(BuildInfo, Entry.ValueData);
            case Entry.ValueType of
              TRegistryEntryType.String: RegWriteString(HKEY_CURRENT_USER, EntryKeyName, Entry.ValueName, ValueDataExpanded);
              TRegistryEntryType.DWord: RegWriteInteger(HKEY_CURRENT_USER, EntryKeyName, Entry.ValueName, StrToInt(ValueDataExpanded));
            end;
            Logger.Trace(Format('%s added to "%s": %s', [
              EntryKeyName + '|' + Entry.ValueName,
              BuildInfo.ProjectId,
              ValueDataExpanded
            ]));
          end;
        end;
      end;
    finally
      Lock.Release;
    end;
  finally
    Lock.Free;
  end;
{$ENDIF}
end;


procedure TProjectInstaller.UnRegisterFullProject(const DryRun: boolean; const ProjectId: string);
begin
  try
    var data := Persist.Retrieve(ProjectId);
    if data <> '' then
    begin
      var UninstallInfo: IUninstallInfo := TUninstallInfo.Create(data, DryRun, TEngineLevel.Project,
      ProjectId, TIDEName(-1), TPlatform(-1), '', '', Config.Folders.LockedFilesFolder);
      DoUnRegisterFullProject(UninstallInfo);
      UnregisterWebCore(ProjectId, UninstallInfo);
      RemoveRegistryEntries(ProjectId, UninstallInfo);
    end;
  except on ex: Exception do
    begin
      // if something went wrong in uninstall there is not much we can do.
      // the most likely case is that we will keep forever trying to uninstall and failing.
      // so we will ignore it and assume it was uninstalled as well as it was possible.
      Logger.Error('Error uninstalling project "' + ProjectId
                  + '": ' + ex.Message);
    end;
  end;
  if not DryRun then Persist.Remove(ProjectId);
end;

procedure TProjectInstaller.UnRegisterWebCore(const ProjectId: string; const UninstallInfo: IUninstallInfo);
begin
{$IFDEF MSWINDOWS}
  var PathsToRemove := UninstallInfo.Value.ReadStr(TWebCoreInfo.UninstallWebCorePath, '');
  if PathsToRemove = '' then Exit;

  var Lock := TMutex.Create(nil, false, Mutex_WebCore);
  try
    Lock.Acquire;
    try
      if not UninstallInfo.DryRun then
      begin
        var WebCoreLibraryPath: string;
        RegReadString(HKEY_CURRENT_USER, TWebCoreInfo.RegistryKey, TWebCoreInfo.LibraryPath, WebCoreLibraryPath);
        WebCoreLibraryPath := RemovePaths(WebCoreLibraryPath, PathsToRemove);
        RegWriteString(HKEY_CURRENT_USER, TWebCoreInfo.RegistryKey, TWebCoreInfo.LibraryPath, WebCoreLibraryPath);
      end;
    finally
      Lock.Release;
    end;
  finally
    Lock.Free;
  end;

  Logger.Trace(Format('%s removed from %s: %s', [
    TWebCoreInfo.UninstallWebCorePath,
    Projectid,
    PathsToRemove
  ]));
{$ENDIF}
end;

//Sort the keys to delete by decending length, so larger keys are deleted first.
//if we have to delete: a/b/c and a/b, and we start by deleting a/b, then it won't be
//deleted because it has a child a/b/c, then we delete a/b/c and a/b is not deleted anymore.
//we could try some iterative approach here until everything is deleted, but sorting by length should work.
//if a key is a child of other key, it should be longer. (a/../b kind of keys are not supported).
function GetSortedKeyArray(const Keys: TJSONArray): TArray<string>;
begin
  Result := nil;
  SetLength(Result, Keys.Count);
  for var i := 0 to High(Result) do
  begin
    Result[i] := Keys[i].Value;
  end;

  var KeyComparer: IComparer<string> := TDelegatedComparer<string>.Create(
  function(const Left, Right: string): integer
  begin
    if Left.Length < Right.Length then
      Result := 1
    else if Left.Length > Right.Length then
      Result := -1
    else
      Result := 0;
  end);

  TArray.Sort<string>(Result, KeyComparer);
end;

procedure TProjectInstaller.RemoveRegistryEntries(const ProjectId: string; const UninstallInfo: IUninstallInfo);
begin
{$IFDEF MSWINDOWS}
  var ValuesToRemove := UninstallInfo.Value.GetArray(TRegistryInfo.ValuesToDelete);
  var KeysToRemove := UninstallInfo.Value.GetArray(TRegistryInfo.KeysToDelete);
  if (ValuesToRemove.Count = 0) and(KeysToRemove.Count = 0) then exit;

  Logger.StartSection(TMessageType.Unregister, 'Removing registry entries');
  try
    var Lock := TMutex.Create(nil, false, Mutex_WebCore);
    try
      Lock.Acquire;
      try
        if not UninstallInfo.DryRun then
        begin
          for var ValueToRemove in ValuesToRemove do
          begin
            var KeyName := (ValueToRemove as TJSONObject).GetValue(TRegistryInfo.KeyName).Value;
            var ValueName := (ValueToRemove as TJSONObject).GetValue(TRegistryInfo.ValueName).Value;
            var Message := 'Registry value %s not removed from %s: %s';
            if RegDeleteValue(HKEY_CURRENT_USER, KeyName, ValueName) then Message := 'Registry value %s removed from %s: %s';
            Logger.Trace(Format(Message, [
              KeyName,
              Projectid,
              ValueName
            ]));
          end;

          var KeysToRemoveArray := GetSortedKeyArray(KeysToRemove);
          for var KeyToRemove in KeysToRemoveArray do
          begin
            var Message := 'Registry key %s not removed from %s';
            if RegKeyHasValuesOrSubkeys(HKEY_CURRENT_USER, KeyToRemove) then  Message := 'Registry key %s not removed from %s because it has values or subkeys'
            else if RegDeleteKey(HKEY_CURRENT_USER, KeyToRemove)  then Message := 'Registry key %s removed from %s';
            Logger.Trace(Format(Message, [
              KeyToRemove,
              Projectid
            ]));
          end;

        end;
      finally
        Lock.Release;
      end;
    finally
      Lock.Free;
    end;
  finally
    Logger.FinishSection(TMessageType.Unregister);
  end;

{$ENDIF}
end;

procedure TProjectInstaller.RegisterAtIDELevel(const Installer: TInstaller; const BuildInfo: TFullBuildInfo);
begin
  var UninstallInfo: IUninstallInfo := TUninstallInfo.Create('', BuildInfo.Project.DryRun,
    TEngineLevel.Ide, BuildInfo.Project.ProjectId, BuildInfo.IDE.Name, TPlatform(-1), '', BuildInfo.IDE.PathToCompiler, Config.Folders.LockedFilesFolder);
  try
    Installer.RegisterAtIDELevel(BuildInfo, UninstallInfo);
  except
    try
      Installer.UnRegisterAtIDELevel(UninstallInfo);
    except
      //lost hope.
    end;
    raise;
  end;
  if not BuildInfo.Project.DryRun then Persist.Store(UninstallInfo.Value.ToString, BuildInfo.Project.ProjectId, IDEId[BuildInfo.IDE.Name]);
end;

procedure TProjectInstaller.UnRegisterAtIDELevel(const DryRun: boolean; const Installer: TInstaller; const ProjectId: string;
  const IDEName: TIDEName);
begin
  try
    var data := Persist.Retrieve(ProjectId, IDEId[IDEName]);
    if data <> '' then
    begin
      var UninstallInfo: IUninstallInfo := TUninstallInfo.Create(data, DryRun, TEngineLevel.Ide,
      ProjectId, IDEName, TPlatform(-1), '', Config.CompilerPath(ProjectId, IDEName), Config.Folders.LockedFilesFolder); //platform is invalid here.
      Installer.UnRegisterAtIDELevel(UninstallInfo);
    end;
  except on ex: Exception do
    begin
      // if something went wrong in uninstall there is not much we can do.
      // the most likely case is that we will keep forever trying to uninstall and failing.
      // so we will ignore it and assume it was uninstalled as well as it was possible.
      Logger.Error('Error uninstalling project "' + ProjectId
                  + '" in IDE ' + IDEId[IDEName] + ': ' + ex.Message);
    end;
  end;
  if not DryRun then Persist.Remove(ProjectId, IDEId[IDEName]);

end;

procedure TProjectInstaller.RegisterAtPlatformLevel(const Installer: TInstaller;
  const BuildInfo: TFullBuildInfo);
begin
  var UninstallInfo: IUninstallInfo := TUninstallInfo.Create('', BuildInfo.Project.DryRun,
    TEngineLevel.Platform, BuildInfo.Project.ProjectId, BuildInfo.IDE.Name, BuildInfo.Platform.Name, '', BuildInfo.IDE.PathToCompiler, Config.Folders.LockedFilesFolder);

  try
    Installer.RegisterAtPlatformLevel(BuildInfo, UninstallInfo);
  except
    try
      Installer.UnRegisterAtPlatformLevel(UninstallInfo);
    except
      // lost hope
    end;
    raise;
  end;
  if not BuildInfo.Project.DryRun then Persist.Store(UninstallInfo.Value.ToString, BuildInfo.Project.ProjectId, IDEId[BuildInfo.IDE.Name], PlatformID[BuildInfo.Platform.Name]);
end;

procedure TProjectInstaller.UnRegisterAtPlatformLevel(const DryRun: boolean; const Installer: TInstaller; const ProjectId: string;
  const IDEName: TIDEName; const Platform: TPlatform);
begin
  try
    var data := Persist.Retrieve(ProjectId, IDEId[IDEName], PlatformId[Platform]);
    if data <> '' then
    begin
      var UninstallInfo: IUninstallInfo := TUninstallInfo.Create(data, DryRun, TEngineLevel.Platform,
        ProjectId, IDEName, Platform, '', Config.CompilerPath(ProjectId, IDEName), Config.Folders.LockedFilesFolder);
        Installer.UnRegisterAtPlatformLevel(UninstallInfo);
    end;
  except
   on ex: Exception do
     begin
        // if something went wrong in uninstall there is not much we can do.
        // the most likely case is that we will keep forever trying to uninstall and failing.
        // so we will ignore it and assume it was uninstalled as well as it was possible.
      Logger.Error('Error uninstalling project "' + ProjectId
                  + '" in IDE ' + IDEId[IDEName]
                  + ' in Platform ' + PlatformId[Platform]
                  + ': ' + ex.Message);
     end;
  end;

  if not DryRun then Persist.Remove(ProjectId, IDEId[IDEName], PlatformId[Platform]);

end;

procedure TProjectInstaller.Build(const Installer: TInstaller;
  const BuildInfo: TFullBuildInfo);
begin
  var UninstallInfo: IUninstallInfo := TUninstallInfo.Create('', BuildInfo.Project.DryRun,
    TEngineLevel.Platform, BuildInfo.Project.ProjectId, BuildInfo.IDE.Name, BuildInfo.Platform.Name, BuildInfo.Package.Package.Name, BuildInfo.IDE.PathToCompiler, Config.Folders.LockedFilesFolder);

  try
    Installer.Build(BuildInfo, UninstallInfo);
  finally
    // we can't rollback in this case, because Installer.CleanAllBuildTemporaryFiles will remove also the
    // files from other packages (it will remove the output folder). This will mess with previous packages that
    // were installed right.
    if not BuildInfo.Project.DryRun then Persist.Store(UninstallInfo.Value.ToString, BuildInfo.Project.ProjectId, IDEId[BuildInfo.IDE.Name], PlatformID[BuildInfo.Platform.Name], BuildInfo.Package.Package.Name);
  end;
end;

procedure TProjectInstaller.CleanAllBuildTemporaryFiles(const DryRun: boolean;
  const Installer: TInstaller; const FileHasher: TFileHasher; const ProjectId: string; const IDEName: TIDEName;
  const Platform: TPlatform; const Package: string);
begin
  try
    var data := Persist.Retrieve(ProjectId, IDEId[IDEName], PlatformId[Platform], Package);
    if data <> '' then
    begin
      var UninstallInfo: IUninstallInfo := TUninstallInfo.Create(data, DryRun, TEngineLevel.Platform,
        ProjectId, IDEName, Platform, Package, Config.CompilerPath(ProjectId, IDEName), Config.Folders.LockedFilesFolder);

      Installer.CleanAllBuildTemporaryFiles(UninstallInfo);
    end;
  except
   on ex: Exception do
     begin
        // if something went wrong in uninstall there is not much we can do.
        // the most likely case is that we will keep forever trying to uninstall and failing.
        // so we will ignore it and assume it was uninstalled as well as it was possible.
      Logger.Error('Error removing temp files for project "' + ProjectId
                  + '" in IDE ' + IDEId[IDEName]
                  + ' in Platform ' + PlatformId[Platform]
                  + ' in Package ' + Package
                  + ': ' + ex.Message);
     end;
  end;

  if not DryRun then
  begin
    Persist.Remove(ProjectId, IDEId[IDEName], PlatformId[Platform], Package);
    FileHasher.RemoveHashesForPlatform(ProjectId, IDEId[IDEName], PlatformId[Platform]);
  end;
end;


procedure TProjectInstaller.CleanAllBuildTemporaryFilesInPlat(
  const DryRun: boolean; const Installer: TInstaller; const FileHasher: TFileHasher; const ProjectId: string;
  const IDEName: TIDEName; const Platform: TPlatform);
begin
  Logger.StartSection(TMessageType.Unregister, 'Clean Build Temp Files');
  try
    var Packages := Persist.List(ProjectId, IDEId[IDEName], PlatformId[Platform]);
    for var Package in Packages do
    begin
      CleanAllBuildTemporaryFiles(DryRun, Installer, FileHasher, ProjectId, IDEName, Platform, Package.Id);
    end
  finally
    Logger.FinishSection(TMessageType.Unregister, false);
  end;
end;

function TProjectInstaller.GetAllProjects: TArray<IUninstallInfo>;
begin
  var AllProjs := Persist.List('');
  SetLength(Result, Length(AllProjs));
  for var i := Low(Result) to High(Result) do
  begin
    Result[i] := TUninstallInfo.Create(AllProjs[i].Data, Config.DryRun(AllProjs[i].Id), TEngineLevel.Project, AllProjs[i].Id,
               TIDEName(-1), TPlatform(-1), '', '', Config.Folders.LockedFilesFolder);
  end;
end;


function TProjectInstaller.HasIDEUninstallInfo(const ProjectId: string; const IDEName: TIDEName): Boolean;
begin
  Result := Persist.Retrieve(ProjectId, IDEId[IDEName]) <> '';
end;

function TProjectInstaller.HasPlatformUninstallInfo(const ProjectId: string; const IDEName: TIDEName; const Plat: TPlatform): Boolean;
begin
  Result := Persist.Retrieve(ProjectId, IDEId[IDEName], PlatformId[Plat]) <> '';
end;

function TProjectInstaller.GetAllIDEs(const ProjectId: string): TArray<IUninstallInfo>;
begin
  var AllIDEs := Persist.List(ProjectId);
  SetLength(Result, Length(AllIDEs));
  var k := 0;
  for var i := Low(Result) to High(Result) do
  begin
    var ide := GetIDEName(AllIDEs[i].Id);
    if ide in ValidIDEs then
    begin
      Result[k] := TUninstallInfo.Create(AllIDEs[i].Data, false, TEngineLevel.IDE, ProjectId,
                 ide, TPlatform(-1), '', Config.CompilerPath(ProjectId, ide), Config.Folders.LockedFilesFolder);
      inc(k);
    end;
  end;
  SetLength(Result, k);
end;

function TProjectInstaller.GetAllPlatforms(const ProjectId: string; const IDE: TIDEName): TArray<IUninstallInfo>;
begin
  var AllPlatforms := Persist.List(ProjectId, IDEId[IDE]);
  SetLength(Result, Length(AllPlatforms));
  var k := 0;
  for var i := Low(Result) to High(Result) do
  begin
    var plat := GetPlatformName(AllPlatforms[i].Id);
    if plat in ValidPlatforms then
    begin
      Result[k] := TUninstallInfo.Create(AllPlatforms[i].Data, false, TEngineLevel.Platform, ProjectId,
                     IDE, plat, '', Config.CompilerPath(ProjectId, IDE), Config.Folders.LockedFilesFolder);
      inc(k);
    end;
  end;
  SetLength(Result, k);
end;


procedure TProjectInstaller.CreateTempProjects(const Installer: TInstaller;
  const BuildInfo: TFullBuildInfo);
begin
  try
    Installer.CreateTempProjects(BuildInfo);
  except on ex: Exception do
    begin
      Logger.Info('Error creating temp projects for project ' + BuildInfo.Project.Project.Application.Name
            + ', ' + IDEId[BuildInfo.IDE.Name] + ', ' + PlatformId[BuildInfo.Platform.Name]
            + ': ' + ex.Message);
      raise;
    end;
  end;
end;

procedure TProjectInstaller.MoveDataFromTempProjects(const Installer: TInstaller;
  const BuildInfo: TFullBuildInfo);
begin
  try
    Installer.MoveDataFromTempProjects(BuildInfo);
  except on ex: Exception do
    begin
      Logger.Info('Error moving data from temp projects for project ' + BuildInfo.Project.Project.Application.Name
            + ', ' + IDEId[BuildInfo.IDE.Name] + ', ' + PlatformId[BuildInfo.Platform.Name]
            + ': ' + ex.Message);
      raise

    end;
  end;
end;

procedure TProjectInstaller.RemoveTempProjects(const Installer: TInstaller;
  const BuildInfo: TFullBuildInfo);
begin
  try
    Installer.RemoveTempProjects(BuildInfo);
  except on ex: Exception do
    begin
      Logger.Info('Error removing temp projects for project ' + BuildInfo.Project.Project.Application.Name
            + ', ' + IDEId[BuildInfo.IDE.Name] + ', ' + PlatformId[BuildInfo.Platform.Name]
            + ': ' + ex.Message);

    end;
  end;
end;


end.

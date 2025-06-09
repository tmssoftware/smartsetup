unit UEnvironmentPath;

interface
{$IFDEF MSWINDOWS}
uses SysUtils, Deget.CoreTypes, SyncObjs;

procedure StoreDelphiEnviromnentOverrides(const IDEName: TIDEName; const RegistryKey: string; const LinkedFolder: string);
procedure RemoveFromEnviromnentOverrides;
function GetEnvironmentOptionsLock: TMutex;

implementation
uses Deget.IDEInfo, Deget.DelphiInfo, UFileSystemPersistence, Commands.GlobalConfig, JSON, UMultiLogger;

const
  EnvMutex = 'Global\tms-smart-setup-32E67A5E-3220-4009-A921-820F3F017FE1';
  EnvProjectName = 'delphi_environment_overrides';
  SelfUninstallExtension = '.selfuninstall.json';

function GetEnvironmentOptionsLock: TMutex;
begin
  Result := TMutex.Create(nil, false, EnvMutex);
end;

procedure RemoveOldData(const Persist: TFileSystemPersistence; const IDEName: TIDEName);
begin
  try
    var OldData := Persist.Retrieve(EnvProjectName, IDEId[IDEName]);
    if OldData = '' then exit;
    Persist.Remove(EnvProjectName, IDEId[IDEName]);
    var Json := TJSONObject.ParseJSONValue(OldData, false, true);
    try
      var RegistryKey := Json.GetValue<string>('registry-key');
      var Path :=  Json.GetValue<string>('path');
      var IDEInfo: IDelphiIDEInfo := TDelphiIDEInfo.Create(IDEName, RegistryKey);
      IDEInfo.RemoveFolderFromPathOverride(Path);
    finally
      Json.Free;
    end;
  except on ex: Exception do
    begin
      //Not fatal, keep going.
      Logger.Info('Error removing Path Environment override from ' + IDEId[IDEName] + '. ' + ex.Message);
    end;
  end;
end;

procedure StoreDelphiEnviromnentOverrides(const IDEName: TIDEName; const RegistryKey: string; const LinkedFolder: string);
begin
  var Lock := TMutex.Create(nil, false, EnvMutex);
  try
    Lock.Acquire;
    try
      var UninstallInfo := TJSONObject.Create;
      try
        UninstallInfo.AddPair('registry-key', RegistryKey);
        UninstallInfo.AddPair('path', LinkedFolder);
        var Persist := TFileSystemPersistence.Create(Config.Folders.SelfUninstallFolder, SelfUninstallExtension);
        try
          RemoveOldData(Persist, IDEName);
          Persist.Store(UninstallInfo.ToString, EnvProjectName, IDEId[IDEName]);
        finally
          Persist.Free;
        end;
      finally
        UninstallInfo.Free;
      end;
    finally
      Lock.Release;
    end;
  finally
    Lock.Free;
  end;
end;

procedure RemoveFromEnviromnentOverrides;
begin
  var Lock := TMutex.Create(nil, false, EnvMutex);
  try
    Lock.Acquire;
    try
      var Persist := TFileSystemPersistence.Create(Config.Folders.SelfUninstallFolder, SelfUninstallExtension);
      try
        var PathsToRemove := Persist.List(EnvProjectName);
        for var PathToRemove in PathsToRemove do
        begin
          RemoveOldData(Persist, GetIDEName(PathToRemove.Id));
          Logger.Info('Removed path from ' + PathToRemove.Id + ' path override.');

        end;
      finally
        Persist.Free;
      end;
    finally
      Lock.Release;
    end;
  finally
    Lock.Free;
  end;
end;

{$ELSE}
implementation
{$ENDIF}

end.

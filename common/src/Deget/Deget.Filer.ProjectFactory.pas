unit Deget.Filer.ProjectFactory;
{$IFDEF MSWINDOWS}
interface
uses Classes, SysUtils, Generics.Defaults, Generics.Collections, Deget.Filer.DprojFile, Deget.Filer.CBprojFile, Deget.CoreTypes, SyncObjs;

type
TDelphiProjectFactory = record
public
  class var ProjectLock: TCriticalSection;
  class var Projects: TObjectDictionary<string, TBasePackageReadData>;
  class constructor Create;
  class destructor Destroy;
public
  class function GetPackageReadData(const ProjectName: string; const AIDEName: TIDEName): TBasePackageReadData; static;
end;

implementation
uses IOUtils;

{ TDelphiProjectFactory }

class constructor TDelphiProjectFactory.Create;
begin
  Projects := TObjectDictionary<string, TBasePackageReadData>.Create([doOwnsValues]);
  ProjectLock := TCriticalSection.Create;
end;

class destructor TDelphiProjectFactory.Destroy;
begin
  ProjectLock.Free;
  Projects.Free;
end;

class function TDelphiProjectFactory.GetPackageReadData(
  const ProjectName: string; const AIDEName: TIDEName): TBasePackageReadData;
begin
  ProjectLock.Enter;
  try
    if Projects.TryGetValue(ProjectName, Result) then
    begin
      exit;
    end;

    var ProjExt := TPath.GetExtension(ProjectName);
    if SameText(ProjExt, '.cbproj') then
    begin
      var Reader := TCBprojReader.Create(ProjectName, AIDEName);
      try
        var CBData := TCBProjPackageReadData.Create;
        Reader.ReadData(CBData);
        Projects.Add(ProjectName, CBData);
        exit(CBData);
      finally
        Reader.Free;
      end;
    end;

    if SameText(ProjExt, '.dproj') then
    begin
      var Reader := TDprojReader.Create(ProjectName, AIDEName);
      try
        var DData := TPackageReadData.Create;
        Reader.ReadData(DData);
        Projects.Add(ProjectName, DData);
        exit(DData);
      finally
        Reader.Free;
      end;
    end;
  finally
    ProjectLock.Leave;
  end;

  Result := nil;
end;
{$ELSE}
interface
implementation
{$ENDIF}
end.

unit Commands.VersionsRemote;

interface

uses
  System.Generics.Collections, System.SysUtils, System.DateUtils, UCommandLine;

procedure RegisterVersionsRemoteCommand;

implementation
uses
  Commands.CommonOptions, Commands.Logging, Commands.GlobalConfig, URepositoryManager, Deget.Version, System.JSON,
  UJsonPrinter, UConfigDefinition, VCS.Manager, VCS.Registry, VCS.CoreTypes, VCS.Engine.Factory,
  Fetching.ProductVersion, IOUtils, UTmsBuildSystemUtils;

var
  EnableLog: Boolean = False;
  UseJson: Boolean = False;
  ProductId: string;

type
  TOutputVersion = record
    Version: string;
    LicenseStatus: TLicenseStatus;
    ReleaseDate: TDateTime;
    FileHash: string;
    constructor Create(const aVersion: string;
        const aLicenseStatus: TLicenseStatus;
        const aReleaseDate: TDateTime;
        const aFileHash: string);
  end;

procedure OutputAsJson(Versions: TList<TOutputVersion>);
const
  LicenseStatusStr: array[TLicenseStatus] of string = ('none', 'licensed');
begin
  var Root := TJSONObject.Create;
  try
    for var Version in Versions do
    begin
      var Item := TJSONObject.Create;
      Root.AddPair(Version.Version, Item);

      if Version.ReleaseDate <> 0 then
        Item.AddPair('release_date', DateToISO8601(TTimeZone.Local.ToUniversalTime(Version.ReleaseDate)));
    end;
    OutputJson(Root);
  finally
    Root.Free;
  end;
end;

procedure OutputAsText(Versions: TList<TOutputVersion>);
begin
  for var Version in Versions do
    WriteLn(Format('%-11s', [Version.Version]));
end;

function AddApiVersions(const Server: TServerConfig; const ListedVersions: TList<TOutputVersion>): Boolean;
begin
  var Repo := CreateRepositoryManager(Config.Folders.CredentialsFile(Server.Name), FetchOptions, Server.Url, Server.Name, true);
  try
    var Versions := TObjectList<TRepositoryUserVersion>.Create;
    try
      Result := Repo.GetProductVersions(ProductId, Versions);
      if not Result then Exit;

      for var Version in Versions do
        if not (Version.LicenseStatus in [TLicenseStatus.none]) then
          ListedVersions.Add(TOutputVersion.Create(
            Version.Version,
            Version.LicenseStatus,
            Version.ReleaseDate,
            Version.FileHash
          ));
    finally
      Versions.Free;
    end;
  finally
    Repo.Free;
  end;
end;

function AddZipFileVersions(const ListedVersions: TList<TOutputVersion>): Boolean;
begin
  var Products := TObjectList<TRegisteredVersionedProduct>.Create;
  try
    RegisteredVCSRepos('').GetProducts(TProductVersion.Create(ProductId, ''), Products, nil, nil);
    Result := Products.Count > 0;
    if not Result then Exit;

    var Product := Products[0];

    // only retrieve versions for git repositories. If not git, just return an empty version list,
    // but no errors since the product was found.
    if Product.Product.Protocol <> TVCSProtocol.Git then Exit;

    // Find versions for git repo.
    var RepoFolder := TVCSManager.GetProductFolder(Product.Product.ProductId);
    var TempGUIDProductFolder := TPath.Combine(Config.Folders.VCSTempFolder, GuidToStringN(TGUID.NewGuid));

    var Engine := TVCSFactory.Instance.GetEngine(Product.Product.Protocol);
    for var VersionName in Engine.GetVersionNames(RepoFolder, TempGUIDProductFolder, Config.Folders.LockedFilesFolder, Product.Product.Url) do
      ListedVersions.Add(TOutputVersion.Create(VersionName.Version, TLicenseStatus.licensed, VersionName.Date, ''));
  finally
    Products.Free;
  end;
end;

procedure RunVersionsRemoteCommand;
begin
  InitFolderBasedCommand(EnableLog);

  var Found := False;
  var OutputVersions := TList<TOutputVersion>.Create;
  try
    for var i := 0 to Config.ServerConfig.ServerCount - 1 do
    begin
      var Server := Config.ServerConfig.GetServer(i);
      if not Server.Enabled then Continue;

      case Server.ServerType of
        TServerType.Api: Found := AddApiVersions(Server, OutputVersions);
        TServerType.ZipFile: Found := AddZipFileVersions(OutputVersions);
      end;

      if Found then Break;
    end;

    if not Found then
      raise Exception.Create(Format('Could not find any product matching %s', [ProductId]));

    if UseJson then
      OutputAsJson(OutputVersions)
    else
      OutputAsText(OutputVersions);

  finally
    OutputVersions.Free;
  end;
end;

procedure RegisterVersionsRemoteCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('versions-remote', '', 'List installable versions of a product',
    '',
    'versions-remote <product-id>');

  RegisterRepoOption(cmd);

  var option := cmd.RegisterUnNamedOption<string>('id of the product to list available versions for', 'product-id',
    procedure(const Value: string)
    begin
      ProductId := Value;
    end);
  option.Required := True;

  option := cmd.RegisterOption<Boolean>('log', '', 'enable logging for this command',
    procedure(const Value: Boolean)
    begin
      EnableLog := Value;
    end);
  option.HasValue := False;
  option.Hidden := True;

  option := cmd.RegisterOption<Boolean>('json', '', 'output data in JSON format',
    procedure(const Value: Boolean)
    begin
      UseJson := Value;
    end);
  option.HasValue := False;

  AddCommand(cmd.Name, CommandGroups.Status, RunVersionsRemoteCommand);
end;

{ TOutputVersion }

constructor TOutputVersion.Create(const aVersion: string; const aLicenseStatus: TLicenseStatus;
  const aReleaseDate: TDateTime; const aFileHash: string);
begin
  Version := aVersion;
  LicenseStatus := aLicenseStatus;
  FileHash := aFileHash;
  ReleaseDate := aReleaseDate;
end;

end.

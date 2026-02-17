unit Fetching.InfoFile;

interface
uses Classes, SysUtils, Deget.Version;

type
  TFetchInfoFile = class
  public const
    FileName = 'tmsfetch.info.txt';
  private const
    InfoProduct = 'product_id';
    InfoVersion = 'version';
    InfoVersionType = 'version_type';
    InfoChannel = 'channel';
    InfoServer = 'server';
    InfoPinned = 'pinned';
  private
    FProductId: string;
    FProductPath: string;
    FVersion: TLenientVersion;
    FChannel: string;
    FServer: string;
    FPinned: boolean;
    class function GetVersion(const aVersion, aVersionType,
      aServer: string): TLenientVersion; static;
  public
    constructor Create; overload;
    constructor Create(const aProductId, aProductPath: string; const aVersion: TLenientVersion; const aChannel, aServer: string; const aPinned: boolean); overload;

    function DisplayName: string;

    property ProductId: string read FProductId write FProductId;
    property ProductPath: string read FProductPath write FProductPath;
    property Version: TLenientVersion read FVersion write FVersion;
    property Channel: string read FChannel write FChannel;
    property Server: string read FServer write FServer;
    property Pinned: boolean read FPinned write FPinned;
  public
    class var ProductsWereModified: boolean;
    class function FromFile(const FileName: string): TFetchInfoFile;
    class procedure SaveInFolder(const Folder, ProductId: string; const Version: TLenientVersion; const Server: string; const Pinned: boolean);
  end;

implementation
uses IOUtils, UTmsBuildSystemUtils;

{ TFetchInfoFile }

constructor TFetchInfoFile.Create(const aProductId, aProductPath: string; const aVersion: TLenientVersion; const aChannel, aServer: string; const aPinned: boolean);
begin
  FProductId := aProductId;
  FProductPath := aProductPath;
  FVersion := aVersion;
  FChannel := aChannel;
  FServer := aServer;
  FPinned := aPinned;
end;

constructor TFetchInfoFile.Create;
begin
end;

function TFetchInfoFile.DisplayName: string;
begin
  var FullVersion := Version.ToString;
  if (Channel <> '') and not SameText(Channel, 'production') then
    FullVersion := FullVersion + '-' + Channel;
  Result := Format('%s (%s)', [ProductId, FullVersion]);
end;

class function TFetchInfoFile.GetVersion(const aVersion, aVersionType, aServer: string): TLenientVersion;
begin
  if aVersionType = '' then //old file, we will guess it from the server.
  begin
    if aServer = 'tms' then exit(TLenientVersion.Create(aVersion, TVersionType.Semantic))
    else exit(TLenientVersion.Create(aVersion, TVersionType.FreeForm));
  end;
  Result := TLenientVersion.Create(aVersion, aVersionType);
end;

class function TFetchInfoFile.FromFile(const FileName: string): TFetchInfoFile;
begin
  Result := TFetchInfoFile.Create;
  try
    var VersionStr := '';
    var VersionTypeStr := '';
    var Lines := TFile.ReadAllLines(FileName, TUTF8NoBOMEncoding.Instance);
    for var line in Lines do
    begin
      if line.Trim.StartsWith(InfoProduct + ':') then Result.ProductId := line.Trim.Substring(Length(InfoProduct + ':')).Trim;
      if line.Trim.StartsWith(InfoVersion + ':') then VersionStr := line.Trim.Substring(Length(InfoVersion + ':')).Trim;
      if line.Trim.StartsWith(InfoVersionType + ':') then VersionTypeStr := line.Trim.Substring(Length(InfoVersionType + ':')).Trim;
      if line.Trim.StartsWith(InfoServer + ':') then Result.Server := line.Trim.Substring(Length(InfoServer + ':')).Trim;
      if line.Trim.StartsWith(InfoChannel + ':') then Result.Channel := line.Trim.Substring(Length(InfoChannel + ':')).Trim;
      if line.Trim.StartsWith(InfoPinned + ':') then Result.Pinned := StrToBool(line.Trim.Substring(Length(InfoPinned + ':')).Trim);
    end;
    Result.ProductPath := TPath.GetDirectoryName(FileName);

    Result.Version := GetVersion(VersionStr, VersionTypeStr, Result.Server);

    if Result.ProductId = '' then raise Exception.Create('Error reading file "' + FileName + '". Invalid Product Id.');
//    if Result.Version = '' then raise Exception.Create('Error reading file "' + FileName + '". Invalid Version.');
//    if Result.Channel = '' then raise Exception.Create('Error reading file "' + FileName + '". Invalid Channel.');
  except
    Result.Free;
    raise;
  end;
end;

class procedure TFetchInfoFile.SaveInFolder(const Folder, ProductId: string; const Version: TLenientVersion; const Server: string; const Pinned: boolean);
begin
  ProductsWereModified:= true;
  TDirectory_CreateDirectory(Folder);
  var sw := TStreamWriter.Create(CombinePath(Folder, TFetchInfoFile.FileName), false, TUTF8NoBOMEncoding.Instance);
  try
    sw.WriteLine(InfoProduct + ': ' + ProductId);
    sw.WriteLine(InfoVersion + ': ' + Version.ToString);
    sw.WriteLine(InfoVersionType + ': ' + Version.VersionTypeId);
    sw.WriteLine(InfoServer + ': ' + Server);
    sw.WriteLine(InfoPinned + ': ' + BoolToStr(Pinned, false));
  finally
    sw.Free;
  end;
end;


end.

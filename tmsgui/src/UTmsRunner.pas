unit UTmsRunner;

interface

uses
//  WinApi.Windows,
  System.SysUtils, System.Classes, System.IOUtils, System.JSON, Deget.CommandLine, UProductInfo, UMultiLogger, Deget.Version,
  UConfigInfo;

type
  TProgressInfo = record
    Percent: Integer;
    ProductId: string;
    ProductPercent: Integer;
  end;

  TProgressCallback = reference to procedure(const Info: TProgressInfo);

  TOutputLineProc = reference to procedure(const Line: string);

  TTmsRunner = class
  private
    FExeFileName: string;
    FWorkingDir: string;
    FOutput: TStrings;
    FJsonOutput: TJSONValue;
    FOnOutputLine: TOutputLineProc;
    FIsCanceled: Boolean;
    FAlertNewVersion: TStrings;
    FAlertDiskSpace: TStrings;
    FIgnoreExitCode: Boolean;
    FLastExitCode: Integer;
    FRepository: string;
    procedure Run(const Params: string; Callback: TStringProc = nil);
    function JsonOutput: TJSONValue;
    procedure ProcessAlerts;
  protected
    function RemoveLogMetadata(const S: string): string;
    procedure CheckLastExitCode(const Msg: string);
    function AddRepo(const Command: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Cancel;

    property ExeFileName: string read FExeFileName write FExeFileName;
    property WorkingDir: string read FWorkingDir write FWorkingDir;
    property Repository: string read FRepository write FRepository;
    property IgnoreExitCode: Boolean read FIgnoreExitCode write FIgnoreExitCode;
    property LastExitCode: Integer read FLastExitCode;
    property IsCanceled: Boolean read FIsCanceled;
    function NewVersionDetected: Boolean;
    property Output: TStrings read FOutput;
    property OnOutputLine: TOutputLineProc read FOnOutputLine write FOnOutputLine;
  end;

  TTmsListRunner = class(TTmsRunner)
  strict private
    FProducts: TProductInfoList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RunList;
    property Products: TProductInfoList read FProducts;
  end;

  TTmsListRemoteRunner = class(TTmsRunner)
  strict private
    FProducts: TProductInfoList;
    FServer: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RunListRemote;
    property Products: TProductInfoList read FProducts;
    property Server: string read FServer write FServer;
  end;

  TAbstractTmsBuildRunner = class(TTmsRunner)
  strict private
    FProductIds: TStrings;
    FSummary: TStrings;
    FOnProgress: TProgressCallback;
    FProductProgress: Boolean;
  protected
    procedure RunCommand(const Command: string);
    property ProductProgress: Boolean read FProductProgress write FProductProgress;
  public
    constructor Create;
    destructor Destroy; override;
    property ProductIds: TStrings read FProductIds;
    property Summary: TStrings read FSummary;
    property OnProgress: TProgressCallback read FOnProgress write FOnProgress;
  end;

  TTmsBuildRunner = class(TAbstractTmsBuildRunner)
  private
    FFullBuild: Boolean;
  public
    procedure RunBuild;
    property FullBuild: Boolean read FFullBuild write FFullBuild;
  end;

  TTmsInstallRunner = class(TAbstractTmsBuildRunner)
  public
    procedure RunInstall;
  end;

  TTmsUninstallRunner = class(TAbstractTmsBuildRunner)
  public
    procedure RunUninstall;
  end;

  TTmsInfo = class
  private
    FVersion: TVersion;
    FLocation: string;
    FWorkingFolder: string;
    FFolderInitialized: Boolean;
    FHasCredentials: Boolean;
    FConfigFile: string;
  public
    property Version: TVersion read FVersion write FVersion;
    property Location: string read FLocation write FLocation;
    property WorkingFolder: string read FWorkingFolder write FWorkingFolder;
    property FolderInitialized: Boolean read FFolderInitialized write FFolderInitialized;
    property HasCredentials: Boolean read FHasCredentials write FHasCredentials;
    property ConfigFile: string read FConfigFile write FConfigFile;
  end;

  TTmsInfoRunner = class(TTmsRunner)
  strict private
    FInfo: TTmsInfo;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RunInfo; overload;
    procedure RunInfo(Info: TTmsInfo); overload;
    property Info: TTmsInfo read FInfo;
  end;

  TTmsCredentialsRunner = class(TTmsRunner)
  public
    procedure RunGetCredentials(out Email, Code: string);

    // Returns true if credentials updated correctly, false if they are invalid
    function RunUpdateCredentials(const Email, Code: string): Boolean;
  end;

  TTmsConfigureRunner = class(TTmsRunner)
  public
    procedure RunConfigure;
  end;

  TTmsSelfUpdateRunner = class(TTmsRunner)
  public
    function RunSelfUpdate: boolean;
  end;

  TTmsConfigReadRunner = class(TTmsRunner)
  strict private
    FValue: string;
  public
    function RunConfigRead(const ParamName: string): string;
  end;

  TTmsConfigWriteRunner = class(TTmsRunner)
  public
    procedure RunConfigWrite(const ParamName, ParamValue: string);
  end;

  TTmsServerListRunner = class(TTmsRunner)
  public
    procedure RunServerList(Items: TServerConfigItems);
  end;

  TTmsServerAddRunner = class(TTmsRunner)
  public
    procedure RunServerAdd(Item: TServerConfigItem);
  end;

  TTmsServerEnableRunner = class(TTmsRunner)
  public
    procedure RunServerEnable(const Name: string; Enabled: Boolean);
  end;

  TTmsServerRemoveRunner = class(TTmsRunner)
  public
    procedure RunServerRemove(const Name: string);
  end;

  ETmsRunner = class(Exception)
  end;

implementation

uses
  Deget.CoreTypes;

{ TTmsRunner }

function TTmsRunner.AddRepo(const Command: string): string;
begin
  Result := Command;
  if Repository <> '' then
    Result := Result + ' -repo:' + Repository;
end;

procedure TTmsRunner.Cancel;
begin
  FIsCanceled := True;
end;

procedure TTmsRunner.CheckLastExitCode(const Msg: string);
begin
  if LastExitCode <> 0 then
    raise ETmsRunner.CreateFmt('Running failed: %s', [Msg]);
end;

constructor TTmsRunner.Create;
begin
  FExeFileName := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'tms.exe');
  if not TFile.Exists(FExeFileName) then
    FExeFileName := 'tms.exe';

  FWorkingDir := GetCurrentDir;
  FOutput := TStringList.Create;

  FAlertNewVersion := TStringList.Create;
  FAlertDiskSpace := TStringList.Create;
end;

destructor TTmsRunner.Destroy;
begin
  FAlertNewVersion.Free;
  FAlertDiskSpace.Free;
  FOutput.Free;
  FJsonOutput.Free;
  inherited;
end;

procedure TTmsRunner.Run(const Params: string; Callback: TStringProc);
begin
//  if not TFile.Exists(ExeFileName) then
//    raise ETmsRunner.Create('tms.exe could not be found at ' + TPath.GetDirectoryName(ExeFileName));

  var LastOutputIndex := 0;
  var FullOutput := '';

  var CommandLine := Format('"%s" %s', [ExeFileName, Params]);
  var LocalCallback: TStringProc :=
    procedure(const S: string)
    begin
      FullOutput := FullOutput + S;
      FOutput.Text := FullOutput;

//      if Assigned(Callback) then
//        Callback(S);

      // Output lines except the last one, which might be incomplete
      if Assigned(FOnOutputLine) or Assigned(Callback) then
        while LastOutputIndex < FOutput.Count -1 do
        begin
          if Assigned(FOnOutputLine) then
            FOnOutputLine(FOutput[LastOutputIndex]);
          if Assigned(Callback) then
            Callback(FOutput[LastOutputIndex]);
          Inc(LastOutputIndex);
        end;
    end;

  var IsCanceledCallback: TFunc<Boolean> :=
    function: Boolean
    begin
      Result := FIsCanceled;
    end;

  FOutput.Clear;
  var ExitCode := TCommandLine.ExecuteEx(CommandLine, WorkingDir, LocalCallback, nil, IsCanceledCallback);

  if Assigned(FOnOutputLine) or Assigned(Callback) then
    while LastOutputIndex < FOutput.Count do
    begin
      if Assigned(FOnOutputLine) then
        FOnOutputLine(FOutput[LastOutputIndex]);
      if Assigned(Callback) then
        Callback(FOutput[LastOutputIndex]);
      Inc(LastOutputIndex);
    end;

  ProcessAlerts;

  // Handle bad execution
  FLastExitcode := ExitCode;
  if not IgnoreExitCode then
    CheckLastExitCode(Params);
end;

function TTmsRunner.JsonOutput: TJSONValue;
begin
  if FJsonOutput = nil then
    FJsonOutput := TJSONValue.ParseJSONValue(Output.Text);
  Result := FJsonOutput;
end;

function TTmsRunner.NewVersionDetected: Boolean;
begin
  Result := FAlertNewVersion.Count > 0;
end;

procedure TTmsRunner.ProcessAlerts;

  procedure AddNextLines(Target: TStrings; var Index: Integer; Lines: Integer);
  begin
    while (Lines > 0) and (Index < Output.Count) do
    begin
      Target.Add(Output[Index]);
      Output.Delete(Index);
      Dec(Lines);
    end;
  end;

begin
  // Process the extra alerts the command-line output might include, and remove them from the output
  // so the command-line parsing won't fail because of them.
  var I := 0;
  while I < Output.Count do
  begin
    var Line := Trim(Output[I]);

    if Line.StartsWith('There is a new version of TMS Smart Setup available') then
      AddNextLines(FAlertNewVersion, I, 3)
    else
    if Line.StartsWith('WARNING: You only have') and Line.EndsWith('MB left in disk. TMS Smart Setup might not work properly.') then
      AddNextLines(FAlertDiskSpace, I, 2)
    else
      Inc(I);
  end;
end;

function TTmsRunner.RemoveLogMetadata(const S: string): string;
begin
  var P := Pos(']', S);
  if P > 0 then
    Result := Copy(S, P + 1)
  else
    Result := S;
end;

{ TTmsListRunner }

constructor TTmsListRunner.Create;
begin
  inherited Create;
  FProducts := TProductInfoList.Create;
end;

destructor TTmsListRunner.Destroy;
begin
  FProducts.Free;
  inherited;
end;

procedure TTmsListRunner.RunList;
begin
  Run('list -detailed -json');

  // parse response
  if not (JsonOutput is TJSONObject) then
    raise ETmsRunner.Create('Could not parse list result as JSON object');

  var Json := TJSONObject(JsonOutput);
  for var Pair in Json do
  begin
    var Product := TProductInfo.Create;
    Products.Add(Product);
    Product.Id := Pair.JsonString.Value;
    if not (Pair.JsonValue is TJSONObject) then
      raise ETmsRunner.CreateFmt('Could not parse product "%s" as JSON object from list command', [Product.Id]);
    var JsonProduct := TJSONObject(Pair.JsonValue);

    Product.Name := JsonProduct.GetValue<string>('name');
    Product.Version := JsonProduct.GetValue<string>('version', '');
    Product.Channel := JsonProduct.GetValue<string>('channel', '');
    Product.Local := JsonProduct.GetValue<Boolean>('local', False);
    var JsonIDEs: TJSONObject;
    if JsonProduct.TryGetValue('ides', JsonIDEs) then
      for var IDEPair in JsonIDEs do
      begin
        var IDEInfo := Product.IDEInfo(GetIDEName(IDEPair.JsonString.Value));
        var JsonPlatforms: TJSONObject;
        if IDEPair.JsonValue.TryGetValue('platforms', JsonPlatforms) then
          for var PlatformPair in JsonPlatforms do
          begin
            var PlatInfo := IDEInfo.PlatformInfo(GetPlatformName(PlatformPair.JsonString.Value));
            var JsonPlat: TJSONObject;
            if PlatformPair.JsonValue.TryGetValue(JsonPlat) then
            begin
              PlatInfo.IsBuilt := JsonPlat.GetValue('built', False);
              PlatInfo.IsRegistered := JsonPlat.GetValue('registered', False);
            end;
          end;
      end;
  end;
end;

{ TAbstractTmsBuildRunner }

constructor TAbstractTmsBuildRunner.Create;
begin
  inherited;
  FSummary := TStringList.Create;
  FProductIds := TStringList.Create;
end;

destructor TAbstractTmsBuildRunner.Destroy;
begin
  FSummary.Free;
  FProductIds.Free;
  inherited;
end;

procedure TAbstractTmsBuildRunner.RunCommand(const Command: string);
begin
  var LocalCallback: TStringProc :=
    procedure(const S: string)
    begin
      if not Assigned(FOnProgress) then Exit;

      var Info := Default(TProgressInfo);
      var P := S.IndexOf('%]') + 1;
      if P >= 4 then
      begin
        if TryStrToInt(Trim(Copy(S, P - 3, 3)), Info.Percent) then
        begin
          // parse product progress
          if ProductProgress then
          begin
            var P3 := S.IndexOf('%]', P) + 1;
            var P1 := S.IndexOf('[', P) + 1;
            var P2 := S.IndexOf(':', P) + 1;

            // Ensure format is [<product-id>: <percent>%]
            if ((P3 - P2) >= 3) and ((P2 - P1) >= 1) then
            begin
              Info.ProductId := Trim(Copy(S, P1 + 1, P2 - P1 - 1));
              TryStrToInt(Trim(Copy(S, P3 - 3, 3)), Info.ProductPercent);
            end;
          end;
          FOnProgress(Info);
        end;
      end;
    end;

  Run(Command, LocalCallback);

  // extract summary
  var InSummary := False;
  var Summary: TArray<string>;
  for var Line in Output do
  begin
    var CleanLine := RemoveLogMetadata(Line);
    if Pos('=== Build Summary ===', CleanLine) > 0 then
    begin
      InSummary := True;
      Continue;
    end;
    if InSummary then
    begin
      if Trim(CleanLine) = '' then
        Break
      else
        Summary := Summary + [CleanLine];
    end;
  end;
end;

{ TTmsListRemoteRunner }

constructor TTmsListRemoteRunner.Create;
begin
  inherited Create;
  FProducts := TProductInfoList.Create;
end;

destructor TTmsListRemoteRunner.Destroy;
begin
  FProducts.Free;
  inherited;
end;

procedure TTmsListRemoteRunner.RunListRemote;
begin
  var Command := 'list-remote -json';
  if Server <> '' then
    Command := Command + ' -server:' + Server;
  Run(AddRepo(Command));

  // parse response
  if not (JsonOutput is TJSONObject) then
    raise ETmsRunner.Create('Could not parse list result as JSON object');

  var Json := TJSONObject(JsonOutput);
  for var Pair in Json do
  begin
    var Product := TProductInfo.Create;
    Products.Add(Product);
    Product.Id := Pair.JsonString.Value;
    if not (Pair.JsonValue is TJSONObject) then
      raise ETmsRunner.CreateFmt('Could not parse product "%s" as JSON object from list command', [Product.Id]);
    var JsonProduct := TJSONObject(Pair.JsonValue);

    Product.Name := JsonProduct.GetValue<string>('name');
    Product.Version := JsonProduct.GetValue<string>('version', '');
    Product.VendorId := JsonProduct.GetValue<string>('vendor_id', '');
    Product.Server := JsonProduct.GetValue<string>('server', '');
//    Product.LicenseStatus := JsonProduct.GetValue<string>('license-status', '');
  end;
end;

{ TTmsInfoRunner }

constructor TTmsInfoRunner.Create;
begin
  inherited Create;
  FInfo := TTmsInfo.Create;
end;

destructor TTmsInfoRunner.Destroy;
begin
  FInfo.Free;
  inherited;
end;

procedure TTmsInfoRunner.RunInfo;
begin
  RunInfo(Info);
end;

procedure TTmsInfoRunner.RunInfo(Info: TTmsInfo);
begin
  Run(AddRepo('info -json'));

  // parse response
  if not (JsonOutput is TJSONObject) then
    raise ETmsRunner.Create('Could not parse info result as JSON object');

  var Json := TJSONObject(JsonOutput);
  Info.Version := Json.GetValue('tms version', '');
  Info.Location := Json.GetValue('tms location', '');
  Info.WorkingFolder := Json.GetValue('working folder', '');
  Info.FolderInitialized := Json.GetValue('folder initialized', False);
  Info.HasCredentials := Json.GetValue('has credentials', False);
  Info.ConfigFile := Json.GetValue('config file', '');
end;

{ TTmsCredentialsRunner }

procedure TTmsCredentialsRunner.RunGetCredentials(out Email, Code: string);
begin
  IgnoreExitCode := False;
  Run(AddRepo('credentials -print -json'));

  // parse response
  if not (JsonOutput is TJSONObject) then
    raise ETmsRunner.Create('Could not parse info result as JSON object');

  var Json := TJSONObject(JsonOutput);
  Email := Json.GetValue('email', '');
  Code := Json.GetValue('code', '');
end;

function TTmsCredentialsRunner.RunUpdateCredentials(const Email, Code: string): Boolean;
begin
  Result := false;
  IgnoreExitCode := True;
  var Command := Format('credentials -email:%s -code:%s -check', [Email, Code]);
  Run(AddRepo(Command));

  if LastExitCode = 0 then Exit(True);
  if Output.Text.ToLower.Contains('error: oauth2:') then
    Exit(False);
  CheckLastExitCode(Command);
end;

{ TTmsBuildRunner }

procedure TTmsBuildRunner.RunBuild;
begin
  ProductProgress := True;

  var Command := 'build';
  if ProductIds.Count > 0 then
    Command := Command + ' ' + String.Join(' ', ProductIds.ToStringArray);
  if FullBuild then
    Command := Command + ' -full';
  if ProductProgress then
    Command := Command + ' -display-options:product-progress';

  RunCommand(AddRepo(Command));
end;

{ TTmsInstallRunner }

procedure TTmsInstallRunner.RunInstall;
begin
  ProductProgress := True;

  var Command := 'install';
  Command := Command + ' ' + String.Join(' ', ProductIds.ToStringArray);
  if ProductProgress then
    Command := Command + ' -display-options:product-progress';

  RunCommand(AddRepo(Command));
end;

{ TTmsUninstallRunner }

procedure TTmsUninstallRunner.RunUninstall;
begin
  var Command := 'uninstall';
  Command := Command + ' ' + String.Join(' ', ProductIds.ToStringArray);

  RunCommand(Command);
end;

{ TTmsSelfUpdateRunner }

function TTmsSelfUpdateRunner.RunSelfUpdate: boolean;
begin
  Run(AddRepo('self-update'));

  // Quick and dirty, but what better option do we have?
  Result := Pos('TMS Smart Setup has been updated from version', Output.Text) > 0;
end;

{ TTmsConfigureRunner }

procedure TTmsConfigureRunner.RunConfigure;
begin
  Run('config');
end;

{ TTmsConfigReadRunner }

function TTmsConfigReadRunner.RunConfigRead(const ParamName: string): string;
begin
  var Command := 'config-read';
  Command := Command + ' ' + ParamName.QuotedString('"');
  Run(Command);
  FValue := Output.Text;
  Result := FValue;
end;

{ TTmsConfigWriteRunner }

procedure TTmsConfigWriteRunner.RunConfigWrite(const ParamName, ParamValue: string);
begin
  var Command := 'config-write';
  Command := Command + ' -p:' + (ParamName + '=' + ParamValue).QuotedString('"');
  Run(Command);
end;

{ TTmsServerListRunner }

procedure TTmsServerListRunner.RunServerList(Items: TServerConfigItems);
begin
  Run('server-list -json');

  // parse response
  if not (JsonOutput is TJSONObject) then
    raise ETmsRunner.Create('Could not parse server list result as JSON object');

  Items.Clear;
  var Json := TJSONObject(JsonOutput);
  for var Pair in Json do
  begin
    var Item := TServerConfigItem.Create;
    Items.Add(Item);
    Item.Name := Pair.JsonString.Value;

    if not (Pair.JsonValue is TJSONObject) then
      raise ETmsRunner.CreateFmt('Could not parse server "%s" as JSON object from server-list command', [Item.Name]);
    var JsonItem := TJSONObject(Pair.JsonValue);

    // name property is redundant
//    Product.Name := JsonItem.GetValue<string>('name');
    Item.Url := JsonItem.GetValue<string>('url', '');
    Item.Protocol := JsonItem.GetValue<string>('protocol', '');
    Item.Enabled := JsonItem.GetValue<Boolean>('enabled', False);
  end;
end;

{ TTmsServerAddRunner }

procedure TTmsServerAddRunner.RunServerAdd(Item: TServerConfigItem);
begin
  var Command := Format('server-add %s %s %s %s', [Item.Name, Item.Protocol, Item.Url, BoolToStr(Item.Enabled, True)]);
  Run(Command);
end;

{ TTmsServerEnableRunner }

procedure TTmsServerEnableRunner.RunServerEnable(const Name: string;
  Enabled: Boolean);
begin
  var Command := Format('server-enable %s %s', [Name, BoolToStr(Enabled, True)]);
  Run(Command);
end;

{ TTmsServerRemoveRunner }

procedure TTmsServerRemoveRunner.RunServerRemove(const Name: string);
begin
  var Command := Format('server-remove %s', [Name]);
  Run(Command);
end;

end.

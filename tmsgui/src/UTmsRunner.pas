unit UTmsRunner;

interface

uses
//  WinApi.Windows,
  System.SysUtils, System.Classes, System.IOUtils, System.JSON, Deget.CommandLine, UProductInfo, UMultiLogger, Deget.Version,
  UCommonTypes, Generics.Collections;

type
  TProgressInfo = record
    Percent: Integer;
    ProductId: string;
    ProductPercent: Integer;
  end;

  TProgressCallback = reference to procedure(const Info: TProgressInfo);

  TOutputLineProc = reference to procedure(const Line: string);

  // One per tms.exe command we run; each runner says which one it is. Full and partial builds are both Build.
  {$SCOPEDENUMS ON}
  TTmsCommand = (List, ListRemote, Info, Credentials, Configure, LogView, SelfUpdate,
    ConfigRead, ConfigWrite, ServerList, ServerAdd, ServerEnable, ServerRemove,
    VersionsRemote, Build, Install, Uninstall, Pin, Unpin, Backup, Restore, SBOM, Doctor);
  {$SCOPEDENUMS OFF}

  TTmsRunner = class
  private
    FExeFileName: string;
    FWorkingDir: string;
    FOutput: TList<string>;
    FJsonOutput: TJSONValue;
    FOnOutputLine: TOutputLineProc;
    FIsCanceled: Boolean;
    FAlertNewVersion: TStrings;
    FAlertDiskSpace: TStrings;
    FIgnoreExitCode: Boolean;
    FLastExitCode: Integer;
    FRepository: string;
    FParams: string;
    procedure Run(const aParams: string; Callback: TStringProc = nil);
    function JsonOutput: TJSONValue;
    procedure ProcessAlerts;
  protected
    function GetParams: string; virtual;
    function RemoveLogMetadata(const S: string): string;
    procedure CheckLastExitCode(const Msg: string);
    function AddRepo(const Command: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Cancel;

    class function Command: TTmsCommand; virtual; abstract;

    property ExeFileName: string read FExeFileName write FExeFileName;
    property WorkingDir: string read FWorkingDir write FWorkingDir;
    property Repository: string read FRepository write FRepository;
    property IgnoreExitCode: Boolean read FIgnoreExitCode write FIgnoreExitCode;
    property LastExitCode: Integer read FLastExitCode;
    // What the last run passed to tms.exe, for the log. Runners whose params shouldn't be shown (credentials) override GetParams.
    property Params: string read GetParams;
    property IsCanceled: Boolean read FIsCanceled;
    function NewVersionDetected: Boolean;
    property Output: TList<String> read FOutput;
    function OutputText: string;
    function CmdAndOutput: string; virtual;
    property OnOutputLine: TOutputLineProc read FOnOutputLine write FOnOutputLine;
  end;

  TTmsListRunner = class(TTmsRunner)
  strict private
    FProducts: TProductInfoList;
  public
    class function Command: TTmsCommand; override;
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
    class function Command: TTmsCommand; override;
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
    class function Command: TTmsCommand; override;
    procedure RunBuild;
    property FullBuild: Boolean read FFullBuild write FFullBuild;
  end;

  TTmsInstallRunner = class(TAbstractTmsBuildRunner)
  public
    class function Command: TTmsCommand; override;
    procedure RunInstall;
  end;

  TTmsUninstallRunner = class(TAbstractTmsBuildRunner)
  public
    class function Command: TTmsCommand; override;
    procedure RunUninstall;
  end;

  // A record, so each caller gets its own copy: it is read from several threads and refreshed while they do.
  TTmsInfo = record
  private
    FInitialized: boolean;
    FVersion: TVersion;
    FLocation: string;
    FWorkingFolder: string;
    FIsValidTMSSetupFolder: Boolean;
    FHasCredentials: Boolean;
    FConfigFile: string;
    function GetWorkingFolder: string;
  public
    property Initialized: boolean read FInitialized write FInitialized;
    property Version: TVersion read FVersion write FVersion;
    property Location: string read FLocation write FLocation;
    property WorkingFolder: string read GetWorkingFolder write FWorkingFolder;
    property IsValidTMSSetupFolder: Boolean read FIsValidTMSSetupFolder write FIsValidTMSSetupFolder;
    property HasCredentials: Boolean read FHasCredentials write FHasCredentials;
    property ConfigFile: string read FConfigFile write FConfigFile;
  end;

  TTmsInfoRunner = class(TTmsRunner)
  public
    class function Command: TTmsCommand; override;
    procedure RunInfo(var Info: TTmsInfo);
  end;

  TTmsCredentialsRunner = class(TTmsRunner)
  protected
    function GetParams: string; override;
  public
    class function Command: TTmsCommand; override;
    function CmdAndOutput: string; override;

    procedure RunGetCredentials(out Email, Code: string);

    // Returns true if credentials updated correctly, false if they are invalid
    function RunUpdateCredentials(const Email, Code: string): Boolean;
  end;

  TTmsConfigureRunner = class(TTmsRunner)
  public
    class function Command: TTmsCommand; override;
    procedure RunConfigure(Silent: Boolean);
  end;

  TTmsLogViewRunner = class(TTmsRunner)
  public
    class function Command: TTmsCommand; override;
    function RunLogView(const SessionId: string; const Print: boolean): string;
  end;

  TTmsSelfUpdateRunner = class(TTmsRunner)
  public
    class function Command: TTmsCommand; override;
    function RunSelfUpdate: boolean;
  end;

  TTmsConfigReadRunner = class(TTmsRunner)
  strict private
    FValue: string;
  public
    class function Command: TTmsCommand; override;
    function RunConfigRead(const ParamName: string): string;
  end;

  TTmsConfigWriteRunner = class(TTmsRunner)
  public
    class function Command: TTmsCommand; override;
    procedure RunConfigWrite(const ParamName, ParamValue: string);
  end;

  TTmsServerListRunner = class(TTmsRunner)
  public
    class function Command: TTmsCommand; override;
    procedure RunServerList(Items: TServerConfigItems);
  end;

  TTmsServerAddRunner = class(TTmsRunner)
  public
    class function Command: TTmsCommand; override;
    procedure RunServerAdd(Item: TServerConfigItem);
  end;

  TTmsServerEnableRunner = class(TTmsRunner)
  public
    class function Command: TTmsCommand; override;
    procedure RunServerEnable(const Name: string; Enabled: Boolean);
  end;

  TTmsServerRemoveRunner = class(TTmsRunner)
  public
    class function Command: TTmsCommand; override;
    procedure RunServerRemove(const Name: string);
  end;

  TTmsVersionsRemoteRunner = class(TTmsRunner)
  public
    class function Command: TTmsCommand; override;
    procedure RunVersionsRemote(const ProductId: string; Items: TVersionInfoList);
  end;

  TTmsPinRunner = class(TTmsRunner)
  public
    class function Command: TTmsCommand; override;
    procedure RunPin(const ProductIds: TArray<string>);
  end;

  TTmsUnpinRunner = class(TTmsRunner)
  public
    class function Command: TTmsCommand; override;
    procedure RunUnpin(const ProductIds: TArray<string>);
  end;

  TTmsBackupRunner = class(TTmsRunner)
  public
    class function Command: TTmsCommand; override;
    procedure RunBackup(const TargetFile: string; const ProductIds: TArray<string>);
  end;

  TTmsRestoreRunner = class(TAbstractTmsBuildRunner)
  public
    class function Command: TTmsCommand; override;
    procedure RunRestore(const BackupFile: string; const SkipRegister: boolean; const ProductIds: TArray<string>);
  end;

  TSBOMInfo =  class
  public
    Id: string;
    FileName: string;
    Generated: boolean;
    Error: string; // empty unless generating it failed
  end;

  TTmsSBOMRunner = class(TTmsRunner)
  private
    FSBOMInfoList: TObjectList<TSBOMInfo>;
  public
    class function Command: TTmsCommand; override;
    constructor Create;
    destructor Destroy; override;
    procedure RunSBOM(const Force: boolean; const ProductIds: TArray<string>);

    property SBOMInfoList: TObjectList<TSBOMInfo> read FSBOMInfoList;
  end;

  ETmsRunner = class(Exception)
  end;

  // The run was canceled by the user, so there is no result to use. Not an error.
  ETmsRunnerCanceled = class(ETmsRunner)
  end;

// The tms.exe command name, like "list-remote".
function TmsCommandName(const Command: TTmsCommand): string;

implementation

uses
  System.DateUtils,
  Deget.CoreTypes;

function TmsCommandName(const Command: TTmsCommand): string;
const
  Names: array[TTmsCommand] of string = ('list', 'list-remote', 'info', 'credentials', 'config', 'log-view', 'self-update',
    'config-read', 'config-write', 'server-list', 'server-add', 'server-enable', 'server-remove',
    'versions-remote', 'build', 'install', 'uninstall', 'pin', 'unpin', 'backup', 'restore', 'sbom-generate', 'doctor');
begin
  Result := Names[Command];
end;

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

function TTmsRunner.CmdAndOutput: string;
begin
  Result := 'tms ' + Params + #13#10 + #13#10 + OutputText;
end;

constructor TTmsRunner.Create;
begin
  FExeFileName := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'tms.exe');
  {$IFDEF DEBUG}
  if not TFile.Exists(FExeFileName) then
    FExeFileName := 'tms.exe';
  {$ENDIF}

  FWorkingDir := GetCurrentDir;
  FOutput := TList<String>.Create;

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

function TTmsRunner.GetParams: string;
begin
  Result := FParams;
end;

procedure TTmsRunner.Run(const aParams: string; Callback: TStringProc);
begin
  var LastOutputIndex := 0;

  FParams := aParams;
{$IFDEF DEBUG}
    var TestValue: string;
  if FindCmdLineSwitch('test-tms', TestValue, true) then
  begin
    FParams := FParams + ' ' + TestValue;
  end;
{$ENDIF}

  var CommandLine := QuoteCmd(ExeFileName) + '  ' + FParams;

  var LocalCallback: TStringProc :=
    procedure(const S: string)
    begin
      var S2 := S.Replace(#13, '', [TReplaceFlag.rfReplaceAll]).Split([#10]);
      if Length(S2) > 0 then
      begin
        if FOutput.Count = 0 then FOutput.Add(S2[0]) else FOutput[FOutput.Count - 1] := FOutput.Last + S2[0];
        for var i:= 1 to Length(S2) - 1 do FOutput.Add(S2[i]);
      end;

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

  // Handle bad execution. A canceled run was killed, so its exit code says nothing; callers check IsCanceled.
  FLastExitcode := ExitCode;
  if not IgnoreExitCode and not FIsCanceled then
    CheckLastExitCode(Params);
end;

function TTmsRunner.JsonOutput: TJSONValue;
begin
  // A killed run has cut-off output (and the **CANCELED** marker). Stop here, so we don't report it as invalid JSON
  // or use a partial result.
  if FIsCanceled then
    raise ETmsRunnerCanceled.Create('Canceled by the user.');

  if FJsonOutput = nil then
    FJsonOutput := TJSONValue.ParseJSONValue(OutputText);
  Result := FJsonOutput;
end;

function TTmsRunner.NewVersionDetected: Boolean;
begin
  Result := FAlertNewVersion.Count > 0;
end;

function TTmsRunner.OutputText: string;
begin
  if FOutput.Count = 0 then Exit('');

  var SB := TStringBuilder.Create(FOutput[0]);
  try
    for var i := 1 to FOutput.Count - 1 do
    begin
      SB.Append(sLineBreak).Append(FOutput[i]);
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
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

class function TTmsListRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.List;
end;

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
    raise ETmsRunner.Create('Invalid result for `tms list -detailed -json` command. Cannot parse "' + OutputText + '" as JSON.');

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
    Product.Version := TLenientVersion.Create(JsonProduct.GetValue<string>('version', ''), JsonProduct.GetValue<string>('version_type', ''));
    Product.Channel := JsonProduct.GetValue<string>('channel', '');
    Product.Server := JsonProduct.GetValue<string>('server', '');
    Product.Local := JsonProduct.GetValue<Boolean>('local', False);
    Product.Pinned := JsonProduct.GetValue<Boolean>('pinned', False);
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

class function TTmsListRemoteRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.ListRemote;
end;

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
    raise ETmsRunner.Create('Invalid result for `tms list-remote -json` command. Cannot parse "' + OutputText + '" as JSON.');

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
    Product.Version := TLenientVersion.Create(JsonProduct.GetValue<string>('version', ''), JsonProduct.GetValue<string>('version_type', ''));
    Product.VendorId := JsonProduct.GetValue<string>('vendor_id', '');
    Product.Server := JsonProduct.GetValue<string>('server', '');
    Product.Description := JsonProduct.GetValue<string>('description', '');
    Product.Url := JsonProduct.GetValue<string>('url', '');
    Product.LicenseStatus := JsonProduct.GetValue<string>('license_status', '');
  end;
end;

{ TTmsInfoRunner }

class function TTmsInfoRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.Info;
end;

procedure TTmsInfoRunner.RunInfo(var Info: TTmsInfo);
begin
  Run(AddRepo('info -json'));

  // parse response
  if not (JsonOutput is TJSONObject) then
    raise ETmsRunner.Create('Could not parse info result as JSON object');

  var Json := TJSONObject(JsonOutput);
  Info.Version := Json.GetValue('tms version', '');
  Info.Location := Json.GetValue('tms location', '');
  Info.WorkingFolder := Json.GetValue('working folder', '');
  Info.IsValidTMSSetupFolder := Json.GetValue('folder initialized', False);
  Info.HasCredentials := Json.GetValue('has credentials', False);
  Info.ConfigFile := Json.GetValue('config file', '');
  Info.Initialized := true;
end;

{ TTmsCredentialsRunner }

function TTmsCredentialsRunner.CmdAndOutput: string;
begin
  Result := 'tms credentials';
end;

class function TTmsCredentialsRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.Credentials;
end;

function TTmsCredentialsRunner.GetParams: string;
begin
  exit('credentials'); //do not log them.
end;

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
  var Command := Format('credentials -email:%s -code:%s -check', [Email.Trim.QuotedString('"'), Code.Trim.QuotedString('"')]);
  Run(AddRepo(Command));

  if IsCanceled then
    raise ETmsRunnerCanceled.Create('Canceled by the user.');
  if LastExitCode = 0 then Exit(True);
  if OutputText.ToLower.Contains('error: oauth2:') then
    Exit(False);
  CheckLastExitCode(OutputText);
end;

{ TTmsBuildRunner }

class function TTmsBuildRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.Build;
end;

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

  RunCommand(Command);
end;

{ TTmsInstallRunner }

class function TTmsInstallRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.Install;
end;

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

class function TTmsUninstallRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.Uninstall;
end;

procedure TTmsUninstallRunner.RunUninstall;
begin
  var Command := 'uninstall';
  Command := Command + ' ' + String.Join(' ', ProductIds.ToStringArray);

  RunCommand(Command);
end;

{ TTmsSelfUpdateRunner }

class function TTmsSelfUpdateRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.SelfUpdate;
end;

function TTmsSelfUpdateRunner.RunSelfUpdate: boolean;
begin
  Run(AddRepo('self-update'));

  // Quick and dirty, but what better option do we have?
  Result := Pos('TMS Smart Setup has been updated from version', OutputText) > 0;
end;

{ TTmsConfigureRunner }

class function TTmsConfigureRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.Configure;
end;

procedure TTmsConfigureRunner.RunConfigure(Silent: Boolean);
begin
  var Command := 'config';
  if Silent then
    Command := Command + ' -print';
  Run(Command);
end;

{ TTmsConfigReadRunner }

class function TTmsConfigReadRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.ConfigRead;
end;

function TTmsConfigReadRunner.RunConfigRead(const ParamName: string): string;
begin
  var Command := 'config-read';
  Command := Command + ' ' + ParamName.QuotedString('"');
  Run(Command);
  FValue := OutputText;
  Result := FValue;
end;

{ TTmsConfigWriteRunner }

class function TTmsConfigWriteRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.ConfigWrite;
end;

procedure TTmsConfigWriteRunner.RunConfigWrite(const ParamName, ParamValue: string);
begin
  var Command := 'config-write';
  Command := Command + ' -p:' + (ParamName + '=' + ParamValue).QuotedString('"');
  Run(Command);
end;

{ TTmsServerListRunner }

class function TTmsServerListRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.ServerList;
end;

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
    Item.ServerType := JsonItem.GetValue<string>('type', '');
    Item.Enabled := JsonItem.GetValue<Boolean>('enabled', False);
  end;
end;

{ TTmsServerAddRunner }

class function TTmsServerAddRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.ServerAdd;
end;

procedure TTmsServerAddRunner.RunServerAdd(Item: TServerConfigItem);
begin
  var Command := Format('server-add %s %s %s %s', [Item.Name, Item.ServerType, Item.Url, BoolToStr(Item.Enabled, True)]);
  Run(Command);
end;

{ TTmsServerEnableRunner }

class function TTmsServerEnableRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.ServerEnable;
end;

procedure TTmsServerEnableRunner.RunServerEnable(const Name: string;
  Enabled: Boolean);
begin
  var Command := Format('server-enable %s %s', [Name, BoolToStr(Enabled, True)]);
  Run(Command);
end;

{ TTmsServerRemoveRunner }

class function TTmsServerRemoveRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.ServerRemove;
end;

procedure TTmsServerRemoveRunner.RunServerRemove(const Name: string);
begin
  var Command := Format('server-remove %s', [Name]);
  Run(Command);
end;

{ TTmsVersionsRemoteRunner }

class function TTmsVersionsRemoteRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.VersionsRemote;
end;

procedure TTmsVersionsRemoteRunner.RunVersionsRemote(const ProductId: string; Items: TVersionInfoList);
begin
  var Command := Format('versions-remote %s -json', [ProductId]);
  Run(AddRepo(Command));

  // parse response
  if not (JsonOutput is TJSONObject) then
    raise ETmsRunner.Create('Could not parse versions-remote result as JSON object');

  Items.Clear;
  var Json := TJSONObject(JsonOutput);
  for var Pair in Json do
  begin
    var Item := TVersionInfo.Create;
    Items.Add(Item);
    Item.Version := Pair.JsonString.Value;

    if not (Pair.JsonValue is TJSONObject) then
      raise ETmsRunner.CreateFmt('Could not parse version "%s" as JSON object from versions-remote command', [Item.Version]);
    var JsonItem := TJSONObject(Pair.JsonValue);

    var LocalReleaseDate: TDateTime;
    if TryISO8601ToDate(JsonItem.GetValue<string>('release_date', ''), LocalReleaseDate, False) then
      Item.ReleaseDate := LocalReleaseDate;
  end;
end;

{ TTmsPinRunner }

class function TTmsPinRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.Pin;
end;

procedure TTmsPinRunner.RunPin(const ProductIds: TArray<string>);
begin
  var Command := 'pin';
  Command := Command + ' ' + String.Join(' ', ProductIds);
  Run(Command);
end;

{ TTmsUnpinRunner }

class function TTmsUnpinRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.Unpin;
end;

procedure TTmsUnpinRunner.RunUnpin(const ProductIds: TArray<string>);
begin
  var Command := 'unpin';
  Command := Command + ' ' + String.Join(' ', ProductIds);
  Run(Command);
end;

{ TTmsLogViewRunner }

class function TTmsLogViewRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.LogView;
end;

function TTmsLogViewRunner.RunLogView(const SessionId: string; const Print: Boolean): string;
begin
  var PrintString := '';
  if Print then PrintString := ' -print';

  var Command := 'log-view ' + SessionId + PrintString;
  Run(Command);
  Result := OutputText.Trim;
end;

{ TTmsInfo }

function TTmsInfo.GetWorkingFolder: string;
begin
  if not Initialized then exit('Error reading working folder');

  Result := FWorkingFolder;
end;


{ TTmsBackupRunner }

class function TTmsBackupRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.Backup;
end;

procedure TTmsBackupRunner.RunBackup(const TargetFile: string;
  const ProductIds: TArray<string>);
begin
  var Command := 'backup';
  Command := Command + ' ' + String.Join(' ', ProductIds);
  Run(Command);
end;

{ TTmsRestoreRunner }

class function TTmsRestoreRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.Restore;
end;

procedure TTmsRestoreRunner.RunRestore(const BackupFile: string; const SkipRegister: boolean; const ProductIds: TArray<string>);
begin
  ProductProgress := true;
  var Command := 'restore "' + BackupFile + '"';
  if SkipRegister then Command := Command + ' -skip-register' else Command := Command + ' -auto-register';
  // an empty list restores everything in the backup.
  for var ProductId in ProductIds do
    Command := Command + ' -include:' + ProductId;

  if ProductProgress then
    Command := Command + ' -display-options:product-progress';

  RunCommand(AddRepo(Command));
end;

{ TTmsSBOMRunner }

class function TTmsSBOMRunner.Command: TTmsCommand;
begin
  Result := TTmsCommand.SBOM;
end;

constructor TTmsSBOMRunner.Create;
begin
  inherited;
  FSBOMInfoList := TObjectList<TSBOMInfo>.Create;
end;

destructor TTmsSBOMRunner.Destroy;
begin
  FSBOMInfoList.Free;
  inherited;
end;

procedure TTmsSBOMRunner.RunSBOM(const Force: boolean; const ProductIds: TArray<string>);
begin
  var Command := 'sbom-generate';
  var ForceStr := '';
  if Force then ForceStr := ' -force';
  
  Command := Command + ' ' + String.Join(' ', ProductIds) + ForceStr + ' -json';
  Run(Command);
  if not (JsonOutput is TJSONObject) then
    raise ETmsRunner.Create('Invalid result for `tms generate-sbom -json` command. Cannot parse "' + OutputText + '" as JSON.');

  var Json := TJSONObject(JsonOutput);
  for var Pair in Json do
  begin
    var SBOMInfo := TSBOMInfo.Create;
    FSBOMInfoList.Add(SBOMInfo);
    SBOMInfo.Id := Pair.JsonString.Value;
    if not (Pair.JsonValue is TJSONObject) then
      raise ETmsRunner.CreateFmt('Could not parse product "%s" as JSON object from generate-sbom command', [SBOMInfo.Id]);
    var JsonProduct := TJSONObject(Pair.JsonValue);

    SBOMInfo.FileName := JsonProduct.GetValue<string>('filename');
    SBOMInfo.Generated := JsonProduct.GetValue<boolean>('generated');
    SBOMInfo.Error := JsonProduct.GetValue<string>('error', '');
  end;

end;

end.

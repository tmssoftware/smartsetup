unit GUI.Environment;

{$SCOPEDENUMS ON}

interface

uses
  System.Generics.Collections, System.SysUtils, System.Classes, System.StrUtils,
  Deget.Version, Deget.CoreTypes, UTmsRunner, UProductInfo, ULogger, UMultiLogger, UCommonTypes;

type
  TProductStatus = (NotInstalled, Available, Installed);
  TLogLevel = (Trace, Info, Error);

  TLogMessageProc = reference to procedure(const Level: TLogLevel; const Message: string);

  TTmsRunner = UTmsRunner.TTmsRunner;
  TTmsCommand = UTmsRunner.TTmsCommand;
  TSBOMInfo = UTmsRunner.TSBOMInfo;

  TGUILogger = class(TLogger)
  strict private
    FOnLogMessage: TLogMessageProc;
    procedure LogMessage(const Msg: string; Level: TLogLevel);
  public
    function ProcessMsg(const s: string): string; override;
    procedure ResetPercentAction; override;
    procedure SetPercentAction(const func: TFunc<integer>); override;
    function IgnoresVerbosity: boolean; override;
    procedure StartSection(const MessageType: TMessageType; const MessageLabel: string); override;
    procedure FinishSection(const MessageType: TMessageType; const IsError: boolean); override;

    procedure Error(const s: string); override;
    procedure Info(const s: string); override;
    procedure Trace(const s: string); override;
    procedure Message(const MessageKind: TLogMessageKind; const Message: string; const NewLine: boolean = true); override;
    property OnLogMessage: TLogMessageProc read FOnLogMessage write FOnLogMessage;
  end;

  TGUIProductInstallation = record
    IDEName: TIDEName;
    Platform: TPlatform;
    IsBuilt: Boolean;
    IsRegistered: Boolean;
  end;

  TGUIProduct = class
  private
    FId: string;
    FLocalVersion: TLenientVersion;
    FRemoteVersion: TLenientVersion;
    FName: string;
    FStatus: TProductStatus;
    FHasFetchInfo: Boolean;
    FVendorId: string;
    FServer: string;
    FIsPinned: Boolean;
    FChannel: string;
    FDescription: string;
    FUrl: string;
    FLicenseStatus: string;
    FInstallations: TArray<TGUIProductInstallation>;
  public
    function IsOutdated: Boolean;
    function DisplayName: string;
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property LocalVersion: TLenientVersion read FLocalVersion write FLocalVersion;
    property RemoteVersion: TLenientVersion read FRemoteVersion write FRemoteVersion;
    property Status: TProductStatus read FStatus write FStatus;
    property HasFetchInfo: Boolean read FHasFetchInfo write FHasFetchInfo;
    property VendorId: string read FVendorId write FVendorId;
    property Server: string read FServer write FServer;
    property IsPinned: Boolean read FIsPinned write FIsPinned;
    property Channel: string read FChannel write FChannel;
    property Description: string read FDescription write FDescription;
    property Url: string read FUrl write FUrl;
    property LicenseStatus: string read FLicenseStatus write FLicenseStatus;
    // IDEs and platforms where the product is built and/or registered
    property Installations: TArray<TGUIProductInstallation> read FInstallations write FInstallations;
  end;

  TGUIProductList = class(TObjectList<TGUIProduct>)
  public
    function Find(const ProductId: string): TGUIProduct;
  end;

  TGUILogItem = class
  private
    FText: string;
    FDateTime: TDateTime;
    FLevel: TLogLevel;
    FIsCanceled: boolean;
    FOutput: string;
    FSessionId: string;
  public
    constructor Create(const AText: string; const ALevel: TLogLevel = TLogLevel.Info; const AOutput: string = ''; const AIsCanceled: boolean = false);
    property Text: string read FText write FText;
    property DateTime: TDateTime read FDateTime;
    property Level: TLogLevel read FLevel write FLevel;
    property IsCanceled: boolean read FIsCanceled write FIsCanceled;
    property Output: string read FOutput write FOutput;
    property SessionId: string read FSessionId write FSessionId;
  end;

  TProductFilter = (All, Installed);

  TProductProgressInfo = record
    Percent: Integer;
    ProductId: string;
    ProductPercent: Integer;
  end;

  TProductsProc = reference to procedure(Products: TGUIProductList);
  TServersProc = reference to procedure(Servers: TServerConfigItems);
  TRequestCredentialsEvent = reference to procedure(var Email, Code: string; var Confirm: Boolean;
    LastWasInvalid: Boolean; var DisableServer: Boolean);
  TGetSelectedProductsProc = reference to procedure(Products: TGUIProductList);
  TCommandOutputProc = reference to procedure(const PartialText: string);
  TProgressProc = reference to procedure(const Percent: Integer);
  TProductProgressProc = reference to procedure(const Info: TProductProgressInfo);
  TLogItemEvent = reference to procedure(const LogItem: TGUILogItem);
  TRunnerProc = reference to procedure(Runner: TTmsRunner);
  TRunEventProc = reference to procedure(const Command: TTmsCommand);

  TGUIEnvironment = class
  private
    FStartWorking: TProc;
    FStopWorking: TProc;

    FFetchedProducts: TGUIProductList;
    FProducts: TGUIProductList;
    FSelected: TGUIProductList;
    FSearchFilter: string;
    FOnProductsUpdated: TProductsProc;
    FActiveRunners: TList<TTmsRunner>; //every runner currently executing, on any thread. Guarded by TMonitor on itself.
    FDestroying: Boolean;
    FInfo: TTmsInfo; //read from workers too, so only read or written under FInfoLock. Callers get a copy.
    FInfoLock: TObject;
    FServer: string;
    FServers: TServerConfigItems;
    FOnRequestCredentials: TRequestCredentialsEvent;
    FOnGetSelectedProducts: TGetSelectedProductsProc;
    FOnLogItemGenerated: TLogItemEvent;
    FLogItems: TObjectList<TGUILogItem>;
    FOnCommandOutput: TCommandOutputProc;
    FProductFilter: TProductFilter;
    FRunningCount: Integer; //changed only under the FActiveRunners lock, see EndRunning.
    FRunningCommand: TTmsCommand; //the command that started the running job. Guarded like FRunningCount.
    FWorkerThreads: Integer; //threads started by StartExclusive or RunBackground that haven't finished yet. The destructor waits for 0.
    FOnRunStart: TRunEventProc;
    FOnRunFinish: TRunEventProc;
    FOnNewVersionDetected: TProc;
    FNewVersionNotified: Boolean;
    FOnRunnerCreated: TRunnerProc;
    FOnServersUpdated: TServersProc;
    procedure ConsolidateGUIProductList(GUIProducts: TGUIProductList; Local, Remote: TProductInfoList);
    function UpdateSelectedProducts: boolean;
    procedure LogMessageReceived(const Level: TLogLevel; const Message: string);
    function GetInfo: TTmsInfo;
    procedure GenerateLogItem(Item: TGUILogItem);
    procedure RunnerOutputEvent(const S: string);
    function TryBeginExclusive(const Command: TTmsCommand): Boolean;
    procedure ExecuteBuild(FullBuild: Boolean; ProgressCallback: TProductProgressProc);
    procedure DoRunStart(const Command: TTmsCommand);
    procedure DoRunFinish(const Command: TTmsCommand);
    procedure DoNotifyNewVersion;
    procedure DoRunnerCreated(Runner: TTmsRunner);
    procedure RefreshFetchedProducts(Filter: TProductFilter; const Server: string);
    procedure RefreshFetchedProductsAfterJob;
    function StartExclusive(const Command: TTmsCommand; const Proc: TProc): Boolean;
    procedure ReplaceFetchedProducts(NewProducts: TGUIProductList; Filter: TProductFilter);
    procedure ApplyProductFilters;
    procedure BeginRunning(const Command: TTmsCommand);
    procedure EndRunning;
    function AddActiveRunner(Runner: TTmsRunner): Boolean;
    procedure RemoveActiveRunner(Runner: TTmsRunner);
    procedure UpdateTMSServerAndRefresh(const Enable: boolean);
  protected
    procedure RunAsync<T: TTmsRunner, constructor>(Proc: TProc<T>);
    procedure RunSync<T: TTmsRunner, constructor>(Proc: TProc<T>);
    procedure RunBackground(const Command: TTmsCommand; Proc: TProc);
  public
    constructor Create;
    destructor Destroy; override;

    function Start(const Alert: TProc<string>): boolean;
    procedure RefreshInfo;
    procedure RefreshServers;

    function IsRunning: Boolean;
    procedure CancelRun;

    function SelectedProductIds: TArray<string>;

    
    // Execute the build all the currently selected products (or all if none is selected)
    procedure ExecuteFullBuild(ProgressCallback: TProductProgressProc);
    procedure ExecutePartialBuild(ProgressCallback: TProductProgressProc);
    procedure ExecuteInstall(ProgressCallback: TProductProgressProc);
    procedure ExecuteUninstall(ProgressCallback: TProgressProc);
    procedure RefreshProductList;

    // Execute install explicitly passing the product ids to be installed
    procedure ExecuteInstallProducts(const ProductIds: TArray<string>; ProgressCallback: TProductProgressProc);

    // Execute self-update command and fires RelaunchCallback if a new update is available and downloaded
    procedure ExecuteSelfUpdate(ProgressCallback: TProgressProc; RelaunchCallback: TProc);

    // Execute pin/unpin operations on selected products
    procedure ExecutePinSelected;
    procedure ExecuteUnpinSelected;

    // Opens doctor in its own console, as it asks questions, and calls WaitForDoctor with its process handle.
    // WaitForDoctor must return only once the process has ended: until then we hold the job, so no other tms starts
    // and fails on the setup lock doctor holds.
    procedure ExecuteDoctor(const WaitForDoctor: TProc<THandle>);
    procedure ExecuteSBOM(const Force: boolean; const ProductIds: TArray<string>; const UpdateSBOMs: TProc<TSBOMInfo>);

    // Updates the credentials
    // Fires the event OnRequestCredentials for an opportunity to offer user an UI to enter credentials
    function ExecuteRequestCredentials(const Alert: TProc<string>): boolean;

    procedure ExecuteConfigure(Silent: Boolean = False);
    function ExecuteLogView(const SessionId: string; const Print: Boolean): string;

    // Change the current applied filter. Will fire the OnProductsUpdated after the product list is modified.
    procedure ChangeProductFilter(Filter: TProductFilter);

    // Retrieves the current applied product filter
    function IsFilterActive(Filter: TProductFilter): Boolean;

    // Functions for enable/disable actions
    function CanInstallSelected: Boolean;
    function GetInstallCaption: string;
    function CanUninstallSelected: Boolean;
    function CanBuild: Boolean;
    function CanApplyFilter: Boolean;
    function CanRequestCredentials: Boolean;
    function CanConfigure: Boolean;
    function CanPinSelected: Boolean;
    function CanUnpinSelected: Boolean;
    function CanRunSBOM: Boolean;
    function CanRunDoctor: Boolean;

    // Functions to read/write configuration parameters
    function ConfigRead(const ParamName: string; var Values: TArray<string>): Boolean;
    function ConfigWrite(const ParamName: string; const Values: TArray<string>): Boolean;

    // Functions to manipulate server config options
    procedure GetServerConfigItems(Items: TServerConfigItems);
    procedure UpdateServerConfigItems(Items: TServerConfigItems);
    procedure RemoveServerConfigItem(const Name: string);
    procedure AddServerConfigItem(Item: TServerConfigItem);
    procedure EnableServerConfigItem(const Name: string; Enabled: Boolean);

    // functions to manipulate version information about products
    procedure GetProductVersions(const ProductId: string; Versions: TVersionInfoList);

    // Additional check to see if a TGUIProduct instance is valid, i.e., was not deleted
    function IsValidProduct(Product: TGUIProduct): Boolean;

    /// <summary>
    ///   Updates the search filter used to filter products. Setting this will refresh the product list.
    /// </summary>
    procedure SetSearchFilter(const Value: string);

    /// <summary>
    ///   Specifies a server from which the products will be retrieved. If empty, all servers will be used.
    ///   False if the product list could not be refreshed because another job is running; the server is then not changed.
    /// </summary>
    function SetServer(const Value: string): Boolean;
    property Server: string read FServer;

    /// <summary>
    ///   Retrieves general information about Smart Setup folder
    /// </summary>
    property Info: TTmsInfo read GetInfo;

    property Servers: TServerConfigItems read FServers;

    property Products: TGUIProductList read FProducts;
    property LogItems: TObjectList<TGUILogItem> read FLogItems;

    property OnProductsUpdated: TProductsProc read FOnProductsUpdated write FOnProductsUpdated;
    property OnRequestCredentials: TRequestCredentialsEvent read FOnRequestCredentials write FOnRequestCredentials;
    property OnServersUpdated: TServersProc read FOnServersUpdated write FOnServersUpdated;

    // Should fill in a list with TGUIProduct objects that represent the current selection.
    // The objects must be the instances previously provided in the OnProductsUpdated
    property OnGetSelectedProducts: TGetSelectedProductsProc read FOnGetSelectedProducts write FOnGetSelectedProducts;

    // Fired once per job, with the command that started it: when the first runner starts and when the last one ends.
    // Can fire on any thread, while we hold a lock, so the handlers must not block (queue to the main thread instead).
    property OnRunStart: TRunEventProc read FOnRunStart write FOnRunStart;
    property OnRunFinish: TRunEventProc read FOnRunFinish write FOnRunFinish;

    property OnLogItemGenerated: TLogItemEvent read FOnLogItemGenerated write FOnLogItemGenerated;
    property OnCommandOutput: TCommandOutputProc read FOnCommandOutput write FOnCommandOutput;

    property OnNewVersionDetected: TProc read FOnNewVersionDetected write FOnNewVersionDetected;

    property OnRunnerCreated: TRunnerProc read FOnRunnerCreated write FOnRunnerCreated;

    property StartWorking: TProc read FStartWorking write FStartWorking;
    property StopWorking: TProc read FStopWorking write FStopWorking;
  end;

implementation

uses
  Winapi.Windows, System.IOUtils, Masks;

{ TGUILogger }

procedure TGUILogger.Error(const s: string);
begin
  LogMessage(S, TLogLevel.Error);
end;

function TGUILogger.IgnoresVerbosity: boolean;
begin
  Result := False;
end;

procedure TGUILogger.Info(const s: string);
begin
  LogMessage(S, TLogLevel.Info);
end;

procedure TGUILogger.LogMessage(const Msg: string; Level: TLogLevel);
begin
  if Assigned(FOnLogMessage) then
    FOnLogMessage(Level, Msg);
end;

procedure TGUILogger.Message(const MessageKind: TLogMessageKind;
  const Message: string; const NewLine: boolean = true);
begin
  LogMessage(Message, TLogLevel.Info);
end;

function TGUILogger.ProcessMsg(const s: string): string;
begin
  Result := s;
end;

procedure TGUILogger.ResetPercentAction;
begin
end;

procedure TGUILogger.SetPercentAction(const func: TFunc<integer>);
begin
end;

procedure TGUILogger.StartSection(const MessageType: TMessageType;
  const MessageLabel: string);
begin
end;

procedure TGUILogger.FinishSection(const MessageType: TMessageType;
  const IsError: boolean);
begin
end;


procedure TGUILogger.Trace(const s: string);
begin
  LogMessage(S, TLogLevel.Trace);
end;

function GetSessionId(const s: string): string;
const
  Id = '] Session Id: ';
begin
  var idx := s.IndexOf(Id);
  if idx < 0 then exit('');

  var eol := s.IndexOf(#$0A, idx + Id.Length);
  if eol < 0 then eol := s.Length;
  exit (s.Substring(idx + Id.Length, eol - (idx + Id.Length)).Trim);
end;

{ TGUILogItem }

constructor TGUILogItem.Create(const AText: string; const ALevel: TLogLevel; const AOutput: string; const AIsCanceled: boolean);
begin
  inherited Create;
  FText := AText;
  FLevel := ALevel;
  FOutput := AOutput;
  FIsCanceled := AIsCanceled;
  FSessionId := GetSessionId(AOutput);
  FDateTime := now;
end;

{ TGUIEnvironment }

procedure TGUIEnvironment.AddServerConfigItem(Item: TServerConfigItem);
begin
  RunSync<TTmsServerAddRunner>(
    procedure(Runner: TTmsServerAddRunner)
    begin
      Runner.RunServerAdd(Item);
    end);
end;

procedure TGUIEnvironment.BeginRunning(const Command: TTmsCommand);
begin
  TMonitor.Enter(FActiveRunners);
  try
    Inc(FRunningCount);
    if FRunningCount = 1 then
    begin
      FRunningCommand := Command;
      DoRunStart(Command);
    end;
  finally
    TMonitor.Exit(FActiveRunners);
  end;
end;

function TGUIEnvironment.CanApplyFilter: Boolean;
begin
  if IsRunning then Exit(False);
  Result := True;
end;

function TGUIEnvironment.CanBuild: Boolean;
begin
  if IsRunning then Exit(False);

  if not UpdateSelectedProducts then Exit(False);
  Result := False;
  for var Product in FSelected do
    if Product.Status in [TProductStatus.Installed, TProductStatus.Available] then
      Exit(True);
end;

procedure TGUIEnvironment.CancelRun;
begin
  TMonitor.Enter(FActiveRunners);
  try
    // Only the runners active now. A later step that needs a canceled one's result stops on ETmsRunnerCanceled,
    // and the refresh after a job is meant to run anyway, to show what a canceled install did finish.
    for var Runner in FActiveRunners do
      Runner.Cancel;
  finally
    TMonitor.Exit(FActiveRunners);
  end;
end;

function TGUIEnvironment.AddActiveRunner(Runner: TTmsRunner): Boolean;
begin
  TMonitor.Enter(FActiveRunners);
  try
    // Checked under the same lock CancelRun takes, so a runner is either canceled or never starts.
    Result := not FDestroying;
    if Result then FActiveRunners.Add(Runner);
  finally
    TMonitor.Exit(FActiveRunners);
  end;
end;

procedure TGUIEnvironment.RemoveActiveRunner(Runner: TTmsRunner);
begin
  TMonitor.Enter(FActiveRunners);
  try
    FActiveRunners.Remove(Runner);
  finally
    TMonitor.Exit(FActiveRunners);
  end;
end;

function TGUIEnvironment.CanConfigure: Boolean;
begin
  if IsRunning then Exit(False);
  Result := True;
end;

function TGUIEnvironment.CanInstallSelected: Boolean;
begin
  if IsRunning then Exit(False);
  if not UpdateSelectedProducts then Exit(False);

  // According with the desired logic below, the Install button will only be disabled for
  // products that are already installed and don't have a new version available to download
  // Since "installing" a product that is in the latest version is harmless, let's simplify everything
  // and just leave the Install button always enabled.
  Result := True;


//  UpdateSelectedProducts;
//  Result := False;
//  for var Product in FSelected do
//  begin
//    if (Product.Status = TProductStatus.NotInstalled)  then
//      Result := True
//    else
//    if (Product.Status = TProductStatus.Installed) and Product.IsOutdated then
//      Result := True
//    else
//    if (Product.Status = TProductStatus.Available) then
//    else
//      Exit(False);
//  end;
end;


function TGUIEnvironment.GetInstallCaption: string;
type
  TInstallCaptionType = (Unknown, Install, Update, Reinstall);

  function GetOneProductInstallCaption(const Product: TGUIProduct): TInstallCaptionType;
  begin
    if Product.Status = TProductStatus.Installed then
    begin
      if Product.IsOutdated or (Product.RemoteVersion.IsNull)
        then Exit(TInstallCaptionType.Update)
        else Exit(TInstallCaptionType.Reinstall);
    end;
    Result := TInstallCaptionType.Install;
  end;


begin
  UpdateSelectedProducts;
  Result := '&Install';
  var AllCaptionType := TInstallCaptionType.Unknown;
  for var Product in FSelected do
  begin
    var ProdCaptionType := GetOneProductInstallCaption(Product);
    if AllCaptionType = TInstallCaptionType.Unknown then AllCaptionType := ProdCaptionType
    else if AllCaptionType <> ProdCaptionType then exit;
  end;

  case AllCaptionType of
    TInstallCaptionType.Update: exit ('U&pdate');
    TInstallCaptionType.Reinstall: exit('Re&install');
  end;
end;


function TGUIEnvironment.CanPinSelected: Boolean;
begin
  if IsRunning then Exit(False);
  if not UpdateSelectedProducts then Exit(false);
  Result := False;
  for var Product in FSelected do
    if not Product.IsPinned then
      Exit(True);
end;

function TGUIEnvironment.CanRunDoctor: Boolean;
begin
  Result := not IsRunning;
end;

function TGUIEnvironment.CanRunSBOM: Boolean;
begin
  if IsRunning then Exit(False);
  if not UpdateSelectedProducts then Exit(false);
  Result := False;
  for var Product in FSelected do
  begin
    if (Product.Status <> TProductStatus.NotInstalled)
     then Exit(True);
  end;
end;

function TGUIEnvironment.CanRequestCredentials: Boolean;
begin
  if IsRunning then Exit(False);
  Result := True;
end;

function TGUIEnvironment.CanUninstallSelected: Boolean;
begin
  if IsRunning then Exit(False);

  if not UpdateSelectedProducts then Exit(false);
  Result := False;
  for var Product in FSelected do
    if Product.Status = TProductStatus.Installed then
      Result := True
    else
    if Product.Status = TProductStatus.Available then
      Result := True
    else
      Exit(False);
end;

function TGUIEnvironment.CanUnpinSelected: Boolean;
begin
  if IsRunning then Exit(False);
  if not UpdateSelectedProducts then Exit(false);
  Result := False;
  for var Product in FSelected do
    if Product.IsPinned then
      Exit(True);
end;

function TGUIEnvironment.ConfigRead(const ParamName: string; var Values: TArray<string>): Boolean;
begin
  var Success := False;
  var Output: TArray<string>;
  RunSync<TTmsConfigReadRunner>(
    procedure(Runner: TTmsConfigReadRunner)
    begin
      var Value := Runner.RunConfigRead(ParamName).Trim;
      if Value.StartsWith('[') and Value.EndsWith(']') then
        Output := SplitString(Copy(Value, 2, Value.Length - 2), ',')
      else
        Output := [Value];
      Success := True;
    end);
  Result := Success;
  if Result then
    Values := Output;
end;

function TGUIEnvironment.ConfigWrite(const ParamName: string; const Values: TArray<string>): Boolean;
begin
  var Success := False;
  RunSync<TTmsConfigWriteRunner>(
    procedure(Runner: TTmsConfigWriteRunner)
    begin
      var ParamValue := '[' + string.Join(',', Values) + ']';
      Runner.RunConfigWrite(ParamName, ParamValue);
      Success := True;
    end);
  Result := Success;
end;

procedure TGUIEnvironment.ConsolidateGUIProductList(GUIProducts: TGUIProductList; Local, Remote: TProductInfoList);
begin
  GUIProducts.Clear;
  for var Product in Local do
  begin
    var GUIProduct := TGUIProduct.Create;
    GUIProducts.Add(GUIProduct);

    GUIProduct.Id := Product.Id;
    GUIProduct.Name := Product.Name;
    GUIProduct.LocalVersion := Product.Version;
    if Product.HasIDEInfo then
      GUIProduct.Status := TProductStatus.Installed
    else
      GUIProduct.Status := TProductStatus.Available;
    GUIProduct.HasFetchInfo := not Product.Local;
    GUIProduct.Server := Product.Server;
    GUIProduct.IsPinned := Product.Pinned;
    GUIProduct.Channel := Product.Channel;

    var Installations: TArray<TGUIProductInstallation> := [];
    for var IDEPair in Product.IDEs do
    begin
      // Skip IDEs or platforms unknown to this version of the GUI
      if (Ord(IDEPair.Key) < Ord(Low(TIDEName))) or (Ord(IDEPair.Key) > Ord(High(TIDEName))) then
        Continue;
      for var PlatPair in IDEPair.Value.Platforms do
      begin
        if (Ord(PlatPair.Key) < Ord(Low(TPlatform))) or (Ord(PlatPair.Key) > Ord(High(TPlatform))) then
          Continue;
        var Installation: TGUIProductInstallation;
        Installation.IDEName := IDEPair.Key;
        Installation.Platform := PlatPair.Key;
        Installation.IsBuilt := PlatPair.Value.IsBuilt;
        Installation.IsRegistered := PlatPair.Value.IsRegistered;
        Installations := Installations + [Installation];
      end;
    end;
    GUIProduct.Installations := Installations;
  end;

  for var Product in Remote do
  begin
    var GUIProduct := GUIProducts.Find(Product.Id);
    if GUIProduct = nil then
    begin
      GUIProduct := TGUIProduct.Create;
      GUIProducts.Add(GUIProduct);
      GUIProduct.Id := Product.Id;
      GUIProduct.Name := Product.Name;
      GUIProduct.Status := TProductStatus.NotInstalled;
    end;

    if GUIProduct.HasFetchInfo then // only update if it's same origin. For now, only one origin is available
      GUIProduct.RemoteVersion := Product.Version;

    // If there is a remote product value in server, override it here
    if Product.Server <> '' then
      GUIProduct.Server := Product.Server;

    // Update VendorId. For now, only remote products have vendor id, so we are setting here regardless.
    GUIProduct.VendorId := Product.VendorId;

    // Same for description, url and license status: only remote products have them.
    GUIProduct.Description := Product.Description;
    GUIProduct.Url := Product.Url;
    GUIProduct.LicenseStatus := Product.LicenseStatus;
  end;
end;

constructor TGUIEnvironment.Create;
begin
  inherited Create;
  FFetchedProducts := TGUIProductList.Create;
  FProducts := TGUIProductList.Create(False);
  FSelected := TGUIProductList.Create(False);
  FLogItems := TObjectList<TGUILogItem>.Create;
  FServers := TServerConfigItems.Create;
  FActiveRunners := TList<TTmsRunner>.Create;
  FInfoLock := TObject.Create;

  // Init logging
  var GUILogger := TGUILogger.Create;
  Logger := TMultiLogger.Create([GUILogger]);
  GUILogger.OnLogMessage := LogMessageReceived;
end;

procedure TGUIEnvironment.RemoveServerConfigItem(const Name: string);
begin
  RunSync<TTmsServerRemoveRunner>(
    procedure(Runner: TTmsServerRemoveRunner)
    begin
      Runner.RunServerRemove(Name);
    end);
end;

destructor TGUIEnvironment.Destroy;
begin
  // Cancel the runners and wait for every worker thread to finish. Canceling already killed the tms.exe
  // processes, so what is left is our own code, and freeing this object under it would crash.
  // Not IsRunning: a job running on the main thread would never finish while we wait here.
  FDestroying := True;
  CancelRun;
  var Deadline := GetTickCount64 + 10000;
  while FWorkerThreads > 0 do
  begin
    // A worker still running after 10 seconds is stuck. We are closing, so end the process the way the
    // task manager would; Windows cleans up, and closing our job handles kills any tms.exe left.
    // Not ExitProcess: it runs the DLL shutdown code, which can block on the stuck thread.
    if GetTickCount64 > Deadline then
      TerminateProcess(GetCurrentProcess, 1);
    // Not Sleep: a worker waiting in TThread.Synchronize would never finish.
    CheckSynchronize(100);
  end;
  // Run what the workers queued, so nothing queued refers to this object after it is freed.
  while CheckSynchronize do ;

  FProducts.Free;
  FFetchedProducts.Free;
  FSelected.Free;
  FLogItems.Free;
  FInfoLock.Free;
  FServers.Free;
  FActiveRunners.Free;
  Logger.Free;
  inherited;
end;

procedure TGUIEnvironment.DoNotifyNewVersion;
begin
  if not FNewVersionNotified then
  begin
    FNewVersionNotified := True;
    if Assigned(OnNewVersionDetected) then
      FOnNewVersionDetected();
  end;
end;

procedure TGUIEnvironment.DoRunFinish(const Command: TTmsCommand);
begin
  if Assigned(FOnRunFinish) then
    FOnRunFinish(Command);
end;

procedure TGUIEnvironment.DoRunnerCreated(Runner: TTmsRunner);
begin
  if Assigned(FOnRunnerCreated) then
    FOnRunnerCreated(Runner);
end;

procedure TGUIEnvironment.DoRunStart(const Command: TTmsCommand);
begin
  if Assigned(FOnRunStart) then
    FOnRunStart(Command);
end;

function TGUIEnvironment.StartExclusive(const Command: TTmsCommand; const Proc: TProc): Boolean;
begin
  // Claimed here, on the calling thread, so a second click can't start a second job before the thread runs.
  Result := TryBeginExclusive(Command);
  if not Result then
  begin
    Logger.Error('tms.exe is already running');
    Exit;
  end;
  AtomicIncrement(FWorkerThreads);
  try
    TThread.CreateAnonymousThread(
      procedure
      begin
        try
          try
            Proc;
          finally
            EndRunning;
          end;
        finally
          // Last: once it reaches 0 the destructor can free this object.
          AtomicDecrement(FWorkerThreads);
        end;
      end)
      .Start;
  except
    AtomicDecrement(FWorkerThreads);
    EndRunning;
    raise;
  end;
end;

procedure TGUIEnvironment.RunAsync<T>(Proc: TProc<T>);
begin
  StartExclusive(T.Command,
    procedure
    begin
      RunSync<T>(Proc);
    end);
end;

procedure TGUIEnvironment.RunBackground(const Command: TTmsCommand; Proc: TProc);
begin
  // Counted from the start, not only while Proc runs: the waiting loop reads FDestroying too.
  AtomicIncrement(FWorkerThreads);
  try
    TThread.CreateAnonymousThread(
      procedure
      begin
        try
          while not FDestroying do
          begin
            if TryBeginExclusive(Command) then
            begin
              try
                Proc;
              finally
                EndRunning;
              end;
              Exit;
            end;
            Sleep(1000); // try a new check after a while
          end;
        finally
          // Last: once it reaches 0 the destructor can free this object.
          AtomicDecrement(FWorkerThreads);
        end;
      end).Start;
  except
    AtomicDecrement(FWorkerThreads);
    raise;
  end;
end;

procedure TGUIEnvironment.ExecuteDoctor(const WaitForDoctor: TProc<THandle>);
begin
  if not TryBeginExclusive(TTmsCommand.Doctor) then
  begin
    Logger.Error('tms.exe is already running');
    Exit;
  end;
  try
    var ExeFileName := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'tms.exe');
    // No cmd.exe: it would expand %vars% and strip quotes in the path. tms.exe gets its own console, and -pause keeps it open.
    var CmdLine := '"' + ExeFileName + '" doctor -pause -fully-interactive';
    UniqueString(CmdLine); // CreateProcessW may write to the buffer
    var SI: TStartupInfo;
    var PI: TProcessInformation;
    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    // In the folder the runners use, not the exe's.
    if not CreateProcess(PChar(ExeFileName), PChar(CmdLine), nil, nil, False,
      CREATE_NEW_CONSOLE, nil, PChar(GetCurrentDir), SI, PI) then
      RaiseLastOSError;
    CloseHandle(PI.hThread);
    try
      WaitForDoctor(PI.hProcess);
    finally
      CloseHandle(PI.hProcess);
    end;
  finally
    EndRunning;
  end;
end;

procedure TGUIEnvironment.ExecuteSBOM(const Force: boolean; const ProductIds: TArray<string>; const UpdateSBOMs: TProc<TSBOMInfo>);
begin
  RunSync<TTmsSBOMRunner>(
    procedure(Runner: TTmsSBOMRunner)
    begin
      Runner.OnOutputLine := RunnerOutputEvent;
      Runner.RunSBOM(Force, ProductIds);
      for var InfoItem in Runner.SBOMInfoList do
      begin
        UpdateSBOMs(InfoItem);
      end;
    end);
end;

procedure TGUIEnvironment.ExecuteSelfUpdate(ProgressCallback: TProgressProc; RelaunchCallback: TProc);
begin
  RunBackground(TTmsCommand.SelfUpdate,
    procedure
    begin
      RunSync<TTmsSelfUpdateRunner>(
        procedure(Runner: TTmsSelfUpdateRunner)
        begin
          if Runner.RunSelfUpdate then
            if Assigned(RelaunchCallback) then
              RelaunchCallback();
        end);
    end
  );
end;

procedure TGUIEnvironment.EnableServerConfigItem(const Name: string;
  Enabled: Boolean);
begin
  RunSync<TTmsServerEnableRunner>(
    procedure(Runner: TTmsServerEnableRunner)
    begin
      Runner.RunServerEnable(Name, Enabled);
    end);
end;

procedure TGUIEnvironment.EndRunning;
begin
  // All under the lock, and the count drops last: until it does no new job can start, so its OnRunStart
  // can't arrive before our OnRunFinish.
  TMonitor.Enter(FActiveRunners);
  try
    if FRunningCount = 1 then
      DoRunFinish(FRunningCommand);
    Dec(FRunningCount);
  finally
    TMonitor.Exit(FActiveRunners);
  end;
end;

procedure TGUIEnvironment.ExecuteBuild(FullBuild: Boolean; ProgressCallback: TProductProgressProc);
begin
  // Read the selection here, on the main thread. Inside the worker it read the list view from a
  // background thread, and got the selection as it was then, not when the user clicked.
  var ProductIds := SelectedProductIds;
  RunAsync<TTmsBuildRunner>(
    procedure(Runner: TTmsBuildRunner)
    begin
      try
        if Assigned(ProgressCallback) then
        begin
          var ProgressInfo := Default(TProductProgressInfo);
          ProgressCallback(ProgressInfo);
        end;

        Runner.OnOutputLine := RunnerOutputEvent;
        Runner.FullBuild := FullBuild;
        Runner.ProductIds.AddStrings(ProductIds);
        Runner.OnProgress :=
          procedure(const Info: TProgressInfo)
          begin
            if Assigned(ProgressCallback) then
            begin
              var ProgressInfo := Default(TProductProgressInfo);
              ProgressInfo.Percent := Info.Percent;
              ProgressInfo.ProductId := Info.ProductId;
              ProgressInfo.ProductPercent := Info.ProductPercent;
              ProgressCallback(ProgressInfo);
            end;
          end;
        Runner.RunBuild;

        // Todo: Handle exit code 3 (which is partial succesfully build)
        if Assigned(ProgressCallback) then
        begin
          var ProgressInfo := Default(TProductProgressInfo);
          if Runner.IsCanceled then
            ProgressInfo.Percent := 0
          else
            ProgressInfo.Percent := 100;
          ProgressCallback(ProgressInfo);
        end;
      finally
        RefreshFetchedProductsAfterJob;
      end;
    end);
end;

procedure TGUIEnvironment.ExecuteConfigure(Silent: Boolean = False);
begin
  RunSync<TTmsConfigureRunner>(
    procedure(Runner: TTmsConfigureRunner)
    begin
      Runner.RunConfigure(Silent);
      RefreshInfo;
    end);
end;

function TGUIEnvironment.ExecuteLogView(const SessionId: string; const Print: Boolean): string;
var
  _Result: string;
begin
  RunSync<TTmsLogViewRunner>(
    procedure(Runner: TTmsLogViewRunner)
    begin
      _Result := Runner.RunLogView(SessionId, Print);
      RefreshInfo;
    end);
    Result := _Result;
end;

procedure TGUIEnvironment.ExecuteFullBuild(ProgressCallback: TProductProgressProc);
begin
  ExecuteBuild(True, ProgressCallback);
end;

procedure TGUIEnvironment.ExecuteInstall(ProgressCallback: TProductProgressProc);
begin
  ExecuteInstallProducts(SelectedProductIds, ProgressCallback);
end;

procedure TGUIEnvironment.ExecuteInstallProducts(const ProductIds: TArray<string>;
  ProgressCallback: TProductProgressProc);
begin
  RunAsync<TTmsInstallRunner>(
    procedure(Runner: TTmsInstallRunner)
    begin
      try
        if Assigned(ProgressCallback) then
        begin
          var ProgressInfo := Default(TProductProgressInfo);
          ProgressCallback(ProgressInfo);
        end;

        Runner.OnOutputLine := RunnerOutputEvent;
        Runner.ProductIds.AddStrings(ProductIds);
        Runner.OnProgress :=
          procedure(const Info: TProgressInfo)
          begin
            if Assigned(ProgressCallback) then
            begin
              var ProgressInfo := Default(TProductProgressInfo);
              ProgressInfo.Percent := Info.Percent;
              ProgressInfo.ProductId := Info.ProductId;
              ProgressInfo.ProductPercent := Info.ProductPercent;
              ProgressCallback(ProgressInfo);
            end;
          end;
        Runner.RunInstall;

        // Todo: Handle exit code 3 (which is partial succesfully build)
        if Assigned(ProgressCallback) then
        begin
          var ProgressInfo := Default(TProductProgressInfo);
          if Runner.IsCanceled then
            ProgressInfo.Percent := 0
          else
            ProgressInfo.Percent := 100;
          ProgressCallback(ProgressInfo);
        end;
      finally
        RefreshFetchedProductsAfterJob;
      end;
    end);
end;

procedure TGUIEnvironment.ExecutePartialBuild(ProgressCallback: TProductProgressProc);
begin
  ExecuteBuild(False, ProgressCallback);
end;

procedure TGUIEnvironment.ExecutePinSelected;
begin
  RunSync<TTmsPinRunner>(
    procedure(Runner: TTmsPinRunner)
    begin
      Runner.RunPin(SelectedProductIds)
    end);
  RefreshFetchedProductsAfterJob;
end;

procedure TGUIEnvironment.ExecuteUninstall(ProgressCallback: TProgressProc);
begin
  var ProductIds := SelectedProductIds; // on the main thread, see ExecuteBuild
  RunAsync<TTmsUninstallRunner>(
    procedure(Runner: TTmsUninstallRunner)
    begin
      try
        if Assigned(ProgressCallback) then
          ProgressCallback(0);

        Runner.OnOutputLine := RunnerOutputEvent;
        Runner.ProductIds.AddStrings(ProductIds);
        Runner.OnProgress :=
          procedure(const Info: TProgressInfo)
          begin
            if Assigned(ProgressCallback) then
              ProgressCallback(Info.Percent);
          end;
        Runner.RunUninstall;

        // Todo: Handle exit code 3 (which is partial succesfully build)
        if Assigned(ProgressCallback) then
        begin
          if Runner.IsCanceled then
            ProgressCallback(0)
          else
            ProgressCallback(100);
        end;
      finally
        RefreshFetchedProductsAfterJob;
      end;
    end);
end;

procedure TGUIEnvironment.ExecuteUnpinSelected;
begin
  RunSync<TTmsUnpinRunner>(
    procedure(Runner: TTmsUnpinRunner)
    begin
      Runner.RunUnpin(SelectedProductIds)
    end);
  RefreshFetchedProductsAfterJob;
end;

procedure TGUIEnvironment.RunnerOutputEvent(const S: string);
begin
  if Assigned(FOnCommandOutput) then
    FOnCommandOutput(S);
end;

procedure TGUIEnvironment.RunSync<T>(Proc: TProc<T>);
const
  NewLine = #13#10;
begin
  var LocalRunner := T.Create;
  try
    DoRunnerCreated(LocalRunner);
    try
      // A list, not a single "current runner" field: runners nest and run on several threads, and
      // saving/restoring one field across threads could leave it pointing at a freed runner.
      // Not added when we are closing: then the remaining steps don't start a new tms.exe.
      if not AddActiveRunner(LocalRunner) then Exit;
      try
        BeginRunning(T.Command);
        try
          if Assigned(StartWorking) then StartWorking;
          try
            Proc(LocalRunner);
          finally
            if Assigned(StopWorking) then StopWorking;
          end;
        finally
          EndRunning;
        end;
      finally
        RemoveActiveRunner(LocalRunner);
      end;

      if LocalRunner.NewVersionDetected then
        DoNotifyNewVersion;

      if (LocalRunner is TAbstractTmsBuildRunner) or (LocalRunner.Command in [TTmsCommand.SBom]) then
      begin
        // not all commands log to the output. But they all do if there is an exception.
        GenerateLogItem(TGUILogItem.Create(
            Format('%s: tms %s', [TmsCommandName(T.Command), LocalRunner.Params]),
            TLogLevel.Info,
            LocalRunner.CmdAndOutput,
            LocalRunner.IsCanceled
        ));
      end;

    except
      on E: ETmsRunnerCanceled do
      begin
        // The user asked for this, so it is logged as canceled, not as an error.
        GenerateLogItem(TGUILogItem.Create(
          TmsCommandName(T.Command) + ': ' + E.Message.Replace(#10, ' ').Replace(#13, ''),
          TLogLevel.Info,
          LocalRunner.CmdAndOutput  + NewLine + E.Message,
          True
        ));
      end;
      on E: Exception do
      begin
        GenerateLogItem(TGUILogItem.Create(
          TmsCommandName(T.Command) + ': ' + E.Message.Replace(#10, ' ').Replace(#13, ''),
          TLogLevel.Error,
          LocalRunner.CmdAndOutput + NewLine + E.Message,
          LocalRunner.IsCanceled
        ));
      end;
    end;
  finally
    LocalRunner.Free;
  end;
end;

function TGUIEnvironment.SelectedProductIds: TArray<string>;
begin
  UpdateSelectedProducts;
  Result := [];
  for var Product in FSelected do
    Result := Result + [Product.Id];
end;

function TGUIEnvironment.Start(const Alert: TProc<string>): boolean;
begin
  // If an exception happens, we keep going. We only return false if the user clicked cancel.
  RefreshServers;

  if FServers.IsEnabled('tms') and not Info.HasCredentials then
    if not ExecuteRequestCredentials(Alert) then exit(false);

  var Server := FServer;
  RunBackground(TTmsCommand.List, procedure
    begin
      if FServers.RemotesEnabled then
        RefreshFetchedProducts(TProductFilter.All, Server)
      else
        RefreshFetchedProducts(TProductFilter.Installed, Server);
    end);

  Result := true;
end;

procedure TGUIEnvironment.GenerateLogItem(Item: TGUILogItem);
begin
  TThread.Queue(nil,
    procedure
    begin
      if FDestroying then
      begin
        Item.Free;
        Exit;
      end;
      FLogItems.Add(Item);
      if Assigned(FOnLogItemGenerated) then
        FOnLogItemGenerated(Item);
    end);
end;

function TGUIEnvironment.GetInfo: TTmsInfo;
begin
  // Workers call this too (RefreshFetchedProducts) while the main thread may call RefreshInfo, so under a lock.
  // Held while "tms info" runs, so a second caller waits for that result instead of starting its own.
  TMonitor.Enter(FInfoLock);
  try
    // Not initialized: never loaded, refreshed, or the last "tms info" failed. Then (re)load it.
    if not FInfo.Initialized then
    begin
      var NewInfo := Default(TTmsInfo);
      RunSync<TTmsInfoRunner>(
        procedure(Runner: TTmsInfoRunner)
        begin
          Runner.RunInfo(NewInfo);
        end);
      FInfo := NewInfo;
    end;
    Result := FInfo; // a copy: the caller can use it after we release the lock
  finally
    TMonitor.Exit(FInfoLock);
  end;
end;

procedure TGUIEnvironment.GetProductVersions(const ProductId: string; Versions: TVersionInfoList);
begin
  RunSync<TTmsVersionsRemoteRunner>(
    procedure(Runner: TTmsVersionsRemoteRunner)
    begin
      Runner.RunVersionsRemote(ProductId, Versions);
    end);
end;

procedure TGUIEnvironment.GetServerConfigItems(Items: TServerConfigItems);
begin
  RunSync<TTmsServerListRunner>(
    procedure(Runner: TTmsServerListRunner)
    begin
      Runner.RunServerList(Items);
    end);
end;

function TGUIEnvironment.IsRunning: Boolean;
begin
  Result := FRunningCount > 0;
end;

function TGUIEnvironment.IsValidProduct(Product: TGUIProduct): Boolean;
begin
  Result := FFetchedProducts.IndexOf(Product) >= 0;
end;

function TGUIEnvironment.IsFilterActive(Filter: TProductFilter): Boolean;
begin
  Result := FProductFilter = Filter;
end;

procedure TGUIEnvironment.LogMessageReceived(const Level: TLogLevel; const Message: string);
begin
  GenerateLogItem(TGUILogItem.Create(Message, Level));
end;

procedure TGUIEnvironment.RefreshInfo;
begin
  // The next GetInfo loads it again.
  TMonitor.Enter(FInfoLock);
  try
    FInfo.Initialized := False;
  finally
    TMonitor.Exit(FInfoLock);
  end;
end;

procedure TGUIEnvironment.RefreshProductList;
begin
  StartExclusive(TTmsCommand.List,
    procedure
    begin
      RefreshFetchedProducts(FProductFilter, FServer);
    end);
end;

procedure TGUIEnvironment.RefreshFetchedProductsAfterJob;
begin
  // Runs even if the job was canceled: a canceled install may have finished some products.
  // The refresh itself can still be canceled, and it does not run when we are closing.
  RefreshFetchedProducts(FProductFilter, FServer);
end;

procedure TGUIEnvironment.RefreshFetchedProducts(Filter: TProductFilter; const Server: string);
begin
  RunSync<TTmsListRunner>(
    procedure(ListRunner: TTmsListRunner)
    begin
      // No check for a valid folder: the form only lets us work in one (TMainForm.EnsureValidFolder).
      ListRunner.RunList;
      RunSync<TTmsListRemoteRunner>(
        procedure(RemoteRunner: TTmsListRemoteRunner)
        begin
          RemoteRunner.Server := Server;
          RemoteRunner.RunListRemote;

          // Build a new list here; FFetchedProducts itself is only replaced on the main thread.
          // Clearing it from this thread freed the TGUIProduct objects the list view still pointed at.
          var NewProducts := TGUIProductList.Create;
          try
            ConsolidateGUIProductList(NewProducts, ListRunner.Products, RemoteRunner.Products);

            // Filter products
            var Predicate :=
              function(Product: TGUIProduct): Boolean
              begin
                if (Filter = TProductFilter.Installed) and (Product.Status <> TProductStatus.Installed) then
                  Exit(False);

                if (Server <> '') and not SameText(Server, Product.Server) then
                  Exit(False);

                Result := True;
              end;
            for var I := NewProducts.Count - 1 downto 0 do
              if not Predicate(NewProducts[I]) then
                NewProducts.Delete(I);
          except
            NewProducts.Free;
            raise;
          end;

          // fire event to refresh producs. Synchronize runs it directly when we already are in the main thread.
          TThread.Synchronize(nil,
            procedure
            begin
              ReplaceFetchedProducts(NewProducts, Filter);
            end);
        end)
    end)
end;

procedure TGUIEnvironment.ReplaceFetchedProducts(NewProducts: TGUIProductList; Filter: TProductFilter);
begin
  if FDestroying then
  begin
    NewProducts.Free;
    Exit;
  end;

  var OldProducts := FFetchedProducts;
  FFetchedProducts := NewProducts;
  FProductFilter := Filter;
  try
    ApplyProductFilters; // the list view now points at the new objects...
  finally
    OldProducts.Free;   // ...so the old ones can go.
  end;
end;

procedure TGUIEnvironment.RefreshServers;
begin
  if not Assigned(FOnServersUpdated) then Exit;

  GetServerConfigItems(FServers);
  FOnServersUpdated(FServers);
end;

procedure TGUIEnvironment.UpdateTMSServerAndRefresh(const Enable: boolean);
begin
  EnableServerConfigItem('tms', Enable);
  RefreshServers;
  RefreshFetchedProductsAfterJob;
end;

function TGUIEnvironment.ExecuteRequestCredentials(const Alert: TProc<string>): boolean;
begin
  if not Assigned(FOnRequestCredentials) then
    Exit(true);

  var Confirm := true;
  var Disable := false;
  RunSync<TTmsCredentialsRunner>(
    procedure(Runner: TTmsCredentialsRunner)
    begin
      var Email: string;
      var Code: string;
      Runner.RunGetCredentials(Email, Code);

      var LastValid := True;
      var Done := False;
      repeat
        try
          Confirm := False;
          Disable := False;
          FOnRequestCredentials(Email, Code, Confirm, not LastValid, Disable);
          if Confirm then
          begin
            LastValid := false;
            if (Email.Trim <> '') and (Code.Trim <> '') and Runner.RunUpdateCredentials(Email, Code) then LastValid := true;
            RefreshInfo;
            Done := LastValid;
            if Done then UpdateTMSServerAndRefresh(true);

          end
          else
          if Disable then
          begin
            UpdateTMSServerAndRefresh(false);
            Done := True;
          end
          else Done := True;
        except
          on ex: ETmsRunnerCanceled do raise; //closing or canceled: don't show the dialog again.
          on ex: Exception do
          begin
            if Runner.IsCanceled then raise;
            Alert(ex.Message); //and show the dialog again.
          end;
        end;
      until Done;
    end);

    Result := Confirm or Disable;
end;

procedure TGUIEnvironment.ChangeProductFilter(Filter: TProductFilter);
begin
  var Server := FServer;
  StartExclusive(TTmsCommand.List,
    procedure
    begin
      RefreshFetchedProducts(Filter, Server);
    end);
end;

function TGUIEnvironment.TryBeginExclusive(const Command: TTmsCommand): Boolean;
begin
  // Check and claim in one step, under the lock. The previous CheckRunning only logged an error and
  // returned, so its callers went on and started a second tms.exe anyway.
  TMonitor.Enter(FActiveRunners);
  try
    Result := FRunningCount = 0;
    if Result then
    begin
      FRunningCount := 1;
      FRunningCommand := Command;
      DoRunStart(Command);
    end;
  finally
    TMonitor.Exit(FActiveRunners);
  end;
end;

procedure TGUIEnvironment.ApplyProductFilters;
begin
  var Filter := FSearchFilter.ToLower;
  // The search text is free text typed by the user, so it may not be a valid mask (e.g. "[").
  // TMask.Create raises EMaskException for those; then we just search without the mask.
  var Mask: TMask := nil;
  try
    Mask := TMask.Create(Filter);
  except
    on EMaskException do ; // Mask stays nil
  end;
  try
    var Predicate :=
      function(Product: TGUIProduct): Boolean
      begin
        Result := True;
        if Filter.Trim <> '' then
        begin
          var IdLower := Product.Id.ToLower;
          var NameLower := Product.Name.ToLower;
          Result := IdLower.Contains(Filter) or NameLower.Contains(Filter) or ((Mask <> nil) and Mask.Matches(IdLower));
        end;
      end;

    FProducts.Clear;
    for var Product in FFetchedProducts do
      if Predicate(Product) then
        FProducts.Add(Product);
    if Assigned(FOnProductsUpdated) then
      FOnProductsUpdated(FProducts);
  finally
    Mask.Free;
  end;
end;

procedure TGUIEnvironment.SetSearchFilter(const Value: string);
begin
  if FSearchFilter <> Value then
  begin
    FSearchFilter := Value;
    ApplyProductFilters;
  end;
end;

function TGUIEnvironment.SetServer(const Value: string): Boolean;
begin
  if FServer = Value then Exit(True);

  // Asynchronous and guarded like any other job: it used to run list and list-remote on the main
  // thread, freezing the window, and could start while an install was running.
  // Only keep the new server if the refresh started, or it would not match the products shown.
  var Filter := FProductFilter;
  var Server := Value;
  Result := StartExclusive(TTmsCommand.List,
    procedure
    begin
      RefreshFetchedProducts(Filter, Server);
    end);
  if Result then FServer := Value;
end;

function TGUIEnvironment.UpdateSelectedProducts: boolean;
begin
  if not Assigned(FOnGetSelectedProducts) then Exit(False);

  FOnGetSelectedProducts(FSelected);
  Result := FSelected.Count > 0;
end;

procedure TGUIEnvironment.UpdateServerConfigItems(Items: TServerConfigItems);
begin
  var OldItems := TServerConfigItems.Create;
  try
    GetServerConfigItems(OldItems);

    // Delete all but reserved
    for var OldItem in OldItems do
      if not OldItem.IsReserved then
        RemoveServerConfigItem(OldItem.Name);

    // Re-add all but reserved
    for var Item in Items do
      if not Item.IsReserved then
        AddServerConfigItem(Item);

    // Change enable status of reserved items
    for var Item in Items do
      if Item.IsReserved then
        EnableServerConfigItem(Item.Name, Item.Enabled);
  finally
    OldItems.Free;
  end;
end;

{ TGUIProductList }

function TGUIProductList.Find(const ProductId: string): TGUIProduct;
begin
  for var Product in Self do
    if Product.Id = ProductId then
      Exit(Product);
  Result := nil;
end;

{ TGUIProduct }

function TGUIProduct.DisplayName: string;
begin
  if Name.Trim <> '' then
    Result := Name
  else
    Result := Id;
end;

function TGUIProduct.IsOutdated: Boolean;
begin
  Result := not RemoteVersion.IsNull and not LocalVersion.IsNull and (RemoteVersion > LocalVersion);
end;

end.

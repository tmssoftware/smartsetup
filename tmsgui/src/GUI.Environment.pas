unit GUI.Environment;

{$SCOPEDENUMS ON}

interface

uses
  System.Generics.Collections, System.SysUtils, System.Classes, System.StrUtils,
  Deget.Version, UTmsRunner, UProductInfo, ULogger, UMultiLogger, UCommonTypes;

type
  TProductStatus = (NotInstalled, Available, Installed);
  TLogLevel = (Trace, Info, Error);

  TLogMessageProc = reference to procedure(const Level: TLogLevel; const Message: string);

  TTmsRunner = UTmsRunner.TTmsRunner;

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
    FOutput: string;
    FSessionId: string;
  public
    constructor Create(const AText: string; const ALevel: TLogLevel = TLogLevel.Info; const AOutput: string = '');
    property Text: string read FText write FText;
    property DateTime: TDateTime read FDateTime;
    property Level: TLogLevel read FLevel write FLevel;
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
    FInfo: TTmsInfo;
    FServer: string;
    FServers: TServerConfigItems;
    FOnRequestCredentials: TRequestCredentialsEvent;
    FOnGetSelectedProducts: TGetSelectedProductsProc;
    FOnLogItemGenerated: TLogItemEvent;
    FLogItems: TObjectList<TGUILogItem>;
    FOnCommandOutput: TCommandOutputProc;
    FProductFilter: TProductFilter;
    FRunningCount: Integer;
    FOnRunStart: TProc;
    FOnRunFinish: TProc;
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
    function TryBeginExclusive: Boolean;
    function SelectedProductIds: TArray<string>;
    procedure ExecuteBuild(FullBuild: Boolean; ProgressCallback: TProductProgressProc);
    procedure DoRunStart;
    procedure DoRunFinish;
    procedure DoNotifyNewVersion;
    procedure DoRunnerCreated(Runner: TTmsRunner);
    procedure RefreshFetchedProducts(Filter: TProductFilter);
    procedure ReplaceFetchedProducts(NewProducts: TGUIProductList; Filter: TProductFilter);
    procedure ApplyProductFilters;
    procedure BeginRunning;
    procedure EndRunning;
    procedure AddActiveRunner(Runner: TTmsRunner);
    procedure RemoveActiveRunner(Runner: TTmsRunner);
  protected
    procedure RunAsync<T: TTmsRunner, constructor>(Proc: TProc<T>);
    procedure RunSync<T: TTmsRunner, constructor>(Proc: TProc<T>);
    procedure RunBackground(Proc: TProc);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure RefreshInfo;
    procedure RefreshServers;

    function IsRunning: Boolean;
    procedure CancelRun;
    
    // Execute the build all the currently selected products (or all if none is selected)
    procedure ExecuteFullBuild(ProgressCallback: TProductProgressProc);
    procedure ExecutePartialBuild(ProgressCallback: TProductProgressProc);
    procedure ExecuteInstall(ProgressCallback: TProductProgressProc);
    procedure ExecuteUninstall(ProgressCallback: TProgressProc);

    // Execute install explicitly passing the product ids to be installed
    procedure ExecuteInstallProducts(const ProductIds: TArray<string>; ProgressCallback: TProductProgressProc);

    // Execute self-update command and fires RelaunchCallback if a new update is available and downloaded
    procedure ExecuteSelfUpdate(ProgressCallback: TProgressProc; RelaunchCallback: TProc);

    // Execute pin/unpin operations on selected products
    procedure ExecutePinSelected;
    procedure ExecuteUnpinSelected;

    // Updates the credentials
    // Fires the event OnRequestCredentials for an opportunity to offer user an UI to enter credentials
    procedure ExecuteRequestCredentials;

    procedure ExecuteConfigure(Silent: Boolean = False);
    function ExecuteLogView(const SessionId: string; const Print: Boolean): string;

    // Change the current applied filter. Will fire the OnProductsUpdated after the product list is modified.
    procedure ChangeProductFilter(Filter: TProductFilter);

    // Retrieves the current applied product filter
    function IsFilterActive(Filter: TProductFilter): Boolean;

    // Functions for enable/disable actions
    function CanInstallSelected: Boolean;
    function CanUninstallSelected: Boolean;
    function CanBuild: Boolean;
    function CanApplyFilter: Boolean;
    function CanRequestCredentials: Boolean;
    function CanConfigure: Boolean;
    function CanPinSelected: Boolean;
    function CanUnpinSelected: Boolean;

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
    /// </summary>
    procedure SetServer(const Value: string);

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

    property OnRunStart: TProc read FOnRunStart write FOnRunStart;
    property OnRunFinish: TProc read FOnRunFinish write FOnRunFinish;

    property OnLogItemGenerated: TLogItemEvent read FOnLogItemGenerated write FOnLogItemGenerated;
    property OnCommandOutput: TCommandOutputProc read FOnCommandOutput write FOnCommandOutput;

    property OnNewVersionDetected: TProc read FOnNewVersionDetected write FOnNewVersionDetected;

    property OnRunnerCreated: TRunnerProc read FOnRunnerCreated write FOnRunnerCreated;

    property StartWorking: TProc read FStartWorking write FStartWorking;
    property StopWorking: TProc read FStopWorking write FStopWorking;
  end;

implementation

uses
  Masks, IOUtils;

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

constructor TGUILogItem.Create(const AText: string; const ALevel: TLogLevel; const AOutput: string);
begin
  inherited Create;
  FText := AText;
  FLevel := ALevel;
  FOutput := AOutput;
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

procedure TGUIEnvironment.BeginRunning;
begin
  if AtomicIncrement(FRunningCount) = 1 then
    DoRunStart;
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
    for var Runner in FActiveRunners do
      Runner.Cancel;
  finally
    TMonitor.Exit(FActiveRunners);
  end;
end;

procedure TGUIEnvironment.AddActiveRunner(Runner: TTmsRunner);
begin
  TMonitor.Enter(FActiveRunners);
  try
    FActiveRunners.Add(Runner);
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

function TGUIEnvironment.CanPinSelected: Boolean;
begin
  if IsRunning then Exit(False);
  if not UpdateSelectedProducts then Exit(false);
  Result := False;
  for var Product in FSelected do
    if not Product.IsPinned then
      Exit(True);
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
  // Wait a little bit for the runner to finish. We could use TEvent here, but let's make it simple for now
  FDestroying := True;
  CancelRun;
  for var I := 1 to 100 do
  begin
    if not IsRunning then
      break;
    // Not Sleep: a worker waiting in TThread.Synchronize would never finish and we would always wait the full time.
    CheckSynchronize(100);
  end;
  // Run what the workers queued, so nothing queued refers to this object after it is freed.
  while CheckSynchronize do ;

  FProducts.Free;
  FFetchedProducts.Free;
  FSelected.Free;
  FLogItems.Free;
  FInfo.Free;
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

procedure TGUIEnvironment.DoRunFinish;
begin
  if Assigned(FOnRunFinish) then
    FOnRunFinish();
end;

procedure TGUIEnvironment.DoRunnerCreated(Runner: TTmsRunner);
begin
  if Assigned(FOnRunnerCreated) then
    FOnRunnerCreated(Runner);
end;

procedure TGUIEnvironment.DoRunStart;
begin
  if Assigned(FOnRunStart) then
    FOnRunStart();
end;

procedure TGUIEnvironment.RunAsync<T>(Proc: TProc<T>);
begin
  // Claimed here, on the calling thread, so a second click can't start a second job before the thread runs.
  if not TryBeginExclusive then
  begin
    Logger.Error('tms.exe is already running');
    Exit;
  end;
  try
    TThread.CreateAnonymousThread(
      procedure
      begin
        try
          RunSync<T>(Proc);
        finally
          EndRunning;
        end;
      end)
      .Start;
  except
    EndRunning;
    raise;
  end;
end;

procedure TGUIEnvironment.RunBackground(Proc: TProc);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      while not FDestroying do
      begin
        if TryBeginExclusive then
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
    end).Start;
end;

procedure TGUIEnvironment.ExecuteSelfUpdate(ProgressCallback: TProgressProc; RelaunchCallback: TProc);
begin
  RunBackground(
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
  if AtomicDecrement(FRunningCount) = 0 then
    DoRunFinish;
end;

procedure TGUIEnvironment.ExecuteBuild(FullBuild: Boolean; ProgressCallback: TProductProgressProc);
begin
  // Read the selection here, on the main thread. Inside the worker it read the list view from a
  // background thread, and got the selection as it was then, not when the user clicked.
  var ProductIds := SelectedProductIds;
  RunAsync<TTmsBuildRunner>(
    procedure(Runner: TTmsBuildRunner)
    begin
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

      RefreshFetchedProducts(FProductFilter);
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

      RefreshFetchedProducts(FProductFilter);
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
  RefreshFetchedProducts(FProductFilter);
end;

procedure TGUIEnvironment.ExecuteUninstall(ProgressCallback: TProgressProc);
begin
  var ProductIds := SelectedProductIds; // on the main thread, see ExecuteBuild
  RunAsync<TTmsUninstallRunner>(
    procedure(Runner: TTmsUninstallRunner)
    begin
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

      RefreshFetchedProducts(FProductFilter);
    end);
end;

procedure TGUIEnvironment.ExecuteUnpinSelected;
begin
  RunSync<TTmsUnpinRunner>(
    procedure(Runner: TTmsUnpinRunner)
    begin
      Runner.RunUnpin(SelectedProductIds)
    end);
  RefreshFetchedProducts(FProductFilter);
end;

procedure TGUIEnvironment.RunnerOutputEvent(const S: string);
begin
  if Assigned(FOnCommandOutput) then
    FOnCommandOutput(S);
end;

procedure TGUIEnvironment.RunSync<T>(Proc: TProc<T>);
begin
  var LocalRunner := T.Create;
  try
    DoRunnerCreated(LocalRunner);
    try
      // A list, not a single "current runner" field: runners nest and run on several threads, and
      // saving/restoring one field across threads could leave it pointing at a freed runner.
      AddActiveRunner(LocalRunner);
      try
        BeginRunning;
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

      if LocalRunner is TAbstractTmsBuildRunner then
      begin
        GenerateLogItem(TGUILogItem.Create(
            LocalRunner.ExeFileName,
            TLogLevel.Info,
            LocalRunner.Output.Text
        ));
      end;

    except
      on E: Exception do
      begin
        GenerateLogItem(TGUILogItem.Create(
          E.Message,
          TLogLevel.Error,
          LocalRunner.Output.Text
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

procedure TGUIEnvironment.Start;
begin
  RefreshServers;

  if FServers.IsEnabled('tms') and not Info.HasCredentials then
    ExecuteRequestCredentials;

  RunBackground(procedure
    begin
      if FServers.RemotesEnabled then
        RefreshFetchedProducts(TProductFilter.All)
      else
        RefreshFetchedProducts(TProductFilter.Installed);
    end);
end;

procedure TGUIEnvironment.GenerateLogItem(Item: TGUILogItem);
begin
  // Log items arrive from worker threads, but FLogItems is read by the log list on the main thread,
  // so it is only changed there. (From the main thread, Queue runs the procedure immediately.)
  // Deleted log files no longer need checking here: acViewHtmlLogExecute clears the session id
  // when the file is gone. The old check started one "tms log-view" per log item for every new item.
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
  if (FInfo <> nil) and not (FInfo.Initialized) then FreeAndNil(FInfo);

  if FInfo = nil then
  begin
    FInfo := TTmsInfo.Create;
    try
      RunSync<TTmsInfoRunner>(
        procedure(Runner: TTmsInfoRunner)
        begin
          Runner.RunInfo(FInfo);
        end);
    except
      FreeAndNil(FInfo);
      raise;
    end;
  end;
  Result := FInfo;
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
  FreeAndNil(FInfo);
end;

procedure TGUIEnvironment.RefreshFetchedProducts(Filter: TProductFilter);
begin
  var Server := FServer;
  RunSync<TTmsListRunner>(
    procedure(ListRunner: TTmsListRunner)
    begin
      if Info.FolderInitialized then
        ListRunner.RunList;
      RunSync<TTmsListRemoteRunner>(
        procedure(RemoteRunner: TTmsListRemoteRunner)
        begin
          if Info.FolderInitialized then
          begin
            RemoteRunner.Server := Server;
            RemoteRunner.RunListRemote;
          end;

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

procedure TGUIEnvironment.ExecuteRequestCredentials;
begin
  if not Assigned(FOnRequestCredentials) then
    Exit;

  RunSync<TTmsCredentialsRunner>(
    procedure(Runner: TTmsCredentialsRunner)
    begin
      var Email: string;
      var Code: string;
      Runner.RunGetCredentials(Email, Code);

      var LastValid := True;
      var Confirm: Boolean;
      repeat
        Confirm := False;
        var Disable := False;
        FOnRequestCredentials(Email, Code, Confirm, not LastValid, Disable);
        if Confirm then
        begin
          LastValid := Runner.RunUpdateCredentials(Email, Code);
          RefreshInfo;
        end
        else
        if Disable then
        begin
          EnableServerConfigItem('tms', False);
          RefreshServers;
        end;
      until not Confirm or LastValid;
    end);
end;

procedure TGUIEnvironment.ChangeProductFilter(Filter: TProductFilter);
begin
  if not TryBeginExclusive then
  begin
    Logger.Error('tms.exe is already running');
    Exit;
  end;
  try
    TThread.CreateAnonymousThread(
      procedure
      begin
        try
          RefreshFetchedProducts(Filter);
        finally
          EndRunning;
        end;
      end)
      .Start;
  except
    EndRunning;
    raise;
  end;
end;

function TGUIEnvironment.TryBeginExclusive: Boolean;
begin
  // Check and claim in one atomic step. The previous CheckRunning only logged an error and
  // returned, so its callers went on and started a second tms.exe anyway.
  Result := AtomicCmpExchange(FRunningCount, 1, 0) = 0;
  if Result then
    DoRunStart;
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
    on EMaskException do
      Mask := nil;
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

procedure TGUIEnvironment.SetServer(const Value: string);
begin
  if FServer <> Value then
  begin
    FServer := Value;
    // Asynchronous and guarded like any other job: it used to run list and list-remote on the main
    // thread, freezing the window, and could start while an install was running.
    ChangeProductFilter(FProductFilter);
  end;
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

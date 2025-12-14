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
    procedure Message(const MessageKind: TLogMessageKind; const Message: string); override;
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
    FLevel: TLogLevel;
    FOutput: string;
  public
    constructor Create(const AText: string; const ALevel: TLogLevel = TLogLevel.Info; const AOutput: string = '');
    property Text: string read FText write FText;
    property Level: TLogLevel read FLevel write FLevel;
    property Output: string read FOutput write FOutput;
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
    FFetchedProducts: TGUIProductList;
    FProducts: TGUIProductList;
    FSelected: TGUIProductList;
    FSearchFilter: string;
    FOnProductsUpdated: TProductsProc;
    FCurrentRunner: TTmsRunner;
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
    procedure UpdateSelectedProducts;
    procedure LogMessageReceived(const Level: TLogLevel; const Message: string);
    function GetInfo: TTmsInfo;
    procedure GenerateLogItem(Item: TGUILogItem);
    procedure RunnerOutputEvent(const S: string);
    procedure CheckRunning;
    function SelectedProductIds: TArray<string>;
    procedure ExecuteBuild(FullBuild: Boolean; ProgressCallback: TProductProgressProc);
    procedure RefreshInfo;
    procedure DoRunStart;
    procedure DoRunFinish;
    procedure DoNotifyNewVersion;
    procedure DoRunnerCreated(Runner: TTmsRunner);
    procedure RefreshFetchedProducts(Filter: TProductFilter);
    procedure ApplyProductFilters;
    procedure BeginRunning;
    procedure EndRunning;
  protected
    procedure RunAsync<T: TTmsRunner, constructor>(Proc: TProc<T>);
    procedure RunSync<T: TTmsRunner, constructor>(Proc: TProc<T>);
    procedure RunBackground(Proc: TProc);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
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

    // Updates the credentials
    // Fires the event OnRequestCredentials for an opportunity to offer user an UI to enter credentials
    procedure ExecuteRequestCredentials;

    procedure ExecuteConfigure(Silent: Boolean = False);

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
  end;

implementation

uses
  Masks;

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
  const Message: string);
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

{ TGUILogItem }

constructor TGUILogItem.Create(const AText: string; const ALevel: TLogLevel; const AOutput: string);
begin
  inherited Create;
  FText := AText;
  FLevel := ALevel;
  FOutput := AOutput;
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

  UpdateSelectedProducts;
  Result := False;
  for var Product in FSelected do
    if Product.Status in [TProductStatus.Installed, TProductStatus.Available] then
      Exit(True);
end;

procedure TGUIEnvironment.CancelRun;
begin
  if Assigned(FCurrentRunner) then
    FCurrentRunner.Cancel;
end;

function TGUIEnvironment.CanConfigure: Boolean;
begin
  if IsRunning then Exit(False);
  Result := True;
end;

function TGUIEnvironment.CanInstallSelected: Boolean;
begin
  if IsRunning then Exit(False);

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

function TGUIEnvironment.CanRequestCredentials: Boolean;
begin
  if IsRunning then Exit(False);
  Result := True;
end;

function TGUIEnvironment.CanUninstallSelected: Boolean;
begin
  if IsRunning then Exit(False);

  UpdateSelectedProducts;
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
  CancelRun;
  for var I := 1 to 10 do
  begin
    if IsRunning then
      Sleep(1000)
    else
      break;
  end;

  FProducts.Free;
  FFetchedProducts.Free;
  FSelected.Free;
  FLogItems.Free;
  FInfo.Free;
  FServers.Free;
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
  CheckRunning;
  TThread.CreateAnonymousThread(
    procedure
    begin
      BeginRunning;
      try
        RunSync<T>(Proc);
      finally
        EndRunning;
      end;
    end)
    .Start;
end;

procedure TGUIEnvironment.RunBackground(Proc: TProc);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      repeat
        if not IsRunning then
        begin
          BeginRunning;
          try
            Proc;
            Exit;
          finally
            EndRunning;
          end;
        end;
        Sleep(1000); // try a new check after a while
      until False;
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
      Runner.ProductIds.AddStrings(SelectedProductIds);
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

procedure TGUIEnvironment.ExecuteUninstall(ProgressCallback: TProgressProc);
begin
  RunAsync<TTmsUninstallRunner>(
    procedure(Runner: TTmsUninstallRunner)
    begin
      if Assigned(ProgressCallback) then
        ProgressCallback(0);

      Runner.OnOutputLine := RunnerOutputEvent;
      Runner.ProductIds.AddStrings(SelectedProductIds);
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
      var OldRunner := FCurrentRunner;
      try
        FCurrentRunner := LocalRunner;
        BeginRunning;
        try
          Proc(LocalRunner);
        finally
          EndRunning;
        end;
      finally
        FCurrentRunner := OldRunner;
      end;

      if LocalRunner.NewVersionDetected then
        DoNotifyNewVersion;
    except
      on E: Exception do
      begin
        GenerateLogItem(TGUILogItem.Create(
          Format('Error running %s: %s (%s)', [T.ClassName, E.Message, E.ClassName]),
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
  FLogItems.Add(Item);
  if Assigned(OnLogItemGenerated) then
    FOnLogItemGenerated(Item);
end;

function TGUIEnvironment.GetInfo: TTmsInfo;
begin
  if FInfo = nil then
  begin
    FInfo := TTmsInfo.Create;
    RunSync<TTmsInfoRunner>(
      procedure(Runner: TTmsInfoRunner)
      begin
        Runner.RunInfo(FInfo);
      end);
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
            RemoteRunner.Server := FServer;
            RemoteRunner.RunListRemote;
          end;
          ConsolidateGUIProductList(Self.FFetchedProducts, ListRunner.Products, RemoteRunner.Products);
          FProductFilter := Filter;

          // Filter products
          var Predicate :=
            function(Product: TGUIProduct): Boolean
            begin
              if (FProductFilter = TProductFilter.Installed) and (Product.Status <> TProductStatus.Installed) then
                Exit(False);

              if (FServer <> '') and not SameText(FServer, Product.Server) then
                Exit(False);

              Result := True;
            end;
          for var I := Self.FFetchedProducts.Count - 1 downto 0 do
            if not Predicate(Self.FFetchedProducts[I]) then
              Self.FFetchedProducts.Delete(I);

          // fire event to refresh producs
          ApplyProductFilters;
        end)
    end)
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
  CheckRunning;
  BeginRunning;
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
end;

procedure TGUIEnvironment.CheckRunning;
begin
  if IsRunning then
  begin
    Logger.Error('tms.exe is already running');
    Exit;
  end;
end;

procedure TGUIEnvironment.ApplyProductFilters;
begin
  var Filter := FSearchFilter.ToLower;
  var Mask := TMask.Create(Filter);
  try
    var Predicate :=
      function(Product: TGUIProduct): Boolean
      begin
        Result := True;
        if Filter.Trim <> '' then
        begin
          var IdLower := Product.Id.ToLower;
          var NameLower := Product.Name.ToLower;
          Result := IdLower.Contains(Filter) or NameLower.Contains(Filter) or Mask.Matches(IdLower);
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
    RefreshFetchedProducts(FProductFilter);
  end;
end;

procedure TGUIEnvironment.UpdateSelectedProducts;
begin
  if Assigned(FOnGetSelectedProducts) then
    FOnGetSelectedProducts(FSelected);
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

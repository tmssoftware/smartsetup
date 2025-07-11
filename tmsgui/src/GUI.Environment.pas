unit GUI.Environment;

{$SCOPEDENUMS ON}

interface

uses
  System.Generics.Collections, System.SysUtils, System.Classes, System.StrUtils,
  Deget.Version, UTmsRunner, UProductInfo, ULogger, UMultiLogger;

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
    FLocalVersion: TVersion;
    FRemoteVersion: TVersion;
    FName: string;
    FStatus: TProductStatus;
    FHasFetchInfo: Boolean;
    FVendorId: string;
  public
    function IsOutdated: Boolean;
    function DisplayName: string;
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property LocalVersion: TVersion read FLocalVersion write FLocalVersion;
    property RemoteVersion: TVersion read FRemoteVersion write FRemoteVersion;
    property Status: TProductStatus read FStatus write FStatus;
    property HasFetchInfo: Boolean read FHasFetchInfo write FHasFetchInfo;
    property VendorId: string read FVendorId write FVendorId;
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
  TRequestCredentialsEvent = reference to procedure(var Email, Code: string; var Confirm: Boolean; LastWasInvalid: Boolean);
  TGetSelectedProductsProc = reference to procedure(Products: TGUIProductList);
  TCommandOutputProc = reference to procedure(const PartialText: string);
  TProgressProc = reference to procedure(const Percent: Integer);
  TProductProgressProc = reference to procedure(const Info: TProductProgressInfo);
  TLogItemEvent = reference to procedure(const LogItem: TGUILogItem);
  TRunnerProc = reference to procedure(Runner: TTmsRunner);

  TGUIEnvironment = class
  private
    FProducts: TGUIProductList;
    FSelected: TGUIProductList;
    FOnProductsUpdated: TProductsProc;
    FCurrentRunner: TTmsRunner;
    FInfo: TTmsInfo;
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
    procedure RefreshProducts(Filter: TProductFilter);
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

    function IsRunning: Boolean;
    procedure CancelRun;
    
    // Execute the build all the currently selected products (or all if none is selected)
    procedure ExecuteFullBuild(ProgressCallback: TProductProgressProc);
    procedure ExecutePartialBuild(ProgressCallback: TProductProgressProc);
    procedure ExecuteInstall(ProgressCallback: TProductProgressProc);
    procedure ExecuteUninstall(ProgressCallback: TProgressProc);

    // Execute self-update command and fires RelaunchCallback if a new update is available and downloaded
    procedure ExecuteSelfUpdate(ProgressCallback: TProgressProc; RelaunchCallback: TProc);

    // Updates the credentials
    // Fires the event OnRequestCredentials for an opportunity to offer user an UI to enter credentials
    procedure ExecuteRequestCredentials;

    procedure ExecuteConfigure;

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

    /// <summary>
    ///   Retrieves general information about Smart Setup folder
    /// </summary>
    property Info: TTmsInfo read GetInfo;

    property Products: TGUIProductList read FProducts;
    property LogItems: TObjectList<TGUILogItem> read FLogItems;

    property OnProductsUpdated: TProductsProc read FOnProductsUpdated write FOnProductsUpdated;
    property OnRequestCredentials: TRequestCredentialsEvent read FOnRequestCredentials write FOnRequestCredentials;

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
  for var Product in Products do
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

    // Update VendorId. For now, only remote products have vendor id, so we are setting here regardless.
    GUIProduct.VendorId := Product.VendorId;
  end;
end;

constructor TGUIEnvironment.Create;
begin
  inherited Create;
  FProducts := TGUIProductList.Create;
  FSelected := TGUIProductList.Create(False);
  FLogItems := TObjectList<TGUILogItem>.Create;

  // Init logging
  var GUILogger := TGUILogger.Create;
  Logger := TMultiLogger.Create([GUILogger]);
  GUILogger.OnLogMessage := LogMessageReceived;
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
  FSelected.Free;
  FLogItems.Free;
  FInfo.Free;
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

      RefreshProducts(FProductFilter);
    end);
end;

procedure TGUIEnvironment.ExecuteConfigure;
begin
  RunSync<TTmsConfigureRunner>(
    procedure(Runner: TTmsConfigureRunner)
    begin
      Runner.RunConfigure;
      RefreshInfo;
    end);
end;

procedure TGUIEnvironment.ExecuteFullBuild(ProgressCallback: TProductProgressProc);
begin
  ExecuteBuild(True, ProgressCallback);
end;

procedure TGUIEnvironment.ExecuteInstall(ProgressCallback: TProductProgressProc);
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

      RefreshProducts(FProductFilter);
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

      RefreshProducts(FProductFilter);
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
  if not Info.HasCredentials then
    ExecuteRequestCredentials;

  RunBackground(procedure
    begin
      if Info.HasCredentials then
        RefreshProducts(TProductFilter.All)
      else
        RefreshProducts(TProductFilter.Installed);
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

function TGUIEnvironment.IsRunning: Boolean;
begin
  Result := FRunningCount > 0;
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

procedure TGUIEnvironment.RefreshProducts(Filter: TProductFilter);
begin
  RunSync<TTmsListRunner>(
    procedure(ListRunner: TTmsListRunner)
    begin
      if Info.FolderInitialized then
        ListRunner.RunList;
      RunSync<TTmsListRemoteRunner>(
        procedure(RemoteRunner: TTmsListRemoteRunner)
        begin
          if Info.FolderInitialized and Info.HasCredentials then
            RemoteRunner.RunListRemote;
          ConsolidateGUIProductList(Self.Products, ListRunner.Products, RemoteRunner.Products);
          FProductFilter := Filter;

          // Filter products
          var Predicate :=
            function(Product: TGUIProduct): Boolean
            begin
              case FProductFilter of
                TProductFilter.Installed: Result := not Product.LocalVersion.IsNull;
              else
                Result := True;
              end;
            end;
          for var I := Self.Products.Count - 1 downto 0 do
            if not Predicate(Self.Products[I]) then
              Self.Products.Delete(I);

          // fire event to refresh producs
          if Assigned(OnProductsUpdated) then
            FOnProductsUpdated(FProducts);
        end)
    end)
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
        FOnRequestCredentials(Email, Code, Confirm, not LastValid);
        if Confirm then
        begin
          LastValid := Runner.RunUpdateCredentials(Email, Code);
          RefreshInfo;
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
        RefreshProducts(Filter);
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

procedure TGUIEnvironment.UpdateSelectedProducts;
begin
  if Assigned(FOnGetSelectedProducts) then
    FOnGetSelectedProducts(FSelected);
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

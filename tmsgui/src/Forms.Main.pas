unit Forms.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.UITypes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, UProductInfo, Deget.Version,
  GUI.Environment, Forms.Credentials, System.Actions, Vcl.ActnList, Vcl.Buttons, Vcl.Menus, System.Types,
  Forms.Config, UCommonTypes, Forms.Start, Vcl.ControlList, Generics.Collections, Forms.LogDetails,
  Vcl.VirtualImage, Util.Animator;

type
  TMainForm = class(TForm)
    ActionList1: TActionList;
    acInstall: TAction;
    acUninstall: TAction;
    acCancelExecution: TAction;
    acFilterAll: TAction;
    acFilterInstalled: TAction;
    acFullBuild: TAction;
    BuildMenu: TPopupMenu;
    Rebuild1: TMenuItem;
    acPartialBuild: TAction;
    Partialbuild1: TMenuItem;
    acCredentials: TAction;
    acConfigure: TAction;
    Panel3: TPanel;
    PageControl1: TPageControl;
    tsProducts: TTabSheet;
    LeftPanel: TPanel;
    rbAll: TRadioButton;
    rbInstalled: TRadioButton;
    lvProducts: TListView;
    RightPanel: TPanel;
    Button1: TButton;
    btnInstall: TButton;
    btnUninstall: TButton;
    Button4: TButton;
    tsOutput: TTabSheet;
    OutputMemo: TMemo;
    LogPanel: TPanel;
    StatusBar: TStatusBar;
    acVersionHistory: TAction;
    pmProducts: TPopupMenu;
    Openversionhistory1: TMenuItem;
    acSettings: TAction;
    TopPanel: TPanel;
    edSearch: TEdit;
    btConfiguration2: TSpeedButton;
    cbServer: TComboBox;
    lbServer: TLabel;
    acSearchFocus: TAction;
    btCredentials: TButton;
    acInstallVersion: TAction;
    Installversion1: TMenuItem;
    acPin: TAction;
    acUnpin: TAction;
    Pinversion1: TMenuItem;
    Unpinversion1: TMenuItem;
    lbLog: TControlList;
    lblError: TLabel;
    btnShowLog: TControlListButton;
    LogSplitter: TSplitter;
    lblErrorCaption: TLabel;
    lblTime: TLabel;
    btnOpenHTMLLog: TControlListButton;
    WorkingFolderDialog: TFileOpenDialog;
    ProgressPanel: TPanel;
    btnCancel: TSpeedButton;
    ProgressBar: TProgressBar;
    acLogDetails: TAction;
    acViewHtmlLog: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure acInstallUpdate(Sender: TObject);
    procedure acUninstallUpdate(Sender: TObject);
    procedure acCancelExecutionUpdate(Sender: TObject);
    procedure acCancelExecutionExecute(Sender: TObject);
    procedure acFilterAllExecute(Sender: TObject);
    procedure acFilterInstalledExecute(Sender: TObject);
    procedure acFilterAllUpdate(Sender: TObject);
    procedure acFilterInstalledUpdate(Sender: TObject);
    procedure acFullBuildExecute(Sender: TObject);
    procedure acFullBuildUpdate(Sender: TObject);
    procedure acInstallExecute(Sender: TObject);
    procedure acPartialBuildExecute(Sender: TObject);
    procedure acPartialBuildUpdate(Sender: TObject);
    procedure acCredentialsUpdate(Sender: TObject);
    procedure acCredentialsExecute(Sender: TObject);
    procedure acUninstallExecute(Sender: TObject);
    procedure acConfigureUpdate(Sender: TObject);
    procedure acConfigureExecute(Sender: TObject);
    procedure acVersionHistoryUpdate(Sender: TObject);
    procedure acVersionHistoryExecute(Sender: TObject);
    procedure lvProductsCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure lvProductsColumnClick(Sender: TObject; Column: TListColumn);
    procedure acSettingsExecute(Sender: TObject);
    procedure acSettingsUpdate(Sender: TObject);
    procedure edSearchChange(Sender: TObject);
    procedure acSearchFocusExecute(Sender: TObject);
    procedure cbServerChange(Sender: TObject);
    procedure acInstallVersionUpdate(Sender: TObject);
    procedure acInstallVersionExecute(Sender: TObject);
    procedure lvProductsInfoTip(Sender: TObject; Item: TListItem; var InfoTip: string);
    procedure lvProductsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure acUnpinUpdate(Sender: TObject);
    procedure acPinUpdate(Sender: TObject);
    procedure acUnpinExecute(Sender: TObject);
    procedure acPinExecute(Sender: TObject);
    procedure lbLogBeforeDrawItem(AIndex: Integer; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
    procedure StatusBarClick(Sender: TObject);
    procedure acLogDetailsExecute(Sender: TObject);
    procedure acViewHtmlLogExecute(Sender: TObject);
  private
    GUI: TGUIEnvironment;
    Relaunch: Boolean;
    SortColumn: Integer;
    SortDescending: Boolean;
    OverCaption: Boolean;
    LogDetailsForm: TLogDetailsForm;
    WorkingAnimator: TAnimator;
    InstallAnimator: TAnimator;
    UninstallAnimator: TAnimator;
    BuildAnimator: TAnimator;
    procedure WMSettingChange(var Msg: TWMSettingChange); message WM_SETTINGCHANGE;
    function CompareStatus(const Status1, Status2: TProductStatus): Integer;
    function CompareVersion(const Version1, Version2: TLenientVersion): Integer;
    function CompareIcons(const Product1, Product2: TGUIProduct): Integer;
    function FindProductItem(const ProductId: string): TListItem;
    procedure ShowInfo;
    function ProductFromItem(Item: TListItem): TGUIProduct;
    function GetVersionHistoryUrl(Product: TGUIProduct): string;
    procedure SortProducts;
    procedure UpdateSortArrows;
    function GetCmdLineParameter(const Prefix: string): string;
    function Repository: string;
{$IFDEF DEBUG}
    function GetAutomationScript: string;
    function GetAutomationLog: string;
{$ENDIF}
    function GetLogIco(const Item: TGUILogItem): string;

    function GetLastError: string;
  public
    procedure InitiateAction; override;
    procedure ProductsUpdatedEvent(Products: TGUIProductList);
    procedure ServersUpdatedEvent(Servers: TServerConfigItems);
    procedure GetSelectedProductsEvent(Products: TGUIProductList);
    procedure RequestCredentialsEvent(var Email, Code: string; var Confirm: Boolean;
      LastWasInvalid: Boolean; var DisableServer: Boolean);
    procedure LogItemGeneratedEvent(const Item: TGUILogItem);
    procedure CommandOutputEvent(const Text: string);
    procedure ProgressEvent(const Percent: Integer);
    procedure ProductProgressEvent(const Info: TProductProgressInfo);
    procedure RunnerCreatedEvent(Runner: TTmsRunner);
    function AnimatorFor(const Command: TTmsCommand): TAnimator;
    procedure RunStartEvent(const Command: TTmsCommand);
    procedure RunFinishEvent(const Command: TTmsCommand);
    procedure NewVersionDetectedEvent;

    {$IFDEF DEBUG}
    function ProductCount: integer;
    {$ENDIF}
  end;

var
  MainForm: TMainForm;

implementation

uses
  Winapi.ShellAPI, Winapi.CommCtrl,
  UTheming, IOUtils,
  GUI.Automation,
  Forms.VersionPicker, Forms.Dialogs;

{$R *.dfm}

const
  // Column indexes in lvProducts. Must match the column order in Forms.Main.dfm.
  // Column 0 is the item Caption; column N > 0 is SubItems[N - 1] (see SubItemIndex).
  ColIcons = 0;
  ColId = 1;
  ColName = 2;
  ColLocalVersion = 3;
  ColRemoteVersion = 4;
  ColStatus = 5;
  ColLast = ColStatus;

function SubItemIndex(const Column: Integer): Integer;
begin
  Result := Column - 1;
end;

procedure TMainForm.acFullBuildExecute(Sender: TObject);
begin
  GUI.ExecuteFullBuild(ProductProgressEvent);
end;

procedure TMainForm.acFullBuildUpdate(Sender: TObject);
begin
  acFullBuild.Enabled := GUI.CanBuild;
end;

procedure TMainForm.acCancelExecutionExecute(Sender: TObject);
begin
  GUI.CancelRun;
end;

procedure TMainForm.acCancelExecutionUpdate(Sender: TObject);
begin
  acCancelExecution.Enabled := GUI.IsRunning;
end;

procedure TMainForm.acConfigureExecute(Sender: TObject);
begin
  GUI.ExecuteConfigure;
end;

procedure TMainForm.acConfigureUpdate(Sender: TObject);
begin
  acConfigure.Enabled := GUI.CanConfigure;
end;

procedure TMainForm.acCredentialsExecute(Sender: TObject);
begin
  GUI.ExecuteRequestCredentials;
end;

procedure TMainForm.acCredentialsUpdate(Sender: TObject);
begin
  acCredentials.Enabled := GUI.CanRequestCredentials;
end;

procedure TMainForm.acFilterAllExecute(Sender: TObject);
begin
  GUI.ChangeProductFilter(TProductFilter.All);
end;

procedure TMainForm.acFilterAllUpdate(Sender: TObject);
begin
  acFilterAll.Checked := GUI.IsFilterActive(TProductFilter.All);
  acFilterAll.Enabled := GUI.CanApplyFilter;
end;

procedure TMainForm.acFilterInstalledExecute(Sender: TObject);
begin
  GUI.ChangeProductFilter(TProductFilter.Installed);
end;

procedure TMainForm.acFilterInstalledUpdate(Sender: TObject);
begin
  acFilterInstalled.Checked := GUI.IsFilterActive(TProductFilter.Installed);
  acFilterInstalled.Enabled := GUI.CanApplyFilter;
end;

procedure TMainForm.acInstallExecute(Sender: TObject);
begin
  GUI.ExecuteInstall(ProductProgressEvent);
end;

procedure TMainForm.acInstallUpdate(Sender: TObject);
begin
  acInstall.Enabled := GUI.CanInstallSelected;
end;

procedure TMainForm.acInstallVersionExecute(Sender: TObject);
begin
  var Product := ProductFromItem(lvProducts.Selected);
  if Product = nil then exit;

  var Versions := TVersionInfoList.Create;
  try
    GUI.GetProductVersions(Product.Id, Versions);
    var VersionToInstall := TVersionPickerForm.Execute(Product.DisplayName, Versions);
    if VersionToInstall <> '' then
    begin
      var InstallId := Format('%s:%s', [Product.Id, VersionToInstall]);
      GUI.ExecuteInstallProducts([InstallId], ProductProgressEvent);
    end;
  finally
    Versions.Free;
  end;
end;

procedure TMainForm.acInstallVersionUpdate(Sender: TObject);
begin
  var Product := ProductFromItem(lvProducts.Selected);
  TAction(Sender).Enabled := (Product <> nil) and GUI.CanInstallSelected;
end;

procedure TMainForm.acLogDetailsExecute(Sender: TObject);
begin
  var RevIndex := GUI.LogItems.Count - 1 - lbLog.ItemIndex;
  if (RevIndex < 0) or (RevIndex >= GUI.LogItems.Count) then exit;
  LogDetailsForm.SetLogText(GUI.LogItems[RevIndex].Text, GUI.LogItems[RevIndex].Output);
  LogDetailsForm.ShowModal;
end;

procedure TMainForm.acPartialBuildExecute(Sender: TObject);
begin
  GUI.ExecutePartialBuild(ProductProgressEvent);
end;

procedure TMainForm.acPartialBuildUpdate(Sender: TObject);
begin
  acPartialBuild.Enabled := GUI.CanBuild;
end;

procedure TMainForm.acPinExecute(Sender: TObject);
begin
  GUI.ExecutePinSelected;
end;

procedure TMainForm.acPinUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := GUI.CanPinSelected;
end;

procedure TMainForm.acSettingsExecute(Sender: TObject);
begin
  var ConfigInfo := TConfigInfo.Create;
  try
    GUI.GetServerConfigItems(ConfigInfo.Servers);

    var Form := TConfigForm.Create(Application);
    try
      Form.OnUpdateServers := procedure
        begin
          GUI.UpdateServerConfigItems(ConfigInfo.Servers);
          GUI.RefreshServers;
        end;
      Form.Execute(ConfigInfo);
    finally
      Form.Free;
    end;
  finally
    ConfigInfo.Free;
  end;
end;

procedure TMainForm.acSettingsUpdate(Sender: TObject);
begin
  acSettings.Enabled := GUI.CanConfigure;
end;

procedure TMainForm.acSearchFocusExecute(Sender: TObject);
begin
  if edSearch.CanFocus then
    edSearch.SetFocus;
end;

procedure TMainForm.acUninstallExecute(Sender: TObject);
begin
  GUI.ExecuteUninstall(ProgressEvent);
end;

procedure TMainForm.acUninstallUpdate(Sender: TObject);
begin
  acUninstall.Enabled := GUI.CanUninstallSelected;
end;

procedure TMainForm.acUnpinExecute(Sender: TObject);
begin
  GUI.ExecuteUnpinSelected;
end;

procedure TMainForm.acUnpinUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := GUI.CanUnpinSelected;
end;

procedure TMainForm.acVersionHistoryExecute(Sender: TObject);
begin
  var Product := ProductFromItem(lvProducts.Selected);
  var Url := GetVersionHistoryUrl(Product);

  if Url <> '' then
    ShellExecute(0, 'open', PChar(Url), '', '', SW_SHOWNORMAL);
end;

procedure TMainForm.acVersionHistoryUpdate(Sender: TObject);
begin
  var Product := ProductFromItem(lvProducts.Selected);
  TAction(Sender).Enabled := GetVersionHistoryUrl(Product) <> '';
end;


procedure TMainForm.acViewHtmlLogExecute(Sender: TObject);
begin
  var RevIndex := GUI.LogItems.Count - 1 - lbLog.ItemIndex;
  if (RevIndex < 0) or (RevIndex >= GUI.LogItems.Count) then exit;
  var SessionId := GUI.LogItems[RevIndex].SessionId;
  if SessionId = '' then exit;

  var LogFile := GUI.ExecuteLogView(SessionId, true);
  if not TFile.Exists(LogFile) then
  begin
    GUI.LogItems[RevIndex].SessionId := ''; //file was deleted, we won't show the log button anymore.
    exit;
  end;

  GUI.ExecuteLogView(SessionId, false);
end;

procedure TMainForm.cbServerChange(Sender: TObject);
begin
  var Server := '';
  if cbServer.ItemIndex > 0 then // ignore "all"
    Server := cbServer.Items[cbServer.ItemIndex];
  if not GUI.SetServer(Server) then
  begin
    // Another job is running and the list was not refreshed: show the server the list is really for.
    var Index := cbServer.Items.IndexOf(GUI.Server);
    if Index = -1 then
      Index := 0; // "all"
    cbServer.ItemIndex := Index;
  end;
end;

procedure TMainForm.CommandOutputEvent(const Text: string);
begin
  TThread.Queue(nil, procedure
    begin
      OutputMemo.Lines.Add(Text);
      SendMessage(OutputMemo.Handle, EM_LINESCROLL, 0, OutputMemo.Lines.Count);
    end);
end;

function TMainForm.CompareIcons(const Product1, Product2: TGUIProduct): Integer;

  function CompareBool(B1, B2: Boolean): Integer;
  begin
    if B1 and not B2 then
      Result := -1
    else
    if B2 and not B1 then
      Result := 1
    else
      Result := 0;
  end;

begin
  Result := CompareBool(Product1.IsOutdated, Product2.IsOutdated);
  if Result = 0 then
    Result := CompareBool(Product1.IsPinned, Product2.IsPinned);
end;

function TMainForm.CompareStatus(const Status1, Status2: TProductStatus): Integer;
const
  StatusValue: array[TProductStatus] of Integer = (
    2, // NotInstalled
    1, // Available
    0  // Installed
  );
begin
  Result := StatusValue[Status1] - StatusValue[Status2];
end;

function TMainForm.CompareVersion(const Version1, Version2: TLenientVersion): Integer;
begin
  if Version1 < Version2 then
    Result := -1
  else
  if Version1 > Version2 then
    Result := 1
  else
    Result := 0;
end;

procedure TMainForm.edSearchChange(Sender: TObject);
begin
  GUI.SetSearchFilter(edSearch.Text);
end;

function TMainForm.FindProductItem(const ProductId: string): TListItem;
begin
  for var Item in lvProducts.Items do
    if (Item.Data <> nil) and SameText(TGUIProduct(Item.Data).Id, ProductId) then
      Exit(Item);
  Result := nil;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SortColumn := ColStatus;  // sort by status by default

  LogDetailsForm := TLogDetailsForm.Create(Self); //keep so it doesn't keep resizing when you reopen it.
  LogPanel.Height := 1; // 0 would not allow us to resize the box when closed.

  WorkingAnimator := TAnimator.Create(500, true, 'Working...', '',
    procedure (Value: string)
    begin
      StatusBar.Panels[3].Text := Value;
    end);

  InstallAnimator := TAnimator.Create(50, false, 'Installing', acInstall.Caption,
    procedure (Value: string)
    begin
      acInstall.Caption := Value;
    end);

  UninstallAnimator := TAnimator.Create(50, false, 'Uninstalling', acUninstall.Caption,
    procedure (Value: string)
    begin
      acUninstall.Caption := Value;
    end);

  BuildAnimator := TAnimator.Create(50, false, 'Building', acFullBuild.Caption,
    procedure (Value: string)
    begin
      acFullBuild.Caption := Value;
    end);

  GUI := TGUIEnvironment.Create;

  GUI.OnProductsUpdated := ProductsUpdatedEvent;
  GUI.OnServersUpdated := ServersUpdatedEvent;
  GUI.OnRequestCredentials := RequestCredentialsEvent;
  GUI.OnGetSelectedProducts := GetSelectedProductsEvent;
  GUI.OnLogItemGenerated := LogItemGeneratedEvent;
  GUI.OnCommandOutput := CommandOutputEvent;
  GUI.OnRunStart := RunStartEvent;
  GUI.OnRunFinish := RunFinishEvent;
  GUI.OnNewVersionDetected := NewVersionDetectedEvent;
  GUI.OnRunnerCreated := RunnerCreatedEvent;
  GUI.StartWorking := WorkingAnimator.Start;
  GUI.StopWorking := WorkingAnimator.Stop;

  ShowInfo;

  PageControl1.ActivePage := tsProducts;
  ActiveControl := lvProducts;

{$IFDEF DEBUG}
  var ActionScript := GetAutomationScript;
  if ActionScript <> '' then
  begin
    TGUIAutomator.LoadActions(ActionScript, GetAutomationLog);
  end;
{$ENDIF}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  GUI.Free;
  WorkingAnimator.Free;
  InstallAnimator.Free;
  UninstallAnimator.Free;
  BuildAnimator.Free;

  // at this point no tms.exe should be running anymore
  if Relaunch then
  begin
    var tmsgui := ParamStr(0);
    ShellExecute(0, 'open', PWideChar(tmsgui), '-no-self-update', '', SW_SHOWNORMAL);
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Closing cancels the running job, which kills tms.exe wherever it is, so ask first.
  // Not when relaunching after a self-update: that job is the one closing us.
  if Relaunch or not GUI.IsRunning then exit;
  CanClose := TDialogForm.MessageDlg('An operation is still running. Closing will cancel it.' + sLineBreak + sLineBreak +
    'Close anyway?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0, TMsgDlgBtn.mbNo) = mrYes;
end;

function TMainForm.GetLastError: string;
begin
  if GUI.LogItems.Count = 0 then exit('Unknown error');
  exit(GUI.LogItems.Last.Output);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if not GUI.Info.Initialized then
  begin
    TDialogForm.ShowMessage('Error initializing SmartSetup: ' + GetLastError);
   // Application.Terminate;
    Gui.Start;
    exit;
  end;

  //Show the message only if there is no config file yet.
  if not TFile.Exists(GUI.Info.ConfigFile) then
  begin
    if not GUI.Info.IsValidTMSSetupFolder then
    begin
      var Response := TDialogForm.MessageDlg('The folder "' + GUI.Info.WorkingFolder + '" isn''t empty or recognized as a valid smartsetup folder. Are you sure you want to use this folder for installing components?',
                         mtInformation,
                         [mbYes, mbNo],
                         0,
                         mbNo,
                         ['&Use anyway', 'E&xit application']);

      if Response <> mrYes then
      begin
        Application.Terminate;
        exit;
      end;
    end;


    var TMS, Community: Boolean;
    if TStartForm.Execute(TMS, Community) then
    begin
      // Initialize the folders only if one of servers were chosen
      if TMS or Community then
      begin
        GUI.ExecuteConfigure(True);
        GUI.EnableServerConfigItem('tms', TMS);
        GUI.EnableServerConfigItem('community', Community);
      end;
    end;
  end;

  if TFile.Exists(GUI.Info.ConfigFile) then
    GUI.Start
  else
    Application.Terminate;
end;

procedure TMainForm.GetSelectedProductsEvent(Products: TGUIProductList);
begin
  Products.Clear;
  for var Item in lvProducts.Items do
    if Item.Selected then
      Products.Add(ProductFromItem(Item));
end;

function TMainForm.GetVersionHistoryUrl(Product: TGUIProduct): string;
const
  // Let's just hard code specific version history pages for now, until when (if) Smart Setup has a more
  // generic mechanism for that.
  Urls: array[0..31] of string = (
    'tms.biz.aurelius', 'https://doc.tmssoftware.com/biz/aurelius/about/whats-new.html',
    'tms.biz.bcl', '',
    'tms.biz.echo', 'https://doc.tmssoftware.com/biz/echo/about/whats-new.html',
    'tms.biz.logging', 'https://doc.tmssoftware.com/biz/logging/about/whats-new.html',
    'tms.biz.remotedb', 'https://doc.tmssoftware.com/biz/remotedb/about/whats-new.html',
    'tms.biz.scripter', 'https://doc.tmssoftware.com/biz/scripter/about/whatsnew.html',
    'tms.biz.sparkle', 'https://doc.tmssoftware.com/biz/sparkle/about/whats-new.html',
    'tms.biz.sphinx', 'https://doc.tmssoftware.com/biz/sphinx/guide/release-notes.html',
    'tms.biz.xdata', 'https://doc.tmssoftware.com/biz/xdata/about/whats-new.html',
    'tms.flexcel.vcl', 'https://doc.tmssoftware.com/flexcel/vcl/about/whatsnew.html',
    'tms.fnc.excel.bridge', 'https://doc.tmssoftware.com/grid-excel-bridge/fnc/about/whatsnew.html',
    'tms.graphql', 'https://graphql-delphi.com/guide/release-notes.html',
    'tms.vcl.diagram', 'https://doc.tmssoftware.com/biz/diagram/about/whats-new.html',
    'tms.vcl.excel.bridge', 'https://doc.tmssoftware.com/grid-excel-bridge/vcl/about/whatsnew.html',
    'tms.vcl.query', 'https://doc.tmssoftware.com/biz/query/about/whats-new.html',
    'tms.vcl.workflow', 'https://doc.tmssoftware.com/biz/workflow/about/whats-new.html'
  );
begin
  if Product = nil then Exit('');

  var I := 0;
  while I < Length(Urls) - 1 do
  begin
    if SameText(Urls[I], Product.Id) then
      Exit(Urls[I + 1]);
    Inc(I, 2);
  end;

  if Product.VendorId <> '' then
    Exit(Format('https://www.tmssoftware.com/site/%s.asp?s=history', [Product.VendorId]));

  Result := '';
end;

procedure TMainForm.InitiateAction;
begin
  inherited;
  acCredentials.Visible := GUI.Servers.IsEnabled('tms');
  // Changing the server reloads the product list, which can't run while another job is running.
  cbServer.Enabled := not GUI.IsRunning;
end;

function TMainForm.GetLogIco(const Item: TGUILogItem): string;
begin
  if Item.IsCanceled then exit('🚫');

  case Item.Level of
    TLogLevel.Error: exit('❌');
    TLogLevel.Info: exit('✔');
    TLogLevel.Trace: exit('✔');
  end;
  Result :='';
end;

procedure TMainForm.lbLogBeforeDrawItem(AIndex: Integer; ACanvas: TCanvas;
  ARect: TRect; AState: TOwnerDrawState);
begin
  var RevIndex := GUI.LogItems.Count - 1 - AIndex;

  if (RevIndex < 0) or (RevIndex >= GUI.LogItems.Count) then
  begin
    lblError.Caption := 'Internal Error';
    lblTime.Caption := '';
    exit;
  end;

  lblError.Caption := GUI.LogItems[RevIndex].Text;
  lblTime.Caption := TimeToStr(GUI.LogItems[RevIndex].DateTime);
  btnOpenHTMLLog.Enabled := GUI.LogItems[RevIndex].SessionId <> ''; //To be 100% sure, we would need to do a if File.Exists here, but it would be too time consuming to put in BeforeDrawItem. So we guess. It might be that the file isn't there anymore, but then the button just won't do anything.
  if btnOpenHTMLLog.Enabled then btnOpenHTMLLog.Caption := acViewHtmlLog.Caption else btnOpenHTMLLog.Caption := '';


  lblErrorCaption.Caption := GetLogIco(GUI.LogItems[RevIndex]);
  if GUI.LogItems[RevIndex].Level = TLogLevel.Error then
  begin
    lblErrorCaption.Font.Color := TColors.Red;
    lblErrorCaption.StyleElements := lblErrorCaption.StyleElements - [TStyleElement.SeFont];
  end
  else
  begin
    lblErrorCaption.Font.Color := TColors.Black;
    lblErrorCaption.StyleElements := lblErrorCaption.StyleElements + [TStyleElement.SeFont];
  end;
end;

procedure TMainForm.LogItemGeneratedEvent(const Item: TGUILogItem);
begin
  TThread.Queue(nil, procedure
    begin
      if (LogPanel.Height <= 5) and (Item.Level = TLogLevel.Error) then
      begin
        LogPanel.Height := 300;
      end;
      lbLog.ItemCount := GUI.LogItems.Count;
      lbLog.Refresh;
    end);
end;

procedure TMainForm.lvProductsColumnClick(Sender: TObject; Column: TListColumn);
begin
  if SortColumn = Column.Index then
    SortDescending := not SortDescending
  else
  begin
    SortColumn := Column.Index;
    SortDescending := False;
  end;
  SortProducts;
end;

procedure TMainForm.lvProductsCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
var
  Product1, Product2: TGUIProduct;
begin
  Product1 := ProductFromItem(Item1);
  Product2 := ProductFromItem(Item2);
  if (Product1 = nil) and (Product2 = nil) then
    Compare := 0
  else
  if Product1 = nil then
    Compare := -1
  else
  if Product2 = nil then
    Compare := 1
  else
  begin
    // Review this in case we allow columns to be moved
    case SortColumn of
      ColIcons: Compare := CompareIcons(Product1, Product2);
      ColId: Compare := CompareText(Product1.Id, Product2.Id);
      ColName: Compare := CompareText(Product1.Name, Product2.Name);
      ColLocalVersion: Compare := CompareVersion(Product1.LocalVersion, Product2.LocalVersion);
      ColRemoteVersion: Compare := CompareVersion(Product1.RemoteVersion, Product2.RemoteVersion);
      ColStatus: Compare := CompareStatus(Product1.Status, Product2.Status);
    end;

    // add produt id as secondary comparison
    if Compare = 0 then
      Compare := CompareText(Product1.Id, Product2.Id);
  end;

  if SortDescending then
    Compare := -Compare;
end;

procedure TMainForm.lvProductsInfoTip(Sender: TObject; Item: TListItem; var InfoTip: string);
begin
  InfoTip := '';
  var Product := ProductFromItem(Item);
  if Product = nil then Exit;
  if not OverCaption then Exit;

  if Product.IsOutdated then
    InfoTip := InfoTip + 'New version available';
  if Product.IsPinned then
  begin
    if InfoTip <> '' then InfoTip := InfoTip + sLineBreak;
    InfoTip := InfoTip + 'Pinned version';
  end;
end;

procedure TMainForm.lvProductsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  var Item := lvProducts.GetItemAt(X, Y);
  if Item = nil then
  begin
    OverCaption := False;
    Exit;
  end;

  // drLabel = rectangle for the first column (caption)
  var R := Item.DisplayRect(drLabel);
  OverCaption := PtInRect(R, Point(X, Y));
end;

procedure TMainForm.NewVersionDetectedEvent;
begin
  if (ParamCount = 0) or (ParamStr(1) <> '-no-self-update') then
  begin
    TThread.Queue(nil, procedure
      begin
        TDialogForm.ShowMessage('A new version of TMS Smart Setup is available and ready. We will process with update and relaunch.');
        GUI.ExecuteSelfUpdate(ProgressEvent,
          procedure
          begin
            Relaunch := True;
            TThread.Synchronize(nil, Application.MainForm.Close); //must be called from the main thread.
          end);
      end);
  end;
end;

function TMainForm.GetCmdLineParameter(const Prefix: string): string;
begin
  Result := '';
  // Quick and dirty parsing of a cmd line parameter. If we add more parameters later, we can improve the parsing, not needed now.
  for var I := 1 to ParamCount do
    if ParamStr(I).StartsWith(Prefix, True) then
    begin
      exit(ParamStr(I).Substring(Length(Prefix)).Trim);
    end;
end;

function TMainForm.Repository: string;
begin
  Result := GetCmdLineParameter('-repo:');
end;

{$IFDEF DEBUG}
function TMainForm.GetAutomationScript: string;
begin
  Result := GetCmdLineParameter('-automation-script:');
end;

function TMainForm.GetAutomationLog: string;
begin
  Result := GetCmdLineParameter('-automation-log:');
end;
{$ENDIF}

procedure TMainForm.RequestCredentialsEvent(var Email, Code: string; var Confirm: Boolean;
  LastWasInvalid: Boolean; var DisableServer: Boolean);
begin
  Confirm := TCredentialsForm.GetCredentials(Email, Code, LastWasInvalid, DisableServer);
  if DisableServer and not Confirm then
    TDialogForm.ShowMessage('The "tms" server is now disabled. To turn it back on, go to Settings (gear icon) and select "Servers"');
end;

function TMainForm.AnimatorFor(const Command: TTmsCommand): TAnimator;
begin
  case Command of
    TTmsCommand.Build: Result := BuildAnimator; // full and partial: the partial build menu item wouldn't show it
    TTmsCommand.Install: Result := InstallAnimator;
    TTmsCommand.Uninstall: Result := UninstallAnimator;
    else Result := nil;
  end;
end;

procedure TMainForm.RunFinishEvent(const Command: TTmsCommand);
begin
  // Fired by the last job to finish, usually on a worker thread.
  TThread.Queue(nil, procedure
    begin
      ProgressBar.Position := 0;
    end);
  var Animator := AnimatorFor(Command);
  if Animator <> nil then Animator.Stop; // thread safe, it queues itself
end;

procedure TMainForm.RunnerCreatedEvent(Runner: TTmsRunner);
begin
  Runner.Repository := Repository;
end;

procedure TMainForm.RunStartEvent(const Command: TTmsCommand);
begin
  // Can be on a worker thread, see RunFinishEvent.
  var Animator := AnimatorFor(Command);
  if Animator <> nil then Animator.Start;
end;

procedure TMainForm.ServersUpdatedEvent(Servers: TServerConfigItems);
begin
  var Selected := '';
  if cbServer.ItemIndex >= 0 then
    Selected := cbServer.Items[cbServer.ItemIndex];

  cbServer.Items.BeginUpdate;
  try
    cbServer.Items.Clear;
    cbServer.Items.Add('all');
    for var Item in Servers do
      if Item.Enabled then
        cbServer.Items.Add(Item.Name);
    var Index := cbServer.Items.IndexOf(Selected);
    if Index = -1 then
      Index := 0;
    cbServer.ItemIndex := Index;
  finally
    cbServer.Items.EndUpdate;
  end;
  cbServer.Visible := cbServer.Items.Count > 2;
  lbServer.Visible := cbServer.Visible;
end;

procedure TMainForm.ShowInfo;
const
  {$I ..\..\version.inc}
begin
  StatusBar.Panels[0].Text := TMSVersion;
  StatusBar.Panels[2].Text := Repository;

  StatusBar.Panels[1].Text := GUI.Info.WorkingFolder;
end;

procedure TMainForm.SortProducts;
begin
  lvProducts.AlphaSort;
  UpdateSortArrows;
end;

procedure TMainForm.StatusBarClick(Sender: TObject);
begin
  if GUI.IsRunning then exit;
  
  WorkingFolderDialog.DefaultFolder := GetCurrentDir;
  if not WorkingFolderDialog.Execute then exit;

  SetCurrentDir(WorkingFolderDialog.FileName);
  GUI.RefreshInfo;
  GUI.Start;
  ShowInfo;
end;

procedure TMainForm.UpdateSortArrows;
var
  Header: HWND;
  Item: THDItem;
  I: Integer;
begin
  Header := ListView_GetHeader(lvProducts.Handle);
  for I := 0 to lvProducts.Columns.Count - 1 do
  begin
    FillChar(Item, SizeOf(Item), 0);
    Item.Mask := HDI_FORMAT;
    Header_GetItem(Header, I, Item);
    Item.fmt := Item.fmt and not (HDF_SORTUP or HDF_SORTDOWN);
    if I = SortColumn then
      if SortDescending then
        Item.fmt := Item.fmt or HDF_SORTDOWN
      else
        Item.fmt := Item.fmt or HDF_SORTUP;
    Header_SetItem(Header, I, Item);
  end;
end;

procedure TMainForm.WMSettingChange(var Msg: TWMSettingChange);
begin
  inherited;
  if SameText('ImmersiveColorSet', String(Msg.Section)) then
    ApplyThemeFromWindows;
end;

function TMainForm.ProductFromItem(Item: TListItem): TGUIProduct;
begin
  if (Item <> nil) and (Item.Data <> nil) and GUI.IsValidProduct(TGUIProduct(Item.Data)) then
    Result := TGUIProduct(Item.Data)
  else
    Result := nil;
end;

procedure TMainForm.ProductProgressEvent(const Info: TProductProgressInfo);
begin
  TThread.Queue(nil, procedure
    begin
      ProgressBar.Position := Info.Percent;
      if Info.ProductId <> '' then
      begin
        TThread.Queue(nil, procedure
          begin
            var ListItem := FindProductItem(Info.ProductId);
            if (ListItem <> nil) and (ListItem.SubItems.Count > SubItemIndex(ColStatus)) then
              ListItem.SubItems[SubItemIndex(ColStatus)] := Format('%d%%', [Info.ProductPercent])
          end);
      end;
    end);
end;

procedure TMainForm.ProductsUpdatedEvent(Products: TGUIProductList);
const
  PinnedChar = #$D83D#$DCCC;
  OutdatedChar = #$D83D#$DD52;
  StatusToStr: array[TProductStatus] of string = ('not installed', 'available', 'installed');
begin
  lvProducts.Items.BeginUpdate;
  try
    lvProducts.Items.Clear;
    for var Product in Products do
    begin
      var Item := lvProducts.Items.Add;

      var Icons := '';
      if Product.IsOutdated then
        Icons := Icons + OutdatedChar;
      if Product.IsPinned then
        Icons := Icons + PinnedChar;
      Item.Caption := Icons;  // ColIcons

      for var i := 1 to ColLast do
        Item.SubItems.Add('');
      Item.SubItems[SubItemIndex(ColId)] := Product.Id;
      Item.SubItems[SubItemIndex(ColName)] := Product.Name;
      Item.SubItems[SubItemIndex(ColLocalVersion)] := Product.LocalVersion.ToString;
      Item.SubItems[SubItemIndex(ColRemoteVersion)] := Product.RemoteVersion.ToString;
      Item.SubItems[SubItemIndex(ColStatus)] := StatusToStr[Product.Status];
      Item.Data := Product;
    end;
    SortProducts;
  finally
    lvProducts.Items.EndUpdate;
  end;
end;

procedure TMainForm.ProgressEvent(const Percent: Integer);
begin
  TThread.Queue(nil, procedure
    begin
      ProgressBar.Position := Percent;
    end);
end;

{$IFDEF DEBUG}
//for testing
function TMainForm.ProductCount: integer;
begin
  Result := lvProducts.Items.Count;
end;
{$ENDIF}


end.

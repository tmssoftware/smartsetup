unit Forms.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.UITypes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, UProductInfo, Deget.Version,
  GUI.Environment, Forms.Credentials, System.Actions, Vcl.ActnList, Vcl.Buttons, Vcl.Menus;

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
    Panel1: TPanel;
    rbAll: TRadioButton;
    rbInstalled: TRadioButton;
    lvProducts: TListView;
    Panel2: TPanel;
    Button1: TButton;
    btCredentials: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    TabSheet1: TTabSheet;
    OutputMemo: TMemo;
    mmLogDetails: TMemo;
    LogPanel: TPanel;
    ProgressPanel: TPanel;
    SpeedButton1: TSpeedButton;
    ProgressBar: TProgressBar;
    StatusBar: TStatusBar;
    lbLogItems: TListBox;
    LogSplitter: TSplitter;
    acVersionHistory: TAction;
    pmProducts: TPopupMenu;
    Openversionhistory1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
    procedure lvProductsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure lbLogItemsClick(Sender: TObject);
    procedure acVersionHistoryUpdate(Sender: TObject);
    procedure acVersionHistoryExecute(Sender: TObject);
    procedure lvProductsCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure lvProductsColumnClick(Sender: TObject; Column: TListColumn);
  private
    GUI: TGUIEnvironment;
    Relaunch: Boolean;
    SortColumn: Integer;
    SortDescending: Boolean;
    function CompareStatus(const Status1, Status2: TProductStatus): Integer;
    function CompareVersion(const Version1, Version2: TVersion): Integer;
    function FormatLogMessage(const Item: TGUILogItem): string;
    function FindProductItem(const ProductId: string): TListItem;
    procedure ShowInfo;
    function ProductFromItem(Item: TListItem): TGUIProduct;
    function GetVersionHistoryUrl(Product: TGUIProduct): string;
    procedure SortProducts;
    procedure UpdateSortArrows;
    function Repository: string;
  public
    procedure ProductsUpdatedEvent(Products: TGUIProductList);
    procedure GetSelectedProductsEvent(Products: TGUIProductList);
    procedure RequestCredentialsEvent(var Email, Code: string; var Confirm: Boolean; LastWasInvalid: Boolean);
    procedure LogItemGeneratedEvent(const Item: TGUILogItem);
    procedure CommandOutputEvent(const Text: string);
    procedure ProgressEvent(const Percent: Integer);
    procedure ProductProgressEvent(const Info: TProductProgressInfo);
    procedure RunnerCreatedEvent(Runner: TTmsRunner);
    procedure RunStartEvent;
    procedure RunFinishEvent;
    procedure NewVersionDetectedEvent;
  end;

var
  MainForm: TMainForm;

implementation

uses
  Winapi.ShellAPI, Winapi.CommCtrl;

{$R *.dfm}

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

procedure TMainForm.acPartialBuildExecute(Sender: TObject);
begin
  GUI.ExecutePartialBuild(ProductProgressEvent);
end;

procedure TMainForm.acPartialBuildUpdate(Sender: TObject);
begin
  acPartialBuild.Enabled := GUI.CanBuild;
end;

procedure TMainForm.acUninstallExecute(Sender: TObject);
begin
  GUI.ExecuteUninstall(ProgressEvent);
end;

procedure TMainForm.acUninstallUpdate(Sender: TObject);
begin
  acUninstall.Enabled := GUI.CanUninstallSelected;
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
  acVersionHistory.Enabled := GetVersionHistoryUrl(Product) <> '';
end;

procedure TMainForm.CommandOutputEvent(const Text: string);
begin
  TThread.Queue(nil, procedure
    begin
      OutputMemo.Lines.Add(Text);
      SendMessage(OutputMemo.Handle, EM_LINESCROLL, 0, OutputMemo.Lines.Count);
    end);
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

function TMainForm.CompareVersion(const Version1, Version2: TVersion): Integer;
begin
  if Version1 < Version2 then
    Result := -1
  else
  if Version1 > Version2 then
    Result := 1
  else
    Result := 0;
end;

function TMainForm.FindProductItem(const ProductId: string): TListItem;
begin
  for var Item in lvProducts.Items do
    if (Item.Data <> nil) and SameText(TGUIProduct(Item.Data).Id, ProductId) then
      Exit(Item);
  Result := nil;
end;

function TMainForm.FormatLogMessage(const Item: TGUILogItem): string;
const
  LogLevelStr: array[TLogLevel] of string = ('TRACE', 'INFO ', 'ERROR');
begin
  Result := Format('[%s] %s', [LogLevelStr[Item.Level], Item.Text]);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SortColumn := 4;  // sort by status by default

  GUI := TGUIEnvironment.Create;

  GUI.OnProductsUpdated := ProductsUpdatedEvent;
  GUI.OnRequestCredentials := RequestCredentialsEvent;
  GUI.OnGetSelectedProducts := GetSelectedProductsEvent;
  GUI.OnLogItemGenerated := LogItemGeneratedEvent;
  GUI.OnCommandOutput := CommandOutputEvent;
  GUI.OnRunStart := RunStartEvent;
  GUI.OnRunFinish := RunFinishEvent;
  GUI.OnNewVersionDetected := NewVersionDetectedEvent;
  GUI.OnRunnerCreated := RunnerCreatedEvent;

  ShowInfo;

  PageControl1.ActivePage := tsProducts;
  ActiveControl := lvProducts;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  GUI.Free;

  // at this point no tms.exe should be running anymore
  if Relaunch then
  begin
    var tmsgui := ParamStr(0);
    ShellExecute(0, 'open', PWideChar(tmsgui), '-no-self-update', '', SW_SHOWNORMAL);
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  GUI.Start;
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

procedure TMainForm.lbLogItemsClick(Sender: TObject);
begin
  var Line := lbLogItems.ItemIndex;
  var Item: TGUILogItem := nil;
  if Line >= 0 then
    Item := TGUILogItem(lbLogItems.Items.Objects[Line]);

  var Details: string := '';
  if Item <> nil then
    Details := Item.Output;
  mmLogDetails.Visible := Trim(Details) <> '';
  mmLogDetails.Lines.Text := Details;
  LogSplitter.Visible := mmLogDetails.Visible;
end;

procedure TMainForm.LogItemGeneratedEvent(const Item: TGUILogItem);
begin
  var Text := FormatLogMessage(Item);
  TThread.Queue(nil, procedure
    begin
      if not LogPanel.Visible then
        LogPanel.Visible := True;
      lbLogItems.AddItem(Text, Item);
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
  I: Integer;
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
      0: Compare := CompareText(Product1.Id, Product2.Id);
      1: Compare := CompareText(Product1.Name, Product2.Name);
      2: Compare := CompareVersion(Product1.LocalVersion, Product2.LocalVersion);
      3: Compare := CompareVersion(Product1.RemoteVersion, Product2.RemoteVersion);
      4: Compare := CompareStatus(Product1.Status, Product2.Status);
    end;

    // add produt id as secondary comparison
    if Compare = 0 then
      Compare := CompareText(Product1.Id, Product2.Id);
  end;

  if SortDescending then
    Compare := -Compare;
end;

procedure TMainForm.lvProductsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  var Product := ProductFromItem(Item);
  if Product.Status <> TProductStatus.Installed then
    Sender.Canvas.Font.Style := Sender.Canvas.Font.Style + [fsItalic];
  if Product.IsOutdated then
    Sender.Canvas.Font.Style := Sender.Canvas.Font.Style + [fsBold];
end;

procedure TMainForm.NewVersionDetectedEvent;
begin
  if GUI.Info.HasCredentials then
  begin
    if (ParamCount = 0) or (ParamStr(1) <> '-no-self-update') then
    begin
      TThread.Queue(nil, procedure
        begin
          ShowMessage('A new version of TMS Smart Setup is available and ready. We will process with update and relaunch.');
          GUI.ExecuteSelfUpdate(ProgressEvent,
            procedure
            begin
              Relaunch := True;
              TThread.Synchronize(nil, Application.MainForm.Close); //must be called from the main thread.
            end);
        end);
    end;
  end;
end;

function TMainForm.Repository: string;
const
  RepoPrefix = '-repo:';
begin
  // Quick and dirty parsing of -repo parameter. If we add more parameters later, we can improve the parsing, not needed now.
  for var I := 1 to ParamCount do
    if ParamStr(I).StartsWith(RepoPrefix, True) then
    begin
      Result := Copy(ParamStr(I), Length(RepoPrefix) + 1);
    end;
end;

procedure TMainForm.RequestCredentialsEvent(var Email, Code: string; var Confirm: Boolean; LastWasInvalid: Boolean);
begin
  Confirm := TCredentialsForm.GetCredentials(Email, Code, LastWasInvalid);
end;

procedure TMainForm.RunFinishEvent;
begin
  ProgressBar.Position := 0;
end;

procedure TMainForm.RunnerCreatedEvent(Runner: TTmsRunner);
begin
  Runner.Repository := Repository;
end;

procedure TMainForm.RunStartEvent;
begin
end;

procedure TMainForm.ShowInfo;
const
  {$I ..\..\version.inc}
begin
  StatusBar.Panels[0].Text := TMSVersion;
  StatusBar.Panels[1].Text := GUI.Info.WorkingFolder;
  if (Repository <> '') then
  begin
    if StatusBar.Panels.Count < 3 then
      StatusBar.Panels.Add;
    StatusBar.Panels[2].Text := Repository;
  end;
end;

procedure TMainForm.SortProducts;
begin
  lvProducts.AlphaSort;
  UpdateSortArrows;
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

function TMainForm.ProductFromItem(Item: TListItem): TGUIProduct;
begin
  if (Item <> nil) and (Item.Data <> nil) then
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
            if (ListItem <> nil) and (ListItem.SubItems.Count > 3) then
              ListItem.Subitems[3] := Format('%d%%', [Info.ProductPercent])
          end);
      end;
    end);
end;

procedure TMainForm.ProductsUpdatedEvent(Products: TGUIProductList);
const
  StatusToStr: array[TProductStatus] of string = ('not installed', 'available', 'installed');
begin
  lvProducts.Items.BeginUpdate;
  try
    lvProducts.Items.Clear;
    for var Product in Products do
    begin
      var Item := lvProducts.Items.Add;
      Item.Caption := Product.Id;
      Item.SubItems.Add(Product.Name);
      Item.SubItems.Add(Product.LocalVersion.ToString);
      Item.SubItems.Add(Product.RemoteVersion.ToString);
      Item.SubItems.Add(StatusToStr[Product.Status]);
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

end.

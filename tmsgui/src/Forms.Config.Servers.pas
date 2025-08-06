unit Forms.Config.Servers;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Mask,
  Vcl.ExtCtrls, UConfigInfo;

type
  TServerConfigForm = class(TForm)
    lbServerList: TCheckListBox;
    btServerAdd: TButton;
    btServerRemove: TButton;
    pnServerDetails: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edServerName: TEdit;
    edServerUrl: TEdit;
    cbServerType: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure lbServerListClick(Sender: TObject);
    procedure lbServerListClickCheck(Sender: TObject);
    procedure edServerNameChange(Sender: TObject);
    procedure edServerUrlChange(Sender: TObject);
    procedure cbServerTypeClick(Sender: TObject);
    procedure btServerRemoveClick(Sender: TObject);
    procedure btServerAddClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FServers: TServerConfigItems;
    FModified: Boolean;
    FLoading: Integer;
    FOnModified: TProc;
    FOnFocusRequested: TProc<TWinControl>;
    procedure DoModified;
    procedure BeginLoading;
    procedure EndLoading;
    procedure SetServers(const Value: TServerConfigItems);
    procedure UpdateList;
    procedure UpdateSelected;
    procedure SelectItem(Index: Integer; FocusControl: TWinControl = nil);
    function ServerFromIndex(Index: Integer): TServerConfigItem;
    function SelectedServer: TServerConfigItem;
  public
    procedure Validate;
    procedure RaiseError(const Msg: string; Index: Integer; FocusControl: TWinControl);
    property Servers: TServerConfigItems read FServers write SetServers;
    property OnModified: TProc read FOnModified write FOnModified;
    property OnFocusRequested: TProc<TWinControl> read FOnFocusRequested write FOnFocusRequested;
    property Modified: Boolean read FModified write FModified;
  end;

implementation

{$R *.dfm}

procedure TServerConfigForm.BeginLoading;
begin
  Inc(FLoading);
end;

procedure TServerConfigForm.btServerAddClick(Sender: TObject);
begin
  var Server := TServerConfigItem.Create;
  FServers.Add(Server);
  Server.Name := 'server';
  Server.ServerType := 'zipfile';
  Server.Enabled := True;
  UpdateList;
  SelectItem(FServers.Count - 1, edServerName);
  DoModified;
end;

procedure TServerConfigForm.btServerRemoveClick(Sender: TObject);
begin
  if SelectedServer = nil then Exit;
  
  FServers.Remove(SelectedServer);
  var SelectedIndex := lbServerList.ItemIndex;
  UpdateList;
  SelectItem(SelectedIndex);
  DoModified;
end;

procedure TServerConfigForm.Button1Click(Sender: TObject);
begin
  Validate;
end;

procedure TServerConfigForm.cbServerTypeClick(Sender: TObject);
begin
  if SelectedServer = nil then Exit;
  SelectedServer.ServerType := cbServerType.Text;
  DoModified;
end;

procedure TServerConfigForm.edServerNameChange(Sender: TObject);
begin
  if SelectedServer = nil then Exit;

  SelectedServer.Name := edServerName.Text;
  lbServerList.Items[lbServerList.ItemIndex] := SelectedServer.Name;
  DoModified;
end;

procedure TServerConfigForm.edServerUrlChange(Sender: TObject);
begin
  if SelectedServer = nil then Exit;
  SelectedServer.Url := edServerUrl.Text;
  DoModified;
end;

procedure TServerConfigForm.EndLoading;
begin
  Dec(FLoading);
end;

procedure TServerConfigForm.FormCreate(Sender: TObject);
begin
  cbServerType.Items.Clear;
  cbServerType.Items.Add('');
  cbServerType.Items.Add('api');
  cbServerType.Items.Add('zipfile');
end;

procedure TServerConfigForm.lbServerListClick(Sender: TObject);
begin
  UpdateSelected;
end;

procedure TServerConfigForm.lbServerListClickCheck(Sender: TObject);
begin
  SelectedServer.Enabled := lbServerList.Checked[lbServerList.ItemIndex];
  DoModified;
end;

procedure TServerConfigForm.RaiseError(const Msg: string; Index: Integer;
  FocusControl: TWinControl);
begin
  SelectItem(Index, FocusControl);
  if Assigned(FOnFocusRequested) then
    FOnFocusRequested(Self);
  if Assigned(FocusControl) and FocusControl.CanFocus then
    FocusControl.SetFocus;

  raise Exception.Create(Msg);
end;

procedure TServerConfigForm.DoModified;
begin
  if FLoading > 0 then Exit;

  FModified := True;
  if Assigned(FOnModified) then
    FOnModified();
end;

function TServerConfigForm.SelectedServer: TServerConfigItem;
begin
  Result := ServerFromIndex(lbServerList.ItemIndex);
end;

procedure TServerConfigForm.SelectItem(Index: Integer; FocusControl: TWinControl = nil);
begin
  if Index >= lbServerList.Count then
    Index := lbServerList.Count - 1;
  lbServerList.ItemIndex := Index;
  UpdateSelected;
  if Assigned(FocusControl) and FocusControl.CanFocus then
    FocusControl.SetFocus;
end;

function TServerConfigForm.ServerFromIndex(Index: Integer): TServerConfigItem;
begin
  if Index >= 0 then
    Result := TServerConfigItem(lbServerList.Items.Objects[Index])
  else
    Result := nil;
end;

procedure TServerConfigForm.SetServers(const Value: TServerConfigItems);
begin
  FServers := Value;
  UpdateList;
  SelectItem(0);
end;

procedure TServerConfigForm.UpdateList;
begin
  BeginLoading;
  try
    lbServerList.Items.BeginUpdate;
    try
      lbServerList.Items.Clear;
      for var Item in FServers do
      begin
        lbServerList.AddItem(Item.Name, Item);
        lbServerList.Checked[lbServerList.Count - 1] := Item.Enabled;
      end;
    finally
      lbServerList.Items.EndUpdate;
    end;
  finally
    EndLoading;
  end;
end;

procedure TServerConfigForm.UpdateSelected;
begin
  BeginLoading;
  try
    var Server := SelectedServer;
    if not Assigned(Server) then
    begin
      pnServerDetails.Visible := False;
      btServerRemove.Enabled := False;
      Exit;
    end;

    edServerName.Text := Server.Name;
    edServerUrl.Text := Server.Url;
    cbServerType.ItemIndex := cbServerType.Items.IndexOf(Server.ServerType);
    if SameText(Server.ServerType, 'local') then
      cbServerType.ItemIndex := 0;

    var Reserved := Server.IsReserved;
    edServerName.Enabled := not Reserved;
    edServerUrl.Enabled := not Reserved;
    cbServerType.Enabled := not Reserved;

    pnServerDetails.Visible := not Reserved;
    btServerRemove.Enabled := not Reserved;
  finally
    EndLoading;
  end;
end;

procedure TServerConfigForm.Validate;
begin
  for var I := 0 to FServers.Count - 1 do
  begin
    var Item := FServers[I];
    if Item.IsReserved then Continue;

    if Item.Name.Trim = '' then
      RaiseError('Server name cannot be empty', I, edServerName);

    if Item.Url.Trim = '' then
      RaiseError('Url cannot be empty', I, edServerUrl);

    if (Item.ServerType = '') or SameText(Item.ServerType, 'local') then
      RaiseError('Invalid type, only zipfile and api are allowed', I, cbServerType);

    // check duplicated items
    for var J := 0 to FServers.Count - 1 do
      if (I <> J) and SameText(FServers[I].Name, FServers[J].Name) then
        RaiseError(Format('Duplicated server name: %s', [FServers[I].Name]), J, edServerName);
  end;
end;

end.

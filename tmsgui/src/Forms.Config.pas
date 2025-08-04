unit Forms.Config;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls,
  Forms.Config.Servers, UConfigInfo;

type
  TConfigForm = class(TForm)
    tvMenu: TTreeView;
    btApply: TButton;
    btCancel: TButton;
    btOk: TButton;
    pnDetails: TPanel;
    pnTitle: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure btOkClick(Sender: TObject);
    procedure tvMenuChange(Sender: TObject; Node: TTreeNode);
    procedure btApplyClick(Sender: TObject);
  private
    FConfigInfo: TConfigInfo;
    FServerConfigForm: TServerConfigForm;
    FOnUpdateServers: TProc;

    FGeneralNode: TTreeNode;
    FServersNode: TTreeNode;
    procedure ConfigModified;
    procedure FocusRequested(Control: TWinControl);
  public
    procedure ApplyChanges;
    procedure Execute(AConfigInfo: TConfigInfo);
    property OnUpdateServers: TProc read FOnUpdateServers write FOnUpdateServers;
  end;

implementation

{$R *.dfm}

procedure TConfigForm.ApplyChanges;
begin
  if FServerConfigForm.Modified then
  begin
    FServerConfigForm.Validate;
    if Assigned(FOnUpdateServers) then
      FOnUpdateServers();
    FServerConfigForm.Modified := False;
  end;

  btApply.Enabled := False;
end;

procedure TConfigForm.btApplyClick(Sender: TObject);
begin
  ApplyChanges;
end;

procedure TConfigForm.btOkClick(Sender: TObject);
begin
  ApplyChanges;
  ModalResult := mrOk;
end;

procedure TConfigForm.ConfigModified;
begin
  btApply.Enabled := True;
end;

procedure TConfigForm.Execute(AConfigInfo: TConfigInfo);
begin
  FConfigInfo := AConfigInfo;
  FServerConfigForm.Servers := FConfigInfo.Servers;
  ShowModal;
end;

procedure TConfigForm.FocusRequested(Control: TWinControl);
begin
  for var Node in tvMenu.Items do
    if Node.Data = Control then
    begin
      tvMenu.Selected := Node;
      Break;
    end;
end;

procedure TConfigForm.FormCreate(Sender: TObject);
begin
  btApply.Enabled := False;

  FServerConfigForm := TServerConfigForm.Create(Self);
  FServerConfigForm.OnModified := ConfigModified;
  FServerConfigForm.OnFocusRequested := FocusRequested;

  // build the tree view
  FGeneralNode := tvMenu.Items.AddChild(nil, 'General');
  FServersNode := tvMenu.Items.AddChildObject(FGeneralNode, 'Servers', FServerConfigForm);
  tvMenu.FullExpand;
  tvMenu.Selected := FServersNode;
end;

procedure TConfigForm.tvMenuChange(Sender: TObject; Node: TTreeNode);
begin
  // Show the form associated with the tree view node
  var Obj: TObject := nil;
  var Control: TForm := nil;
  if Assigned(tvMenu.Selected) then
  begin
    Obj := TObject(tvMenu.Selected.Data);
    pnTitle.Caption := tvMenu.Selected.Text;
  end
  else
    pnTitle.Caption := '';

  if Assigned(Obj) and (Obj is TForm) then
    Control := TForm(Obj);

  for var I := 0 to pnDetails.ControlCount - 1 do
    if pnDetails.Controls[I] <> Control then
      pnDetails.Controls[I].Visible := False;

  if Assigned(Control) then
  begin
    Control.Parent := pnDetails;
    Control.Visible := True;
    Control.BorderStyle := bsNone;
    Control.Align := alClient;
  end;
end;

end.

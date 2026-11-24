unit Forms.AddDefine;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Forms.Dialogs;

type
  // A define that a product says it understands, from "available defines" in its tmsbuild.yaml.
  TProductDefine = record
    ProductId: string;
    Name: string;
    Description: string;
  end;

  // Lets you pick one of the defines the products declare, or type any other define name.
  TAddDefineForm = class(TForm)
    lbProduct: TLabel;
    cbProduct: TComboBox;
    lbDefines: TLabel;
    lvDefines: TListView;
    lbName: TLabel;
    edName: TEdit;
    btOk: TButton;
    btCancel: TButton;
    procedure btOkClick(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure lvDefinesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvDefinesDblClick(Sender: TObject);
    procedure lvDefinesColumnClick(Sender: TObject; Column: TListColumn);
    procedure cbProductChange(Sender: TObject);
  private
    FDefines: TArray<TProductDefine>;
    FExistingDefines: TArray<string>;
    // Index of the column the list is sorted by: 0 product, 1 define, 2 description.
    FSortColumn: Integer;
    FSortAscending: Boolean;
    function GetDefineName: string;
    procedure SetDefines(const Defines: TArray<TProductDefine>);
    procedure FillProducts;
    procedure FillList;
    procedure UpdateSortArrows;
  public
    // ExistingDefines are the defines already in the list. They are not offered, and can't be added again.
    class function AskDefine(const Defines: TArray<TProductDefine>; const ExistingDefines: TArray<string>; out Name: string): Boolean;
  end;

implementation

uses
  Winapi.CommCtrl, System.Generics.Defaults, System.Generics.Collections, UVCLUtils;

{$R *.dfm}

const
  AllProductsCaption = 'All products';

function IsExisting(const ExistingDefines: TArray<string>; const Name: string): Boolean;
begin
  for var Existing in ExistingDefines do
    if SameText(Existing, Name) then exit(True);
  Result := False;
end;

function DefineColumn(const Define: TProductDefine; const Column: Integer): string;
begin
  case Column of
    0: Result := Define.ProductId;
    1: Result := Define.Name;
  else
    Result := Define.Description;
  end;
end;

{ TAddDefineForm }

class function TAddDefineForm.AskDefine(const Defines: TArray<TProductDefine>; const ExistingDefines: TArray<string>; out Name: string): Boolean;
begin
  var Form := TAddDefineForm.Create(Application);
  try
    Form.FExistingDefines := ExistingDefines;
    Form.SetDefines(Defines);

    Name := '';
    Result := Form.ShowModal = mrOk;
    if Result then
    begin
      Name := Form.GetDefineName;
    end;
  finally
    Form.Free;
  end;
end;

procedure TAddDefineForm.SetDefines(const Defines: TArray<TProductDefine>);
begin
  FDefines := nil;
  for var Define in Defines do
    if not IsExisting(FExistingDefines, Define.Name) then FDefines := FDefines + [Define];

  FSortColumn := 0;
  FSortAscending := True;
  FillProducts;
  FillList;
end;

procedure TAddDefineForm.FillProducts;
begin
  var Products := TStringList.Create;
  try
    Products.Sorted := True;
    Products.Duplicates := dupIgnore;
    Products.CaseSensitive := False;
    for var Define in FDefines do Products.Add(Define.ProductId);

    cbProduct.Items.BeginUpdate;
    try
      cbProduct.Items.Clear;
      cbProduct.Items.Add(AllProductsCaption);
      cbProduct.Items.AddStrings(Products);
    finally
      cbProduct.Items.EndUpdate;
    end;
    cbProduct.ItemIndex := 0;
  finally
    Products.Free;
  end;
end;

procedure TAddDefineForm.FillList;
begin
  // Index 0 is "All products"; the others are the product ids.
  var ProductFilter := '';
  if cbProduct.ItemIndex > 0 then ProductFilter := cbProduct.Items[cbProduct.ItemIndex];

  var Shown: TArray<TProductDefine> := nil;
  for var Define in FDefines do
    if (ProductFilter = '') or SameText(Define.ProductId, ProductFilter) then Shown := Shown + [Define];

  var SortColumn := FSortColumn;
  var SortAscending := FSortAscending;
  TArray.Sort<TProductDefine>(Shown, TComparer<TProductDefine>.Construct(
    function(const Left, Right: TProductDefine): Integer
    begin
      Result := CompareText(DefineColumn(Left, SortColumn), DefineColumn(Right, SortColumn));
      // Ties are ordered by product and define, so rows with the same value don't jump around.
      if Result = 0 then Result := CompareText(Left.ProductId, Right.ProductId);
      if Result = 0 then Result := CompareText(Left.Name, Right.Name);
      if not SortAscending then Result := -Result;
    end));

  lvDefines.Items.BeginUpdate;
  try
    lvDefines.Items.Clear;
    for var Define in Shown do
    begin
      var Item := lvDefines.Items.Add;
      Item.Caption := Define.ProductId;
      Item.SubItems.Add(Define.Name);
      Item.SubItems.Add(Define.Description);
    end;
  finally
    lvDefines.Items.EndUpdate;
  end;
  UpdateSortArrows;
end;

procedure TAddDefineForm.UpdateSortArrows;
begin
  var Header := ListView_GetHeader(lvDefines.Handle);
  if Header = 0 then exit;

  for var i := 0 to lvDefines.Columns.Count - 1 do
  begin
    var HeaderItem: THDItem;
    FillChar(HeaderItem, SizeOf(HeaderItem), 0);
    HeaderItem.Mask := HDI_FORMAT;
    if not Header_GetItem(Header, i, HeaderItem) then continue;

    HeaderItem.fmt := HeaderItem.fmt and not (HDF_SORTUP or HDF_SORTDOWN);
    if i = FSortColumn then
    begin
      if FSortAscending then HeaderItem.fmt := HeaderItem.fmt or HDF_SORTUP
      else HeaderItem.fmt := HeaderItem.fmt or HDF_SORTDOWN;
    end;
    Header_SetItem(Header, i, HeaderItem);
  end;
end;

procedure TAddDefineForm.lvDefinesColumnClick(Sender: TObject; Column: TListColumn);
begin
  // Clicking the column the list is already sorted by reverses the order.
  if Column.Index = FSortColumn then FSortAscending := not FSortAscending
  else
  begin
    FSortColumn := Column.Index;
    FSortAscending := True;
  end;
  FillList;
end;

procedure TAddDefineForm.cbProductChange(Sender: TObject);
begin
  FillList;
end;

function TAddDefineForm.GetDefineName: string;
begin
  Result := CaptionToString(edName.Text).Trim;
end;

procedure TAddDefineForm.lvDefinesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected and (Item <> nil) then
    edName.Text := Item.SubItems[0];
end;

procedure TAddDefineForm.lvDefinesDblClick(Sender: TObject);
begin
  if lvDefines.Selected = nil then exit;
  edName.Text := lvDefines.Selected.SubItems[0];
  btOk.Click;
end;

procedure TAddDefineForm.edNameChange(Sender: TObject);
begin
  btOk.Enabled := GetDefineName <> '';
end;

procedure TAddDefineForm.btOkClick(Sender: TObject);
begin
  var Name := GetDefineName;
  if Name = '' then
  begin
    TDialogForm.ShowMessage('The define name cannot be empty.');
    exit;
  end;
  if not IsValidIdent(Name) then
  begin
    TDialogForm.ShowMessage(Format('"%s" is not a valid define name.', [Name]));
    exit;
  end;
  if IsExisting(FExistingDefines, Name) then
  begin
    TDialogForm.ShowMessage(Format('The define "%s" is already in the list.', [Name]));
    exit;
  end;

  ModalResult := mrOk;
end;

end.

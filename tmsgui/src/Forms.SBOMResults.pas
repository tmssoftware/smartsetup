unit Forms.SBOMResults;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TSBOMResult = record
    ProductId: string;
    ProductName: string;
    FileName: string;
    Generated: Boolean;
    Error: string; // not empty if generating it failed. Generated is false then, so it can be regenerated.
  end;

  TSBOMResultsForm = class(TForm)
    lbCaption: TLabel;
    lvResults: TListView;
    Panel1: TPanel;
    btRegenerate: TButton;
    btOpenFolder: TButton;
    btClose: TButton;
    procedure btOpenFolderClick(Sender: TObject);
    procedure lvResultsDblClick(Sender: TObject);
    procedure lvResultsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    procedure FillResults(const Results: TArray<TSBOMResult>);
    procedure OpenSelectedFolder;
  protected
    function DoExecute(const Results: TArray<TSBOMResult>): Boolean;
  public
    /// <summary>Returns true if the user asked to regenerate the skipped and failed SBOMs.</summary>
    class function Execute(const Results: TArray<TSBOMResult>): Boolean;
  end;

implementation

uses
  Winapi.ShellAPI, System.Generics.Collections, System.Generics.Defaults;

{$R *.dfm}

procedure TSBOMResultsForm.btOpenFolderClick(Sender: TObject);
begin
  OpenSelectedFolder;
end;

function TSBOMResultsForm.DoExecute(const Results: TArray<TSBOMResult>): Boolean;
begin
  FillResults(Results);
  btOpenFolder.Enabled := False;
  Result := ShowModal = mrRetry;
end;

class function TSBOMResultsForm.Execute(const Results: TArray<TSBOMResult>): Boolean;
begin
  var Form := TSBOMResultsForm.Create(Application);
  try
    Result := Form.DoExecute(Results);
  finally
    Form.Free;
  end;
end;

function Rank(const R: TSBOMResult): Integer;
begin
  if R.Error <> '' then Result := 0
  else if R.Generated then Result := 1
  else Result := 2;
end;

procedure TSBOMResultsForm.FillResults(const Results: TArray<TSBOMResult>);
begin
  // Failed first, so they are seen, then generated, then skipped. By name inside each group.
  var Sorted := Copy(Results);
  TArray.Sort<TSBOMResult>(Sorted, TComparer<TSBOMResult>.Construct(
    function(const Left, Right: TSBOMResult): Integer
    begin
      Result := Rank(Left) - Rank(Right);
      if Result = 0 then
        Result := CompareText(Left.ProductName, Right.ProductName);
    end));

  var GeneratedCount := 0;
  var FailedCount := 0;
  lvResults.Items.BeginUpdate;
  try
    lvResults.Items.Clear;
    for var R in Sorted do
    begin
      var Item := lvResults.Items.Add;
      Item.Caption := R.ProductName;
      Item.SubItems.Add(R.ProductId);
      if R.Error <> '' then
      begin
        Item.SubItems.Add('Failed: ' + R.Error);
        Inc(FailedCount);
      end
      else if R.Generated then
      begin
        Item.SubItems.Add('Generated');
        Inc(GeneratedCount);
      end
      else
        Item.SubItems.Add('Skipped');
      Item.SubItems.Add(R.FileName);
    end;
  finally
    lvResults.Items.EndUpdate;
  end;

  lbCaption.Caption := Format('SBOM files generated: %d. Skipped (already existed): %d.',
    [GeneratedCount, Length(Results) - GeneratedCount - FailedCount]);
  if FailedCount > 0 then
    lbCaption.Caption := lbCaption.Caption + Format(' Failed: %d.', [FailedCount]);
  btRegenerate.Enabled := GeneratedCount < Length(Results);
end;

procedure TSBOMResultsForm.lvResultsDblClick(Sender: TObject);
begin
  OpenSelectedFolder;
end;

procedure TSBOMResultsForm.lvResultsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  btOpenFolder.Enabled := (lvResults.Selected <> nil) and (lvResults.Selected.SubItems[2] <> '');
end;

procedure TSBOMResultsForm.OpenSelectedFolder;
begin
  var Item := lvResults.Selected;
  if Item = nil then
    Exit;
  var FileName := Item.SubItems[2];
  if FileName = '' then
    Exit;

  if FileExists(FileName) then
    ShellExecute(Handle, 'open', 'explorer.exe', PChar('/select,"' + FileName + '"'), nil, SW_SHOWNORMAL)
  else if DirectoryExists(ExtractFileDir(FileName)) then
    ShellExecute(Handle, 'open', PChar(ExtractFileDir(FileName)), nil, nil, SW_SHOWNORMAL);
end;

end.

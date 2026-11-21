unit Commands.BackupInfo;
// Shows what a backup made with "tms backup" contains.

interface
uses
  System.Classes, System.SysUtils, System.Generics.Collections, UCommandLine;

procedure RegisterBackupInfoCommand;

implementation
uses
  System.IOUtils, System.Generics.Defaults, JSON, Commands.CommonOptions, Commands.Logging, UMultiLogger,
  Status.Manager, Snapshot.Backup, UJsonPrinter;

var OptionFileName: string;

function IsInFile(const Backup: TBackupData; const Product: TProductStatus): boolean;
begin
  var Index: integer;
  Result := Backup.ProductInBackupList(Product.Id, Product.Version, Index);
end;

procedure OutputJsonInfo(const FileName: string; const Backup: TBackupData);
begin
  var Root := TJSONObject.Create;
  try
    Root.AddPair('filename', FileName);
    Root.AddPair('has_snapshot', Backup.HasSnapshot);
    var JsonProducts := TJSONObject.Create;
    Root.AddPair('products', JsonProducts);
    for var Product in Backup.Products do
    begin
      var Item := TJSONObject.Create;
      JsonProducts.AddPair(Product.Id, Item);
      Item.AddPair('version', Product.Version.ToString);
      Item.AddPair('server', Product.Server);
      Item.AddPair('pinned', Product.Pinned);
      // false: restore gets it from its server, or doesn't restore it at all if it is local.
      Item.AddPair('in_file', IsInFile(Backup, Product));
    end;
    OutputJson(Root);
  finally
    Root.Free;
  end;
end;

procedure OutputTextInfo(const FileName: string; const Backup: TBackupData);
begin
  WriteLn('Backup file: ' + FileName);
  if Backup.Products.Count = 0 then
  begin
    WriteLn('It contains no products.');
    exit;
  end;

  WriteLn(Format('Products (%d):', [Backup.Products.Count]));
  for var Product in Backup.Products do
  begin
    var Where: string;
    if IsInFile(Backup, Product) then Where := 'in the backup file'
    else if Product.Server = '' then Where := 'local, not restored'
    else Where := 'restored from server "' + Product.Server + '"';
    WriteLn(Format('  %s %s (%s)', [Product.Id, Product.Version.ToString, Where]));
  end;
end;

procedure RunBackupInfoCommand;
begin
  InitFolderBasedCommand(false);
  if OptionFileName.Trim = '' then raise Exception.Create('Please specify the backup file.');
  var FileName := TPath.GetFullPath(OptionFileName.Trim);
  if not TFile.Exists(FileName) then raise Exception.Create('The file "' + FileName + '" doesn''t exist.');

  var Backup := TBackupData.Create;
  try
    LoadBackup(FileName, Backup);
    Backup.Products.Sort(TComparer<TProductStatus>.Construct(
      function(const P1, P2: TProductStatus): Integer
      begin
        Result := CompareStr(P1.Id, P2.Id);
      end));

    if Logger.JsonMode then OutputJsonInfo(FileName, Backup)
    else OutputTextInfo(FileName, Backup);
  finally
    Backup.Free;
  end;
end;

procedure RegisterBackupInfoCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('backup-info', '', 'show the products and versions inside a backup file created with tms backup.',
    'More information: https://doc.tmssoftware.com/smartsetup/reference/tms-backup-info.html',
    'backup-info <filename> [-json]');

  cmd.Examples.Add('backup-info c:\test\backup.snapshot.zip');
  cmd.Examples.Add('backup-info c:\test\backup.snapshot.zip -json');

  var option := cmd.RegisterUnnamedOption<string>('The backup file (.zip), or a snapshot file (.yaml)', 'filename',
    procedure(const Value : string)
    begin
      OptionFileName := Value;
    end);
  option.Required := True;

  RegisterJsonOption(cmd);

  AddCommand(cmd.Name, CommandGroups.Status, RunBackupInfoCommand);
end;

end.

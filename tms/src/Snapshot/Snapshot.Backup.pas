unit Snapshot.Backup;
// Reads what a backup made by "tms backup" (or a plain snapshot) contains.
// Shared by "tms restore" and "tms backup-info", so both always agree on what is in a file.

interface
uses
  System.Classes, System.SysUtils, System.Generics.Collections, Status.Manager, Deget.Version;

type
  TBackupData = class
  public
    HasSnapshot: boolean;
    ZipFile: string; // empty if the file is a snapshot, not a zip.
    Products: TObjectList<TProductStatus>;
    NameToIndexMap: TDictionary<string, integer>;
    NameToProductMap: TDictionary<string, TProductStatus>;
    constructor Create;
    destructor Destroy; override;

    function ProductInBackupList(const ProductId: string; const ProductVersion: TLenientVersion; out BackupIndex: integer): boolean;
  end;

/// <summary>Loads the products in FileName, a backup zip or a snapshot (.yaml) file.
/// If the zip has no snapshot inside, one is made up from the files in the zip.</summary>
procedure LoadBackup(const FileName: string; const Backup: TBackupData);

implementation
uses
  System.IOUtils, System.Zip, Snapshot.Reader, Downloads.FileNameManager, UGenericDecompressor, Deget.CoreTypes;

procedure LoadSnapshotFromZip(const zip: TZipFile; const idx: integer; const Products: TList<TProductStatus>; const DisplayFileName: string);
begin
  var SnapshotBytes: TBytes;
  zip.Read(idx, SnapshotBytes);
  if SnapshotBytes <> nil then
  begin
    var SnapshotStream := TMemoryStream.Create;
    try
      SnapshotStream.WriteBuffer(SnapshotBytes[0], Length(SnapshotBytes));
      SnapshotStream.Position := 0;
      var SnapshotStreamReader := TStreamReader.Create(SnapshotStream, TEncoding.UTF8);
      try
        LoadSnapshot(SnapshotStreamReader, DisplayFileName, Products);
      finally
        SnapshotStreamReader.Free;
      end;
    finally
      SnapshotStream.Free;
    end;
  end;

end;

procedure LoadProductsFromZip(const zip: TZipFile; const Backup: TBackupData);
begin
  for var i := 0 to zip.FileCount - 1 do
  begin
    var NameOnly: string;
    if TDownloadFileName.TryGetFileNameWithoutExtension(TPath.GetFileName(zip.FileName[i]), NameOnly) then
    begin
      var ProductId: string;
      var ProductVersion:TLenientVersion;
      if TDownloadFileName.TryParse(NameOnly + '.zip', ProductId, ProductVersion) and not (ProductId.StartsWith('tms.smartsetup.', true)) then
      begin
        var BackupFileName := TDownloadFileName.GenerateFileName(ProductId, ProductVersion);
        if (not Backup.NameToIndexMap.ContainsKey(BackupFileName)) then
        begin
          Backup.NameToIndexMap.Add(BackupFileName, i);
        end;

        if not Backup.HasSnapshot then //autocreate a snapshot from the filenames.
        begin
          var ExistingProduct: TProductStatus;
          if not Backup.NameToProductMap.TryGetValue(ProductId, ExistingProduct) then
          begin
            Backup.Products.Add(TProductStatus.Create());
            var NewProduct := Backup.Products.Last;
            NewProduct.Id := ProductId;
            NewProduct.Version := ProductVersion;
            NewProduct.Server := '*backup*';
            Backup.NameToProductMap.Add(ProductId, NewProduct);
          end
          else
          begin
            if ExistingProduct.Version < ProductVersion then  //if there are many versions in the same zip file, without a snapshot, get the latest.
            begin
              ExistingProduct.Version := ProductVersion;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure LoadBackup(const FileName: string; const Backup: TBackupData);
const
  DefaultSnapshotFile = 'tms.snapshot.yaml';
begin
  if TBundleDecompressor.GetFileFormat(FileName) = TDownloadFormat.Zip then
  begin
    Backup.ZipFile := FileName;
    var zip := TZipFile.Create;
    try
      zip.Open(FileName, TZipMode.zmRead);
      var idx := zip.IndexOf(DefaultSnapshotFile);
      if idx >= 0 then
      begin
        Backup.HasSnapshot := true;
        LoadSnapshotFromZip(zip, idx, Backup.Products, FileName + ':' + DefaultSnapshotFile);
      end else
      begin
        Backup.HasSnapshot := false;
      end;
      LoadProductsFromZip(zip, Backup);
    finally
      zip.Free;
    end;
  end
  else
  begin
    LoadSnapshot(FileName, Backup.Products);
    Backup.HasSnapshot := true;
  end;

end;

{ TBackupData }

constructor TBackupData.Create;
begin
  inherited;
  Products := TObjectList<TProductStatus>.Create;
  NameToIndexMap := TDictionary<string, integer>.Create;
  NameToProductMap := TDictionary<string, TProductStatus>.Create;
end;

destructor TBackupData.Destroy;
begin
  NameToProductMap.Free;
  NameToIndexMap.Free;
  Products.Free;
  inherited;
end;

function TBackupData.ProductInBackupList(const ProductId: string;
  const ProductVersion: TLenientVersion; out BackupIndex: integer): boolean;
begin
  var BackupFileName := TDownloadFileName.GenerateFileName(ProductId, ProductVersion);
  Result := NameToIndexMap.TryGetValue(BackupFileName, BackupIndex);
end;

end.

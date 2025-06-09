unit Downloads.VersionManager;
{$i ../../tmssetup.inc}

interface
procedure RotateDownloads(const MaxVersionsPerProduct: integer);

implementation
uses SysUtils, Generics.Defaults, Generics.Collections, Commands.GlobalConfig,
{$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} //to keep compiler happy
{$IFDEF POSIX}
  Posix.UniStd, Posix.Stdio,
{$ENDIF}
Deget.Version, IOUtils, UMultiLogger, Math, UTmsBuildSystemUtils;

type
TProductInfo = record
  FileName: string;
  Version: TVersion;

  constructor Create(const aFileName: string; const aVersion: TVersion);
end;
TProductInfoList = TList<TProductInfo>;
TAllProductsInfo = TObjectDictionary<string, TProductInfoList>;

//Review where this method goes. It is similar to the one in Commands.SelfUpdate and should be in a common place.
//But right now I don't know where that should be. It could also be made a little more robust.
function ExtractVersion(const FileName: string): string;
begin
  var OnlyFileName := TPath.GetFileNameWithoutExtension(FileName);
  Result := OnlyFileName.Substring(OnlyFileName.LastIndexOf('_') + 1);

  //.tar.zstd files have 2 extensions, and GetFileNameWithoutExtension will only remove the first.
  //this should be indeed made more robust.
  var i := Result.Length;
  while (i > 0) and not (CharInSet(Result[i], ['0'..'9'])) do dec(i);
  if i < Result.Length then SetLength(Result, i);
end;

function ExtractProduct(const FileName: string): string;
begin
  var OnlyFileName := TPath.GetFileNameWithoutExtension(FileName);
  Result := OnlyFileName.Substring(0, OnlyFileName.LastIndexOf('_'));
end;


//This method could be made simpler using TDirectory.GetFiles, but would also generate a lot more temporary objects.
function GetProducts(const Folder: string): TAllProductsInfo;
var
  F: SysUtils.TSearchRec;
begin
  Result := TAllProductsInfo.Create([doOwnsValues]);
  try
    if SysUtils.FindFirst(TPath.Combine(Folder, '*'), faAnyFile, F) = 0 then begin
      try
        repeat
          if (F.Attr and faDirectory <> 0) then begin
          end else
          begin
            var FileName := TPath.Combine(Folder, F.Name);
              try
              var ProductName := ExtractProduct(FileName);
              var TmpList: TProductInfoList := nil;
              if not Result.TryGetValue(ProductName, TmpList) then
              begin
                TmpList := TProductInfoList.Create;
                Result.Add(ProductName, TmpList);
              end;
              TmpList.Add(TProductInfo.Create(FileName, ExtractVersion(FileName)));
            except
              Logger.Info(Format('Can''t process downloaded file "%s", looks invalid. You might need to delete it manually.', [FileName]));
            end;
          end;
        until SysUtils.FindNext(F) <> 0;
      finally
        SysUtils.FindClose(F);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure SortVersions(const Product: TProductInfoList);
begin
  Product.Sort(
  TComparer<TProductInfo>.Construct(
    function (const a, b: TProductInfo): integer
    begin
      if a.Version < b.Version then exit(1);
      if a.Version > b.Version then exit(-1);
      exit(0);
    end)
  );
end;

procedure DeleteFileAndLog(const FileName: string);
begin
  if SysUtils.DeleteFile(FileName) then //No need to move to locked folder. If we can't delete it, we'll keep retrying
  begin
    Logger.Info(Format('Deleted file "%s"', [FileName]));
  end else
  begin
    //ignore errors, file might be in use or something. We will keep trying every time.
    Logger.Info(Format('Can''t delete file "%s". %s', [FileName, SysErrorMessage(GetLastError)]));
  end;

end;

procedure MoveFileAndLog(const SourceFileName, DestFolder: string);
begin
  var DestFileName := TPath.Combine(DestFolder, TPath.GetFileName(SourceFileName));
  try
    TDirectory_CreateDirectory(DestFolder);
    SysUtils.DeleteFile(DestFileName);

    //do not use TFile.Move. It thinks R:\ <> r:\ and decides are different drives, trying to copy instead, and causing crashes here.
    //FileName and TempFileName should be in the same drive always, so there is never a need to copy.
    RenameAndCheck(SourceFileName, DestFileName);
    Logger.Info(Format('Moved "%s" to "%s"', [SourceFileName, DestFileName]));
  except on ex: Exception do
    //ignore errors, file might be in use or something. We will keep trying every time.
    Logger.Info(Format('Can''t move file from "%s" to "%s". %s', [SourceFileName, DestFileName, ex.Message]));
  end;

end;

procedure MoveOrDeleteOlder(const Product: TProductInfoList; const DeleteInsteadOfMoving: boolean; const Keep: integer);
begin
  SortVersions(Product);
  for var i := Math.Max(0, Keep) to Product.Count - 1 do
  begin
    if DeleteInsteadOfMoving then
    begin
      DeleteFileAndLog(Product[i].FileName);
    end else
    begin
      MoveFileAndLog(Product[i].FileName, Config.Folders.OldDownloadsFolder);
    end;
  end;
end;

procedure MoveFromCurrentToOld(const MaxVersionsPerProduct: integer);
begin
  var Keep := 1;
  if (MaxVersionsPerProduct = 0) then Keep := 0;
  var DeleteInsteadOfMoving := false;
  if (MaxVersionsPerProduct = 0) or (MaxVersionsPerProduct = 1) then DeleteInsteadOfMoving := true;

  var Current := GetProducts(Config.Folders.DownloadsFolder);
  try
    for var Product in Current.Values do
    begin
      if Product.Count > 0 then MoveOrDeleteOlder(Product, DeleteInsteadOfMoving, Keep);
    end;
  finally
    Current.Free;
  end;
end;


procedure DeleteFromOld(const MaxVersionsPerProduct: integer);
begin
 var Old := GetProducts(Config.Folders.OldDownloadsFolder);
 try
   for var Product in Old.Values do
   begin
     if Product.Count > MaxVersionsPerProduct - 1 then MoveOrDeleteOlder(Product, true, MaxVersionsPerProduct - 1);
   end;
 finally
   Old.Free;
 end;
end;

procedure RotateDownloads(const MaxVersionsPerProduct: integer);
begin
 Logger.Info('Cleaning up downloaded files...');
 MoveFromCurrentToOld(MaxVersionsPerProduct);

 if MaxVersionsPerProduct >= 0 then  //negative means keep everything.
 begin
   DeleteFromOld(MaxVersionsPerProduct);
 end;

 Logger.Info('Finished cleaning up downloaded files.')
end;

{ TProductInfo }

constructor TProductInfo.Create(const aFileName: string;
  const aVersion: TVersion);
begin
  FileName := aFileName;
  Version := aVersion;
end;

end.

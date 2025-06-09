unit Removal.FolderDeleter;

interface

uses
  Generics.Defaults, System.Generics.Collections, System.IOUtils, System.SysUtils, System.SyncObjs, System.Classes,
  UMultiLogger, UAppTerminated;
type

  TDeleteInfoList = class(TList<string>);
  TFilesAndFoldersDeleteInfo = class
  private
    //We keep folders and files separated, so we can delete the files in parallel, and after they are all gone, delete the folders.
    FFiles: TDeleteInfoList;
    FFolders: TDeleteInfoList;
  public
    constructor Create;
    destructor Destroy; override;

    property Files: TDeleteInfoList read FFiles;
    property Folders: TDeleteInfoList read FFolders;
  end;

  TDirectoryWalkProc = reference to procedure(const Path: string; const FileInfo: TSearchRec);

  TFolderDeleter = class
  private
    FRootFolders: TDictionary<string, TFilesAndFoldersDeleteInfo>;
    FTotal: Integer;
    FProcessed: Integer;
    procedure GatherFiles(const Folder: string; FilesAndFolders: TFilesAndFoldersDeleteInfo);
    procedure WalkThroughDirectory(const Path: string; Proc: TDirectoryWalkProc);
    procedure DeleteFileItem(const Item: string);
    procedure DeleteFolderItem(const Item: string);
    function CaptureDelete(FolderIndex: integer; AFolder: TPair<string, TFilesAndFoldersDeleteInfo>; Proc: TProc<integer, TPair<string, TFilesAndFoldersDeleteInfo>>): TProc;
    function CaptureGather(AFolder: TPair<string, TFilesAndFoldersDeleteInfo>;
      Proc: TProc<TPair<string, TFilesAndFoldersDeleteInfo>>): TProc;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFolder(const Folder: string);
    procedure DeleteAll;
  end;

implementation
uses UTmsBuildSystemUtils, Commands.GlobalConfig, Threading, ULogger, ULoggerTask;


{ TFolderDeleter }

procedure TFolderDeleter.AddFolder(const Folder: string);
begin
  if not FRootFolders.ContainsKey(Folder) then
    FRootFolders.Add(Folder, TFilesAndFoldersDeleteInfo.Create);
end;

constructor TFolderDeleter.Create;
begin
  FRootFolders := TObjectDictionary<string, TFilesAndFoldersDeleteInfo>.Create([doOwnsValues]);
end;

function TFolderDeleter.CaptureDelete(FolderIndex: integer; AFolder: TPair<string, TFilesAndFoldersDeleteInfo>; Proc: TProc<integer, TPair<string, TFilesAndFoldersDeleteInfo>>): TProc;
begin
  Result := procedure begin Proc(FolderIndex, AFolder); end;
end;

function TFolderDeleter.CaptureGather(AFolder: TPair<string, TFilesAndFoldersDeleteInfo>; Proc: TProc<TPair<string, TFilesAndFoldersDeleteInfo>>): TProc;
begin
  Result := procedure begin Proc(AFolder); end;
end;

procedure TFolderDeleter.DeleteAll;
begin
  // gather files to be deleted
  Logger.Info('Gathering files to be removed...');
  FTotal := 0;
  var Tasks: TArray<ITask> := nil;
  SetLength(Tasks, FRootFolders.Count);
  var CurrentRootFolder := 0;
  for var Folder in FRootFolders do
  begin
    Tasks[CurrentRootFolder] := RunTask(
    CaptureGather(Folder,
      procedure(AFolder: TPair<string, TFilesAndFoldersDeleteInfo>)
      begin
        GatherFiles(AFolder.Key, AFolder.Value);
        AtomicIncrement(FTotal, AFolder.Value.Files.Count);
      end)
    );

    Inc(CurrentRootFolder);
  end;

  TTask.WaitForAll(Tasks);

  Logger.SetPercentAction(
    function: integer
    begin
       if (FTotal = 0) or (FProcessed > FTotal) then
         Exit(-1);
       Result := (FProcessed * 100) div FTotal;
    end);
  try

  // delete all files
  Logger.Info('Starting removal');
  FProcessed := 0;
  var FolderDeletes: TArray<ITask> := nil;
  SetLength(FolderDeletes, FRootFolders.Count);
  var CurrentFolder := 0;
  var BatchSize := FRootFolders.Count * 100; //the more folders we have to delete in parallel, the less we need file parallelism.
  for var Folder in FRootFolders do
  begin
    FolderDeletes[CurrentFolder] := RunTask(
      CaptureDelete(CurrentFolder, Folder,
      procedure (CF: integer; AFolder: TPair<string, TFilesAndFoldersDeleteInfo>)
      begin
        Logger.Info('Removing ' + AFolder.Key);

        //First all files, then the empty folders
        var FileCount := AFolder.Value.Files.Count;
        TParallel.For(0, (FileCount - 1) div BatchSize, procedure(i: integer)
        begin
          for var index := i * BatchSize to (i + 1) * BatchSize - 1 do
          begin
            if index >= FileCount then exit;

            var Item := AFolder.Value.Files[index];
            //Logger.Trace('-' + Item);
            try
              DeleteFileItem(Item);
            except on ex: Exception do
              Logger.Error('Error removing file: ' + Item +': ' + ex.Message);
            end;
            AtomicIncrement(FProcessed);
            if AppIsTerminated then
              exit;
          end;
        end);

        //This can't be done in parallel, because the order matters. We can't remove c:\A if we haven't removed c:\A\B.
        //In any case, it should be so fast that doing it in parallel might slow it down.
        for var Item in AFolder.Value.Folders do
        begin
          try
            DeleteFolderItem(Item);
          except on ex: Exception do
            Logger.Error('Error removing folder: ' + Item +': ' + ex.Message);
          end;
        end;

        Logger.Info('Directory removed: ' + AFolder.Key);

      end));
    Inc(CurrentFolder);
  end;
  TTask.WaitForAll(FolderDeletes);
  finally
    Logger.ResetPercentAction;
  end;
end;

procedure TFolderDeleter.DeleteFileItem(const Item: string);
begin
{$IFDEF MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
  FileSetAttr(Item, System.SysUtils.faNormal);
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF MSWINDOWS}

  DeleteFileOrMoveToLocked(Config.Folders.LockedFilesFolder, Item);
end;

procedure TFolderDeleter.DeleteFolderItem(const Item: string);
begin
{$IFDEF MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
  FileSetAttr(Item, System.SysUtils.faNormal);
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF MSWINDOWS}
  RemoveDir(Item);
end;

destructor TFolderDeleter.Destroy;
begin
  FRootFolders.Free;
  inherited;
end;

procedure TFolderDeleter.GatherFiles(const Folder: string; FilesAndFolders: TFilesAndFoldersDeleteInfo);
begin
  WalkThroughDirectory(Folder,
    procedure(const Path: string; const FileInfo: TSearchRec)
    begin
      var IsDir := (FileInfo.Attr and System.SysUtils.faDirectory) = System.SysUtils.faDirectory;
      if IsDir then FilesAndFolders.Folders.Add(TPath.Combine(Path, FileInfo.Name))
      else FilesAndFolders.Files.Add(TPath.Combine(Path, FileInfo.Name));
    end);
  FilesAndFolders.Folders.Add(Folder);
end;

procedure TFolderDeleter.WalkThroughDirectory(const Path: string; Proc: TDirectoryWalkProc);
var
  SearchRec: TSearchRec;
begin
  if FindFirst(TPath.Combine(Path, '*', False), faAnyFile, SearchRec) = 0 then
  try
    repeat
      var Ignore := false;
      // gather subfolder files recursively
      if (SearchRec.Attr and System.SysUtils.faDirectory <> 0) then
      begin
        Ignore := (SearchRec.Name = '.') or (SearchRec.Name = '..');
        if not Ignore then WalkThroughDirectory(TPath.Combine(Path, SearchRec.Name, False), Proc);
      end;

      // call the post-order callback method
      if not Ignore then Proc(Path, SearchRec);
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;

{ TFilesAndFoldersDeleteInfo }

constructor TFilesAndFoldersDeleteInfo.Create;
begin
  FFiles := TDeleteInfoList.Create;
  FFolders := TDeleteInfoList.Create;
end;

destructor TFilesAndFoldersDeleteInfo.Destroy;
begin
  FFiles.Free;
  FFolders.Free;
  inherited;
end;

end.

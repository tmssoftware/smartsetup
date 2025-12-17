unit Doctor.DuplicatedBplCheck;
interface
{$IFDEF MSWINDOWS}
uses Doctor.Check, SysUtils, Classes, Generics.Collections,
     UFileProperties, Doctor.DuplicatedFiles, Deget.CoreTypes;

type
TBplBitCollection = TDictionary<string, TArray<string>>;

TBplCollection = class
  private
    procedure Add(const Collection: TBplBitCollection; const FileName: string);
    class function FromEmbarcadero(const FileName: string): boolean;
  public
    Bit32: TBplBitCollection;
    Bit64: TBplBitCollection;
    Folders: THashSet<string>;
    Corrupt: TList<string>;

    constructor Create;
    destructor Destroy; override;

    procedure AddFolder(const FolderPath: string);
end;

TDuplicatedBplCheck = class(TCheck)
  private
    Bpls: TBplCollection;
    procedure AnalyzeBpls(const FullPath: string);
    procedure AddDuplicatedFixes;
  public
    constructor Create;
    destructor Destroy; override;
    function Name: string; override;
    function Description: string; override;
    //Slow checks shouldn't be run by tms build.
    function IsSlow: boolean; override;
    procedure Check; override;

    procedure Fix(const UndoInfo: TUndoInfo); override;

    class procedure TestCompanyName(const Folder: string);

end;

TCorruptBplFix = class(TFix)
  private
    FileName: string;
  public
    constructor Create(const aFileName: string);
end;

TDuplicatedBplFix = class(TDuplicatedFileFix)
  public
    constructor Create(const aFolderList: TArray<string>; const aFileList: TArray<string>);
end;


{$ENDIF}

implementation
{$IFDEF MSWINDOWS}
uses UWindowsPath, IOUtils, Windows, UDllBitness, UMultiLogger,
     Deget.IDEInfo, Deget.DelphiInfo, Commands.GlobalConfig, ShellApi;

type
TDuplicatedFixInfo = class
  public
    Folders: TArray<string>;
    Files: TArray<string>;

    constructor Create(const aFolders: TArray<string>);
end;

{ TDuplicatedBplCheck }

constructor TDuplicatedBplCheck.Create;
begin
  inherited;
  Bpls := TBplCollection.Create;
end;

destructor TDuplicatedBplCheck.Destroy;
begin
  Bpls.Free;
  inherited;
end;

procedure TDuplicatedBplCheck.AnalyzeBpls(const FullPath: string);
begin
  var Paths := FullPath.Split([';'], TStringSplitOptions.None);
  for var Path in Paths do
  begin
    var ExpandedPath := ExpandWindowsPath(Path);
    Bpls.AddFolder(ExpandedPath);
  end;
end;

procedure TDuplicatedBplCheck.Check;
begin
  AnalyzeBpls(GetUserWindowsPath);
  AnalyzeBpls(GetLocalMachineWindowsPath);

  for var Corrupt in Bpls.Corrupt do
  begin
    if TFile.GetSize(Corrupt) > 0 then continue; //To be safe, let's only offer to remove 0-byte files. We might also get an invalid file here because other reasons.

    Fixes.Add(TCorruptBplFix.Create(Corrupt));
  end;

  if Bpls.Folders.Count > 0 then
  begin
    AddDuplicatedFixes;
  end;

end;

procedure TDuplicatedBplCheck.AddDuplicatedFixes;
begin
  var AllFixInfo := TObjectDictionary<string, TDuplicatedFixInfo>.Create([doOwnsValues]);
  try
    for var bpl in Bpls.Bit32 do
    begin
      var Folders := bpl.Value;
      if Length(Folders) < 2 then continue;
      TArray.Sort<string>(Folders);
      var key := String.Join(';', Folders);
      var FixInfo: TDuplicatedFixInfo;
      if AllFixInfo.TryGetValue(key, FixInfo) then
      begin
        FixInfo.Files := FixInfo.Files + [bpl.Key];
      end else
      begin
        FixInfo := TDuplicatedFixInfo.Create(Folders);
        FixInfo.Files := FixInfo.Files + [bpl.Key];
        AllFixInfo.Add(key, FixInfo);
      end;
    end;

    for var FixInfo in AllFixInfo.Values do
    begin
      Fixes.Add(TDuplicatedBplFix.Create(FixInfo.Folders, FixInfo.Files));
    end;

  finally
    AllFixInfo.Free;
  end;
end;

function TDuplicatedBplCheck.Description: string;
begin
  Result := 'If there are duplicated or corrupt bpls in your Windows Path, Delphi might find the wrong ones.'
           +' This test will search for corrupt bpls and offer to delete the ones you don''t need.'
           +' For duplicated dlls, it will offer to open the folders with the files so you can manually delete the wrong ones.';
end;

procedure TDuplicatedBplCheck.Fix(const UndoInfo: TUndoInfo);
begin
  inherited;

end;

function TDuplicatedBplCheck.IsSlow: boolean;
begin
  Result := true;
end;

function TDuplicatedBplCheck.Name: string;
begin
  Result := 'Duplicated BPLs';
end;

class procedure TDuplicatedBplCheck.TestCompanyName(const Folder: string);
begin
  if not TDirectory.Exists(Folder) then raise Exception.Create('Can''t find folder ' + Folder);

  var Files := TDirectory.GetFiles(Folder, '*.bpl', TSearchOption.soTopDirectoryOnly);
  for var f in Files do
  begin
    if not TBplCollection.FromEmbarcadero(f) then raise Exception.Create('The file ' + f + ' isn''t from embarcadero.');
  end;
end;

{ TBplCollection }

procedure TBplCollection.Add(const Collection: TBplBitCollection;
  const FileName: string);
begin
  var Existing: TArray<string>;
  if not Collection.TryGetValue(TPath.GetFileName(FileName), Existing) then Existing := nil;

  Collection.AddOrSetValue(TPath.GetFileName(FileName), Existing + [TPath.GetDirectoryName(FileName)]);
  if Length(Collection[TPath.GetFileName(FileName)]) > 1 then Folders.Add(TPath.GetDirectoryName(FileName));

end;

class function TBplCollection.FromEmbarcadero(const FileName: string): boolean;
const
  EmbarcaderoNames: Array[0..2] of string = (
   //Newer values first, as they are more likely
    'Embarcadero Technologies, Inc.',
    'CodeGear',
    'Borland Software Corporation'
  );

  ReservedNames: Array[0..4] of string = (
    'bcbsmpc',
    'dclIndy',
    'Indy',
    'dclsmp',
    'vclsmp'
  );
begin
  var CompanyName := GetFileCompanyName(FileName).Trim;
  for var Name in EmbarcaderoNames do
  begin
    if CompanyName = Name then exit(true);
  end;

  //The files above don't have a company name
  for var Name in ReservedNames do if TPath.GetFileName(FileName).StartsWith(Name) then exit(true);

  Result := false;
end;

procedure TBplCollection.AddFolder(const FolderPath: string);
begin
  if not TDirectory.Exists(FolderPath) then exit;

  var Files := TDirectory.GetFiles(FolderPath, '*.bpl', TSearchOption.soTopDirectoryOnly);
  for var f in Files do
  begin
    if FromEmbarcadero(f) then exit; //do not delete files in this folder, and do not keep searching here. A file like dbtest.bpl doesn't have version number (no dbtest120.bpl), and can be in multiple emba bin folders.

    var Bitness := GetDllBitness(f);

    case Bitness of
      TBitness.Unknown: ;
      TBitness.CantOpen: Corrupt.Add(f);
      TBitness.Intel32: Add(Bit32, TPath.GetFullPath(f));
      TBitness.Intel64: Add(Bit64, TPath.GetFullPath(f));
    end;
  end;
end;

constructor TBplCollection.Create;
begin
  Bit32 := TBplBitCollection.Create;
  Bit64 := TBplBitCollection.Create;
  Folders := THashSet<string>.Create;
  Corrupt := TList<string>.Create;
end;

destructor TBplCollection.Destroy;
begin
  Bit32.Free;
  Bit64.Free;
  Folders.Free;
  Corrupt.Free;
  inherited;
end;

//Avoid loading errors in delphi once you delete the bpls.
procedure RemoveFromRegistry(const aFileName: string);
begin
  for var IDEName := Low(TIDEName) to High(TIDEName) do
  begin
    var IDEInfo: IDelphiIDEInfo := TDelphiIDEInfo.Create(IDEName, Config.AlternateRegistryKey);
    for var Platform in [TPlatform.win32intel, TPlatform.win64intel] do
    begin
      IDEInfo.UnregisterPackage(aFileName, Platform);
    end;
  end;

end;

{ TCorruptBplFix }

constructor TCorruptBplFix.Create(const aFileName: string);
begin
  inherited Create(
        TFixType.YesNo,
        'The file "' + aFileName + '" has zero bytes and is an invalid bpl.',
        'DELETE IT?',
        procedure
        begin
          Logger.Trace('Deleting file "' + aFileName + '"');
          RemoveFromRegistry(aFileName);
          TFile.Delete(aFileName);
        end);

  FileName := aFileName;

end;

{ TDuplicatedBplFix }

constructor TDuplicatedBplFix.Create(const aFolderList: TArray<string>; const aFileList: TArray<string>);
begin
  var Folders := Copy(aFolderList);
  var Files := Copy(aFileList);
  TArray.Sort<string>(Folders);
  TArray.Sort<string>(Files);
  inherited Create(
        TFixType.Numeric,
        'The folders ' + GetList(Folders) + ' contain the following duplicated files: ' + GetList(Files),
        'Select the folder with files to KEEP. We will open an explorer with the other folders so you can delete them',
        procedure
        begin
          if Apply <= 0 then exit;
          {for var FileName in Files do
          begin
            for var i := Low(Folders) to High(Folders) do
            begin
              if i = Apply - 1 then continue;

              var FileToDelete := TPath.GetFullPath(TPath.Combine(Folders[i], FileName));

              Logger.Trace('Deleting file "' + FileToDelete + '"');
              try
                RemoveFromRegistry(FileToDelete);
                TFile.Delete(FileToDelete);
              except on ex: Exception do
                Logger.Error('Can''t delete file "' + FileToDelete + '": ' + ex.Message);
              end;


            end;
          end;}

          for var i := Low(Folders) to High(Folders) do
          begin
            if i = Apply - 1 then continue;
            var Select := '';
            //We could use SHOpenFolderAndSelectItems to open explorer with multiple files selected, but it isn't worth.
            //It gets difficult to see if more than one file is selected. So we will only select them if it is a single file.
            if Length(Files) = 1 then Select := '/select,"' +  TPath.GetFullPath(TPath.Combine(Folders[i], Files[0])) + '"';

            ShellExecute(0, 'open', 'explorer', PChar(Select), PChar(Folders[i]), SW_SHOWNORMAL);

          end;
        end,
        GetQuestions(Folders)
      );

end;

{ TDuplicatedFixInfo }

constructor TDuplicatedFixInfo.Create(const aFolders: TArray<string>);
begin
  Folders := aFolders;
end;
{$ENDIF}

end.

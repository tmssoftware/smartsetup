unit VCS.Engine.Virtual;
{$I ../../tmssetup.inc}

interface
type
  TVersionAndDate = record
  public
    Version: string;
    Date: TDateTime;
  end;

  IVCSEngine = interface
    procedure Clone(const aCloneFolder, aURL, aVersion: string);
    procedure AfterClone(const aRootFolder, aCloneFolder: string);
    procedure Pull(const aRootFolder, aGitFolder, aVersion: string);
    function GetProduct(const aDestFolderRoot, aDestFolder, aURL, aServer, aProductId, aVersion: string): boolean;  //If this is implemented, then clone and push aren't used.
    function GetVersionNames(const aExistingRepoFolder, aTempFolder, aLockedFolder: string; const aURL: string): TArray<TVersionAndDate>;
    function FileIsVersioned(const aFileName, aWorkingFolder: string): boolean;

    function GetCommitId(const aWorkingFolder: string; const allowTags: boolean): string;
    function IsRootVCSFolder(const Folder: string): boolean;
  end;

implementation

end.

unit VCS.Engine.Virtual;
{$I ../../tmssetup.inc}

interface
type
  IVCSEngine = interface
    procedure Clone(const aCloneFolder, aURL, aVersion: string);
    procedure AfterClone(const aRootFolder, aCloneFolder: string);
    procedure Pull(const aRootFolder, aGitFolder, aVersion: string);
    function GetProduct(const aDestFolderRoot, aDestFolder, aURL, aServer, aProductId, aVersion: string): boolean;  //If this is implemented, then clone and push aren't used.
    function GetVersionNames(const aURL: string): TArray<string>;
    function FileIsVersioned(const aFileName, aWorkingFolder: string): boolean;
  end;

implementation

end.

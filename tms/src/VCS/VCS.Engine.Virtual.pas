unit VCS.Engine.Virtual;
{$I ../../tmssetup.inc}

interface
type
  IVCSEngine = interface
    procedure Clone(const  aCloneFolder, aURL: string);
    procedure Pull(const aFolder: string);
    function GetProduct(const aDestFolderRoot, aDestFolder, aURL, aServer, aProductId: string): boolean;  //If this is implemented, then clone and push aren't used.
  end;

implementation

end.

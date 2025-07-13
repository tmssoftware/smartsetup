unit VCS.Engine.Virtual;
{$I ../../tmssetup.inc}

interface
type
  IVCSEngine = interface
    procedure Clone(const  aCloneFolder, aURL: string);
    procedure Pull(const aFolder: string);
    procedure GetFile(const aFileName, aDestFolder, aURL, aServer: string);
    function GetProduct(const aDestFolder, aURL, aServer: string): boolean;  //If this is implemented, then clone and push aren't used.
  end;

implementation

end.

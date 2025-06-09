unit VCS.Engine.Virtual;
{$I ../../tmssetup.inc}

interface
type
  IVCSEngine = interface
    procedure Clone(const  aCloneFolder, aURL: string);
    procedure Pull(const aFolder: string);
    procedure GetFile(const aFileName, aDestFolder, aURL: string);
  end;

implementation

end.

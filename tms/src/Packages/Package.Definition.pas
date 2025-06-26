unit Package.Definition;
{$i ../../tmssetup.inc}


interface
uses Deget.CoreTypes;
type
  TPackageDefinition = class
  private
    FName: string;
    FFiles: TArray<string>;
    FRequires: TArray<string>;
    FFrameworkType: string;
    FSupportsCpp: boolean;
    FPlatforms: TPlatformSet;
  public
    property Name: string read FName;
    property Files: TArray<string> read FFiles;
    property Requires: TArray<string> read FRequires;
    property FrameworkType: string read FFrameworkType;
    property SupportsCpp: boolean read FSupportsCpp;
    property Platforms: TPlatformSet read FPlatforms;
  end;

implementation

end.

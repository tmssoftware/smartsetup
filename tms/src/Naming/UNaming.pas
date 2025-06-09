unit UNaming;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes;

type
TNaming = class
  private
    FPackagesChangeName: boolean;
  public
    constructor Create(const aPackagesChangeName: boolean);
    function Id: string; virtual; abstract;
    property PackagesChangeName: boolean read FPackagesChangeName;
    function GetPackageNaming(const dv: TIDEName; const IsExe: boolean): string; virtual; abstract;
end;

implementation

{ TNamingConvention }

constructor TNaming.Create(const aPackagesChangeName: boolean);
begin
  FPackagesChangeName := aPackagesChangeName;
end;

end.

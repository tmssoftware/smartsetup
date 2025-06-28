unit UNaming;
{$i ../../tmssetup.inc}

interface
uses Deget.CoreTypes;

type
TNaming = class
  private
    FPackagesChangeName: boolean;
  public
    function Id: string; virtual; abstract;
    function GetPackageNaming(const dv: TIDEName; const IsExe: boolean; const ProjectFolder: string): string; virtual; abstract;
end;

implementation

end.

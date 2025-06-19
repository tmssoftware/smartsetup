unit UStandardNaming;
{$i ../../tmssetup.inc}

interface
uses UNaming, Deget.CoreTypes, Deget.IDETypes, SysUtils;
type
  TStandardNaming = class(TNaming)
  public
  const
    IdStatic = '';
    constructor Create;
    function Id: string; override;

    function GetPackageNaming(const dv: TIDEName; const IsExe: boolean; const ProjectFolder: string): string; override;
  end;


implementation

{ TStandardNaming }

constructor TStandardNaming.Create;
begin
  inherited Create(false);
end;

function TStandardNaming.GetPackageNaming(const dv: TIDEName; const IsExe: boolean; const ProjectFolder: string): string;
begin
  if IsExe then exit('');
  if ProjectFolder <> '' then exit(ProjectFolder);
  Result := DelphiSuffixes[dv];
end;

function TStandardNaming.Id: string;
begin
  Result := IdStatic;
end;

end.

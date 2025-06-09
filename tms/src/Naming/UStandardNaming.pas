unit UStandardNaming;
{$i ../../tmssetup.inc}

interface
uses UNaming, Deget.CoreTypes, SysUtils;
type
  TStandardNaming = class(TNaming)
  public
  const
    IdStatic = '';
    constructor Create;
    function Id: string; override;

    function GetPackageNaming(const dv: TIDEName; const IsExe: boolean): string; override;
  end;


implementation

{ TStandardNaming }

constructor TStandardNaming.Create;
begin
  inherited Create(false);
end;

function TStandardNaming.GetPackageNaming(const dv: TIDEName; const IsExe: boolean): string;
begin
  if IsExe then exit('');
  
  Result := IDEId[dv];
  if Result.StartsWith('delphi') then Result := 'd' + Result.Substring(Length('delphi'));

end;

function TStandardNaming.Id: string;
begin
  Result := IdStatic;
end;

end.

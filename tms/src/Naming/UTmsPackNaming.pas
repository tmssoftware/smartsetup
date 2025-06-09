unit UTmsPackNaming;
{$i ../../tmssetup.inc}

interface
uses UNaming, Deget.CoreTypes, SysUtils;

type
  TTMSPackNaming = class(TNaming)
  public
  const
    IdStatic = 'tms.tmspack';
    constructor Create;

    function Id: string; override;
    function GetPackageNaming(const dv: TIDEName; const IsExe: boolean): string; override;
  end;


implementation

{ TTMSPackNaming }

constructor TTMSPackNaming.Create;
begin
  inherited Create(true);
end;

function TTMSPackNaming.GetPackageNaming(const dv: TIDEName; const IsExe: boolean): string;
begin
  case dv of
    TIDEName.lazarus: exit('LAZARUS');
    TIDEName.delphixe: exit('d2011');
    TIDEName.delphiseattle: exit('DXE9');
    TIDEName.delphiberlin: exit('DXE10');
    TIDEName.delphitokyo: exit('DXE11');
    TIDEName.delphirio: exit('DXE12');
    TIDEName.delphisydney: exit('DXE13');
    TIDEName.delphi11: exit('DXE14');
    TIDEName.delphi12: exit('DXE15');
    else exit('d' + IDEId[dv].Substring(Length('delphi')));
  end;
end;


function TTMSPackNaming.Id: string;
begin
  Result := IdStatic
end;

end.

unit IFace;

interface

function GetVersion: string;

implementation
uses Data;

function GetVersion: string;
begin
  Module := TModule.Create(nil);
  Module.TableVersion.Active := true;
  Result := Module.TableVersion.FieldByName('VERSION').Value;
end;
end.

{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit L_A;

{$warn 5023 off : no warning about unused units}
interface

uses
  L_A_Unit, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('L_A', @Register);
end.

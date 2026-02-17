unit L_C_Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, L_A_Unit, L_B_Unit;

function L_C_Go: string;

implementation
function L_C_Go: string;
begin
  Result :='L_B' + L_A_GO + L_B_Go;
end;
end.


unit L_B_Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, L_A_Unit;

function L_B_Go: string;

implementation
function L_B_Go: string;
begin
  Result :='L_B' + L_A_GO;
end;
end.


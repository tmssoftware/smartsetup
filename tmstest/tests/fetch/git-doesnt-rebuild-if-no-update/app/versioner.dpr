program versioner;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, IFace;

begin
  WriteLn(GetVersion);
end.

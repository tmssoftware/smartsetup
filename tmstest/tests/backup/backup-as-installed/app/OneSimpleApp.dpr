program OneSimpleApp;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, Bcl.Core.Encoding;

begin
  WriteLn(TBclEncoding.WebName(TEncoding.UTF8));
end.

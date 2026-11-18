unit awesomeunit2;

interface
uses System.SysUtils, Bcl.Core.Encoding;

implementation

begin
  WriteLn(TBclEncoding.WebName(TEncoding.UTF8));
end.

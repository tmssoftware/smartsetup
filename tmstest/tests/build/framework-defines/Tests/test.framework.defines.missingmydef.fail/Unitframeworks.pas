unit Unitframeworks;

interface
uses
FMX.Forms, fmx.Dialogs;

implementation

begin
  {$IFNDEF FRAMEWORK_FMX}
   framework_fmx should be enabled.
  {$ENDIF}
  {$IFNDEF MYDEF}
   this won't compile unless we pass a "MYDEF" define.
  {$ENDIF}
end.

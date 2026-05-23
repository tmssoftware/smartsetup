program tripletrouble;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, uDoubleTrouble;

begin
  try
    WriteLn(FormatFloat('0.000000', DoubleTrouble(1), TFormatSettings.Create('en-US')),
            ' | ',
            FormatFloat('0.000000', DoubleTrouble(-1), TFormatSettings.Create('en-US')),
            ' | ',
            FormatFloat('0.000000', DoubleTrouble(0), TFormatSettings.Create('en-US'))
            );
  except
    on E: Exception do
      Writeln(E.Message);
  end;
end.

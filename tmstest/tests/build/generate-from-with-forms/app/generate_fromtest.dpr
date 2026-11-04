program generate_fromtest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, oh_my_form;

begin
  var oh_my_form := TForm1.Create(nil);
  try
    WriteLn(oh_my_form.GetValue);
  finally
    oh_my_form.Free;
  end;
end.

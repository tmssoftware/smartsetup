program d7;

uses
  Forms,
  Ud7 in 'Ud7.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

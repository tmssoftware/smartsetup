unit oh_my_form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TForm1 = class(TForm)
  private
    { Private declarations }
  public
    function GetValue: string;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses Hash;

{$R *.dfm}
{$R 'myres.res' 'myres.rc'}

{ TForm1 }

function TForm1.GetValue: string;
begin
  var LStream := TResourceStream.Create(HInstance, 'MyBitmapName', RT_BITMAP);
  try
    Result := THashMD5.GetHashString(LStream);
  finally
    LStream.Free;
  end;end;

end.

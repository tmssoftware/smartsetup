unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees.BaseAncestorVCL,
  VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees;

type
  TForm2 = class(TForm)
    VirtualStringTree1: TVirtualStringTree;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

end.

unit Data;

interface

uses
  System.SysUtils, System.Classes, Data.DB, Datasnap.DBClient,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.StorageBin, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TModule = class(TDataModule)
    TableVersion: TFDMemTable;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Module: TModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

end.

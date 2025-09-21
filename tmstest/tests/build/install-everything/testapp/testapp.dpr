program testapp;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, Classes,
  FlexCel.VCLSupport, FlexCel.Core, FlexCel.XlsAdapter,
  Bcl.Json, Bcl.Json.Classes, Bcl.Json.Serializer, Bcl.Json.Writer,
  atscripter, VCL.ScripterInit, uSystemLibrary,
  Spring.Container,
  HashObj, MiscObj, CryptoConst,
  VCL.TMSFNCUtils;


type
  ICoffeeMaker = interface
    function MakeCoffee(const s: string): string;
  end;

  TCoffeeMaker = class(TInterfacedObject, ICoffeeMaker)
  public
    function MakeCoffee(const s: string): string;
  end;

function TCoffeeMaker.MakeCoffee(const s: string): string;
begin
  Result := 'Coffee is brewing! ' + s;
end;

begin
  try
    var xls := TXlsFile.Create(1, true);
    var Strings := TStringList.Create;
    for var row := 1 to 10 do
    begin
      Strings.Add(xls.RecalcRelativeFormula(1, row, 1, '="Hello from cell " & Row() & Column()').ToString + ' -> ' + TCellAddress.Create(row, 1).ToString);
    end;

    var json := '';
    var Serializer := TJsonSerializer.Create;
    var Stream := TStringStream.Create;
    var Writer := TJsonWriter.Create(Stream);
    try
      Writer.IndentLength := 2;
      Serializer.Write(Strings, Writer);
      Writer.Flush;
      Stream.Position := 0;
      json := Stream.DataString;
    finally
      Writer.Free;
      Stream.Free;
      Serializer.Free;
    end;

    var scripter := TatScripter.Create(nil);
    scripter.SourceCode.Text := 'function Calculate(s: string): string; begin Result := ''Hello from scripter: '' + s;end;';
    var FunctionValue := Scripter.ExecuteSubRoutine('Calculate', json);

    var Container := TContainer.Create;
    Container.RegisterType<ICoffeeMaker, TCoffeeMaker>;
    Container.Build;
    var CoffeeMaker := Container.Resolve<ICoffeeMaker>;
    var coffee := CoffeeMaker.MakeCoffee(FunctionValue);

    var coffee64 := TTMSFNCUtils.Encode64(coffee);
    var sha2:= TSHA2Hash.Create;
    sha2.HashSize:= THashSize.hsha256;
    sha2.OutputFormat:= hexa;
    sha2.Unicode:= noUni;
    var hash:= sha2.Hash(coffee64);
    sha2.Free;
    WriteLn(hash);

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

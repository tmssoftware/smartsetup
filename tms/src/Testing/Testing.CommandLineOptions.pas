unit Testing.CommandLineOptions;

interface
{$IFDEF DEBUG}

procedure RegisterTestingOptions;

{$ENDIF}
implementation
{$IFDEF DEBUG}
uses Testing.Globals, UCommandLine;

procedure RegisterTestingOptions;
begin
  var option := TOptionsRegistry.RegisterOption<Boolean>(
    'test-offline', '', 'Raises an exception if you try to access the internet. Use it for testing offline work.',
    procedure(const Value: Boolean)
    begin
      TestParameters.Offline := Value;
    end);

  option.HasValue := False;
  option.Hidden := true;

  option := TOptionsRegistry.RegisterOption<string>(
    'test-company-names', '', 'Causes doctor to check if the dcus in the folder have the correct company name.',
    procedure(const Value: String)
    begin
      TestParameters.FolderForCompanyName := Value
    end);
  option.Hidden := true;

  option := TOptionsRegistry.RegisterOption<Boolean>(
    'test-delphi-ce', '', 'Forces Delphi CE compilation.',
    procedure(const Value: Boolean)
    begin
      TestParameters.DelphiCE := Value;
    end);

  option.HasValue := False;
  option.Hidden := true;

end;

{$ENDIF}
end.

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

  option := TOptionsRegistry.RegisterOption<string>(
    'test-credentials-profile', '', 'Allows us to use a different credential profile to test configurations without credentials.',
    procedure(const Value: String)
    begin
      TestParameters.CredentialsProfile := Value
    end);
  option.Hidden := true;

  option := TOptionsRegistry.RegisterOption<Boolean>(
    'test-no-svn', '', 'It is really not possible to test that svn isn''t findable, because tms reads the path from the registry, and we can''t change the registry in a test. Chaning $PATH from the test won''t work',
    procedure(const Value: Boolean)
    begin
      TestParameters.NoSVN := Value;
    end);
  option.HasValue := False;
  option.Hidden := true;

  option := TOptionsRegistry.RegisterOption<Boolean>(
    'test-no-git', '', 'It is really not possible to test that git isn''t findable, because tms reads the path from the registry, and we can''t change the registry in a test. Chaning $PATH from the test won''t work',
    procedure(const Value: Boolean)
    begin
      TestParameters.NoGit := Value;
    end);
  option.HasValue := False;
  option.Hidden := true;

end;

{$ENDIF}
end.

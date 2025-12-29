unit Testing.Globals;

interface
{$IFDEF DEBUG}
uses Classes, SysUtils;
type
  TTestParameters = class
  public
    Offline: boolean;
    FolderForCompanyName: string;
    DelphiCE: boolean;
    CredentialsProfile: string;


    procedure CheckOffline(const Method: string);
  end;

var
  TestParameters: TTestParameters;


{$ENDIF}
implementation
{$IFDEF DEBUG}


{ TTestParameters }

procedure TTestParameters.CheckOffline(const Method: string);
begin
  if Offline then raise Exception.Create(Method + ': Offline mode, no connection to internet.');

end;

initialization
  TestParameters := TTestParameters.Create;
finalization
  TestParameters.Free;

{$ENDIF}
end.

unit Doctor.DelphiEnvironmentCheck;

interface

{$IFDEF MSWINDOWS}

uses Doctor.Check, SysUtils, Classes, Deget.CoreTypes, Deget.IDEInfo,
  Doctor.MultiIDECheck;

type

  TDelphiEnvironmentPathCheck = class(TSingleIDECheck)
  public
    procedure Check; override;
    function Name: string; override;
    function Description: string; override;
  end;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses IOUtils, Deget.DelphiInfo;

{ TDelphiEnvironmentPathCheck }

procedure TDelphiEnvironmentPathCheck.Check;
begin
  var PathOverride := IDEInfo.GetPathOverride;
  if PathOverride = '' then
    exit;

  if (';' + PathOverride + ';').ToUpperInvariant.Contains(';$(PATH);') then
    exit;

  Fixes.Add(TFix.Create(TFixType.YesNo, 'The override for PATH in ' + IDEId[IDEInfo.IDEName] + ': "' +
    PathOverride + '" lacks an entry for $(PATH).', 'Add it?',
    procedure
    begin
      IDEInfo.AddFolderToPathOverride('$(PATH)', False);
    end))
end;

function TDelphiEnvironmentPathCheck.Description: string;
begin
  Result := 'Delphi stores a PATH override in its environment variables. ' +
    'If this override doesn''t include a "$(PATH)" entry, ' +
    'Delphi might fail to load the packages that TMS Smart Setup installed. ' +
    'This check ensures there is a $(PATH) entry inside Delphi''s Path override.';

end;

function TDelphiEnvironmentPathCheck.Name: string;
begin
  Result := '$(PATH) in Path override';
end;

{$ENDIF}

end.

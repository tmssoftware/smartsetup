unit UNamingList;
{$i ../../tmssetup.inc}

interface
uses SysUtils, Generics.Collections, UNaming, UStandardNaming;

type
  TNamingList = class
  private
    FList: TObjectDictionary<string, TNaming>;
  public
    constructor Create;
    destructor Destroy; override;

    function GetNaming(const NamingId: string; const PathForExtraNamings, ProjectFilename: string): TNaming;
  end;
implementation

{ TNamingList }

constructor TNamingList.Create;
begin
  FList := TObjectDictionary<string, TNaming>.Create([doOwnsValues]);
  FList.Add(TStandardNaming.IdStatic, TStandardNaming.Create);
end;

destructor TNamingList.Destroy;
begin
  FList.Destroy;
  inherited;
end;

function TNamingList.GetNaming(const NamingId,
  PathForExtraNamings, ProjectFilename: string): TNaming;
begin
  //In a future version, we could try loading the yaml from PathForExtraNamings first.

  if FList.TryGetValue(NamingId, Result) then exit;
  raise Exception.Create('Can''t find the naming: "' + NamingId + '" mentioned in "' + ProjectFilename + '".');
end;

end.

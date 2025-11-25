unit Fetching.ProductVersion;

interface
uses Classes, SysUtils;

type
  TProductVersion = record
  public
    IdMask: string;
    Version: string;

    constructor Create(const aIdMask, aVersion: string);
  end;

  TProductAndVersionArray = TArray<TProductVersion>;

  function ParseVersions(const ProductsAndVersions: TArray<string>): TProductAndVersionArray;

implementation

function ParseVersions(const ProductsAndVersions: TArray<string>): TProductAndVersionArray;
begin
  Result := nil;
  SetLength(Result, Length(ProductsAndVersions));
  for var i := Low(ProductsAndVersions) to High(ProductsAndVersions) do
  begin
    var idx := ProductsAndVersions[i].IndexOf(':');
    if idx < 0 then
    begin
      Result[i].IdMask := ProductsAndVersions[i];
      Result[i].Version := '';
    end
    else
    begin
      Result[i].IdMask := ProductsAndVersions[i].Substring(0, idx).Trim;
      Result[i].Version := ProductsAndVersions[i].Substring(idx + 1).Trim;
    end;
  end;

end;

{ TProductVersion }

constructor TProductVersion.Create(const aIdMask, aVersion: string);
begin
  IdMask := aIdMask;
  Version := aVersion;
end;

end.

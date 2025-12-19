unit Snapshot.Reader;

interface
  uses Classes, SysUtils, Generics.Collections, Status.Manager, BBYaml, BBClasses;

  procedure LoadSnapshot(const FileName: string; const Products: TList<TProductStatus>);
implementation
type

TSnapshotSection = class(TSection)
  public
    constructor Create(const aParent: TSection);
end;

TSnapshotMainSection = class(TSnapshotSection)
  public
    constructor Create(const aProducts: TList<TProductStatus>);
    class function SectionNameStatic: string; override;
end;

TProductsSection = class(TSnapshotSection)
  private
    FProducts:  TList<TProductStatus>;
  public
    constructor Create(const aParent: TSection; const aProducts: TList<TProductStatus>);

    property Products: TList<TProductStatus> read FProducts;
    class function SectionNameStatic: string; override;
end;

TProductSection = class(TSnapshotSection)
  private
    Name: string;
    FProduct:  TProductStatus;
  public
    constructor Create(const aParent: TSection; const aName: string; const aProduct: TProductStatus);
    class function SectionNameStatic: string; override;
    function SectionName: string; override;
end;


procedure LoadSnapshot(const FileName: string; const Products: TList<TProductStatus>);
begin
   var MainSection := TSnapshotMainSection.Create(Products);
  try
    TBBYamlReader.ProcessFile(Filename, MainSection, '', false);
  finally
    MainSection.Free;
  end;

end;


{ TSnapshotSection }

constructor TSnapshotSection.Create(const aParent: TSection);
begin
  inherited Create(aParent);
end;


{ TSnapshotMainSection }

constructor TSnapshotMainSection.Create(const aProducts: TList<TProductStatus>);
begin
  inherited Create(nil);
  ChildSections.Add(TProductsSection.SectionNameStatic, TProductsSection.Create(Self, aProducts));
end;

class function TSnapshotMainSection.SectionNameStatic: string;
begin
  Result := 'root';
end;


{ TProductsSection }

constructor TProductsSection.Create(const aParent: TSection;
  const aProducts: TList<TProductStatus>);
begin
  inherited Create(aParent);
  FProducts := aProducts;
  ContainsArrays := true;
  ClearArrayValues := procedure begin FProducts.Clear;end;

  ChildSectionAction :=
    function(Name: string; ErrorInfo: TErrorInfo; const KeepValues: boolean): TSection
    begin
      var Product := TProductStatus.Create;
      Products.Add(Product);
      Product.Id := Name;
      Result := TProductSection.Create(Self, Name, Product);
      ChildSections.Add(Name, Result);
    end;
end;

class function TProductsSection.SectionNameStatic: string;
begin
  Result := 'products';
end;

{ TProductSection }

constructor TProductSection.Create(const aParent: TSection; const aName: string;
  const aProduct: TProductStatus);
begin
  inherited Create(aParent);
  Name := aName;
  FProduct := aProduct;

  Actions := TListOfActions.Create;
  Actions.Add('version', procedure(value: string; ErrorInfo: TErrorInfo)
  begin;
    FProduct.Version := value;
  end);
  Actions.Add('server', procedure(value: string; ErrorInfo: TErrorInfo)
  begin;
    FProduct.Server := value;
  end);
  Actions.Add('url', procedure(value: string; ErrorInfo: TErrorInfo)
  begin;
    //we don't currently use this, only stored for historical value.
  end);
  Actions.Add('pinned', procedure(value: string; ErrorInfo: TErrorInfo)
  begin;
    FProduct.Pinned := GetBool(value, ErrorInfo);
  end);
end;

function TProductSection.SectionName: string;
begin
  Result := Name;
end;

class function TProductSection.SectionNameStatic: string;
begin
  Result := 'error';
end;

end.

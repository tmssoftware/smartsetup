unit Commands.RepoList;
{$I ../../tmssetup.inc}

interface
procedure RegisterRepoListCommand;

implementation
uses
  Commands.CommonOptions, Commands.Logging, Commands.GlobalConfig, System.JSON,
  UJsonPrinter, VCS.Registry, Generics.Defaults, Generics.Collections, SysUtils, UCommandLine;

var
  EnableLog: Boolean = False;
  UseJson: Boolean = False;

procedure OutputAsJson(Products: TList<TRegisteredProduct>);
begin
  var Root := TJSONObject.Create;
  try
    for var Product in Products do
    begin
      var Item := TJSONObject.Create;
      Root.AddPair(Product.ProductId, Item);

      Item.AddPair('protocol', Product.ProtocolString);
      Item.AddPair('url', Product.Url);
      Item.AddPair('name', Product.Name);
      Item.AddPair('description', Product.Description);
      Item.AddPair('predefined', Product.IsPredefined)
    end;
    OutputJson(Root);
  finally
    Root.Free;
  end;
end;

procedure OutputAsText(Products: TList<TRegisteredProduct>);
begin
  for var Product in Products do
  begin
    var ProductId := Product.ProductId;
    if not Product.IsPredefined then ProductId := ProductId + '*';

    WriteLn(Format('%s -> %s', [ProductId, Product.Url]));
  end;
end;

procedure RunRepoListCommand;
begin
  InitFolderBasedCommand(EnableLog);
  var Products := TList<TRegisteredProduct>.Create;
  try
    RegisteredVCSRepos.GetProducts('*', Products, nil);
    var ProdComparer: IComparer<TRegisteredProduct> := TDelegatedComparer<TRegisteredProduct>.Create(function(const a, b: TRegisteredProduct): integer
      begin
        Result :=CompareText(a.ProductId, b.ProductId, TLocaleOptions.loInvariantLocale);
      end);
    Products.Sort(ProdComparer);
    if UseJson then
      OutputAsJson(Products)
    else
      OutputAsText(Products);
  finally
    Products.Free;
  end;
end;

procedure RegisterRepoListCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('repo-list', '', 'list products in GIT/SVN repositories',
    '',
    'repo-list');

  RegisterRepoOption(cmd);

  var option := cmd.RegisterOption<Boolean>('log', '', 'enable logging for this command',
    procedure(const Value: Boolean)
    begin
      EnableLog := Value;
    end);
  option.HasValue := False;
  option.Hidden := True;

  option := cmd.RegisterOption<Boolean>('json', '', 'output data in JSON format',
    procedure(const Value: Boolean)
    begin
      UseJson := Value;
    end);
  option.HasValue := False;

  AddCommand(cmd.Name, CommandGroups.Status, RunRepoListCommand);
end;

end.

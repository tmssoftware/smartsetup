unit Commands.Pin;

interface
procedure RegisterPinCommand;
procedure RegisterUnpinCommand;


implementation
uses
  System.StrUtils, SysUtils, Classes, UCommandLine, Commands.CommonOptions, Commands.Logging,
  Commands.GlobalConfig, Fetching.InstallInfo,
  Generics.Collections, Fetching.InfoFile, UMultiLogger, Masks;

var
  ProductMasks: TArray<string>;

function Matches(const ProductId: string): boolean;
begin
  for var Mask in ProductMasks do if MatchesMask(ProductId, Mask) then exit(true);
  Result := false;
end;

procedure ExecutePinAction(const Pin: boolean);
begin
  var Count := 0;
  var InstalledProducts := TObjectList<TFetchInfoFile>.Create;
  try
    GetFetchedProducts(Config.Folders.ProductsFolder, InstalledProducts);
    for var Product in InstalledProducts do
    begin
      if Matches(Product.ProductId) then
      begin
        if Product.Pinned = Pin then
        begin
          if Pin then Logger.Info('Skipped ' + ' ' + Product.ProductId + ' because it was already pinned.')
          else Logger.Info('Skipped ' + ' ' + Product.ProductId + ' because it was already unpinned.');
        end else
        begin
          if Pin then Logger.Info('Pinned ' + Product.ProductId + ' to version ' + Product.Version)
          else Logger.Info('Unpinned ' + Product.ProductId);
          TFetchInfoFile.SaveInFolder(Product.ProductPath, Product.ProductId, Product.Version, Product.Server, Pin);
          inc(Count);
        end;
      end;

    end;
  finally
    InstalledProducts.Free;
  end;

  if Pin then Logger.Info('Pinned ' + IntToStr(Count) + ' products')
  else Logger.Info('Unpinned ' + IntToStr(Count) + ' products');
end;

procedure RunPinCommand;
begin
  InitFolderBasedCommand;

  ExecutePinAction(true);
end;

procedure RunUnpinCommand;
begin
  InitFolderBasedCommand;

  ExecutePinAction(false);
end;

procedure RegisterPinCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('pin', '', 'Keeps the specified products at their current versions',
    'You can use this command to keep a specific product at a specific version, and not update when you do `tms update`.',
    'pin [<product-ids>]');

  cmd.Examples.Add('pin tms.biz.*');
  cmd.Examples.Add('pin tms.biz.aurelius,tms.fnc.maps');
  cmd.Examples.Add('pin *');

  var option := cmd.RegisterUnNamedOption<string>('the ids of the products to be pinned', 'product-ids',
    procedure(const Value: string)
    begin
      ProductMasks := ProductMasks + SplitString(Value, ',');
    end);
  option.AllowMultiple := True;
  option.Required := True;

  AddCommand(cmd.Name, CommandGroups.Install, RunPinCommand);
end;

procedure RegisterUnpinCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('unpin', '', 'Removes the pin for the specified products',
    'If you''ve previously pinned a product with `tms pin`, you can unpin it with this command.',
    'unpin [<product-ids>]');


  cmd.Examples.Add('unpin tms.biz.*');
  cmd.Examples.Add('unpin tms.biz.aurelius,tms.fnc.maps');
  cmd.Examples.Add('unpin *');

  var option := cmd.RegisterUnNamedOption<string>('the ids of the products to be unpinned', 'product-ids',
    procedure(const Value: string)
    begin
      ProductMasks := ProductMasks + SplitString(Value, ',');
    end);
  option.AllowMultiple := True;
  option.Required := True;

  AddCommand(cmd.Name, CommandGroups.Install, RunUnpinCommand);
end;
end.

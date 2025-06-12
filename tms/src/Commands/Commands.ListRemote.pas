unit Commands.ListRemote;

interface

uses
  System.Generics.Collections, System.SysUtils, UCommandLine;

procedure RegisterListRemoteCommand;

implementation

uses
  Commands.CommonOptions, Commands.Logging, Commands.GlobalConfig, URepositoryManager, Deget.Version, System.JSON,
  UJsonPrinter;

var
  EnableLog: Boolean = False;
  UseJson: Boolean = False;

procedure OutputAsJson(Products: TList<TRepositoryProduct>);
const
  LicenseStatusStr: array[TLicenseStatus] of string = ('none', 'licensed');
begin
  var Root := TJSONObject.Create;
  try
    for var Product in Products do
    begin
      var Item := TJSONObject.Create;
      Root.AddPair(Product.Id, Item);

      Item.AddPair('version', TVersion(Product.LatestVersion.Version).Normalized);
      Item.AddPair('name', Product.Name);
      if Product.LicenseStatus <> TLicenseStatus.none then
         Item.AddPair('license_status', LicenseStatusStr[Product.LicenseStatus]);
      if Product.VendorId <> '' then
         Item.AddPair('vendor_id', Product.VendorId);
    end;
    OutputJson(Root);
  finally
    Root.Free;
  end;
end;

procedure OutputAsText(Products: TList<TRepositoryProduct>);
begin
  for var Product in Products do
    WriteLn(Format('%s (%s)', [Product.Id, TVersion(Product.LatestVersion.Version).ToString]));
end;

procedure RunListRemoteCommand;
begin
  InitFolderBasedCommand(EnableLog);
  var Repo := CreateRepositoryManager(Config.Folders.CredentialsFile, FetchOptions, true);
  try
    var ListedProducts := TList<TRepositoryProduct>.Create;
    try
      for var Product in Repo.Products do
        if (Product.LatestVersion <> nil) and not Product.Internal and not (Product.LicenseStatus in [TLicenseStatus.none]) then
          ListedProducts.Add(Product);
      if UseJson then
        OutputAsJson(ListedProducts)
      else
        OutputAsText(ListedProducts);
    finally
      ListedProducts.Free;
    end;
  finally
    Repo.Free;
  end;
end;

procedure RegisterListRemoteCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('list-remote', '', 'list TMS Software products in remote repository available to be installed',
    '',
    'list-remote');

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

  AddCommand(cmd.Name, CommandGroups.Status, RunListRemoteCommand);
end;

end.

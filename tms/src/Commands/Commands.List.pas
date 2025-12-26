unit Commands.List;

interface

uses
  System.Generics.Collections, System.SysUtils, UCommandLine;

procedure RegisterListCommand;

implementation

uses
  Commands.CommonOptions, Commands.Logging, Commands.GlobalConfig, Status.Manager, Deget.CoreTypes, UJsonPrinter, System.JSON,
  Deget.Version;

var
  EnableLog: Boolean = False;
  Detailed: Boolean = False;
  UseJson: Boolean = False;

procedure OutputAsJson(Products: TList<TProductStatus>);
begin
  var Root := TJSONObject.Create;
  try
    for var Product in Products do
    begin
      var Item := TJSONObject.Create;
      Root.AddPair(Product.Id, Item);

      Item.AddPair('version', Product.Version.Normalized);
      Item.AddPair('version_type', Product.Version.VersionTypeId);
      Item.AddPair('name', Product.Name);
      Item.AddPair('server', Product.Server);
      Item.AddPair('pinned', Product.Pinned);
      if Product.Channel <> '' then
        Item.AddPair('channel', Product.Channel);
      if not Product.Fetched then
        Item.AddPair('local', not Product.Fetched);
      if Detailed then
      begin
        var IDEs: TJSONObject := nil;
        for var IDEName := Low(TIDEName) to High(TIDEName) do
        begin
          var PlatsItem: TJSONObject := nil;
          for var Plat := Low(TPlatform) to High(TPlatform) do
          begin
            var PlatStatus := Product.IDEStatus(IDEName).PlatformStatus(Plat);
            if PlatStatus.IsBuilt or PlatStatus.IsRegistered then
            begin
              if PlatsItem = nil then
              begin
                if IDEs = nil then
                begin
                  IDEs := TJSONObject.Create;
                  Item.AddPair('ides', IDEs);
                end;
                var IDEItem := TJSONObject.Create;
                IDEs.AddPair(IDEId[IDEName], IDEItem);
                PlatsItem := TJSONObject.Create;
                IDEItem.AddPair('platforms', PlatsItem);
              end;

              var PlatItem := TJSONObject.Create;
              PlatsItem.AddPair(PlatformId[Plat], PlatItem);

              PlatItem.AddPair('built', PlatStatus.IsBuilt);
              PlatItem.AddPair('registered', PlatStatus.IsRegistered);
              PlatItem.AddPair('registered_items', PlatStatus.RegisteredItems);
            end;
          end;
        end;
      end;
    end;
    OutputJson(Root);
  finally
    Root.Free;
  end;
end;

procedure OutputAsText(Products: TList<TProductStatus>);
begin
  for var Product in Products do
  begin
    var Line := Product.DisplayName;
    if not Product.Fetched then
      Line := Line + '*';
    WriteLn(Line);

    if Detailed then
    begin
      Writeln('server: ' + Product.Server);
      Writeln('pinned: ' + BoolToStr(Product.Pinned, true));
      var IDEPrinted := False;
      for var IDEName := Low(TIDEName) to High(TIDEName) do
        for var Plat := Low(TPlatform) to High(TPlatform) do
          if Product.IDEStatus(IDEName).PlatformStatus(Plat).IsBuilt
            and Product.IDEStatus(IDEName).PlatformStatus(Plat).IsRegistered then
          begin
            if not IDEPrinted then
            begin
              WriteLn('- ' + IDEId[IDEName]);
              IDEPrinted := True;
            end;

            WriteLn('  - ' + PlatformId[Plat]);
          end;
      WriteLn('');
    end;
  end;
end;

procedure RunListCommand;
begin
  InitFolderBasedCommand(EnableLog);
  var Manager := TStatusManager.Create(Config);
  try
    Manager.Update;

    if UseJson then
      OutputAsJson(Manager.Products)
    else
      OutputAsText(Manager.Products);
  finally
    Manager.Free;
  end;
end;

procedure RegisterListCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('list', '', 'list installed products',
    '',
    'list');

  var option := cmd.RegisterOption<Boolean>('log', '', 'enable logging for this command',
    procedure(const Value: Boolean)
    begin
      EnableLog := Value;
    end);
  option.HasValue := False;
  option.Hidden := True;

  option := cmd.RegisterOption<Boolean>('detailed', '', 'display ides and platforms installed for each product',
    procedure(const Value: Boolean)
    begin
      Detailed := Value;
    end);
  option.HasValue := False;

  option := cmd.RegisterOption<Boolean>('json', '', 'output data in JSON format',
    procedure(const Value: Boolean)
    begin
      UseJson := Value;
    end);
  option.HasValue := False;

  AddCommand(cmd.Name, CommandGroups.Status, RunListCommand);
end;

end.

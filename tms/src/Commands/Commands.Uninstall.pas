unit Commands.Uninstall;

interface

procedure RegisterUninstallCommand;

implementation

uses
  System.SysUtils, System.StrUtils, System.IOUtils, UCommandLine, UMultiLogger, System.Diagnostics,
  Commands.CommonOptions, Commands.Logging, Commands.GlobalConfig, Removal.Manager, Removal.Item, Actions.Build,
  UTmsBuildSystemUtils, Removal.FolderDeleter, Commands.Termination, UAppTerminated;
var
  ProductIds: TArray<string>;
//  NoBuild: Boolean = False;
  Cascade: Boolean = False;
  Force: Boolean = False;
  IncludeManual: Boolean = False;

procedure RunUninstallCommand;
begin
  InitFolderBasedCommand;
  EnableCtrlCTermination;

  var AllStopWatch := TStopWatch.Create;
  AllStopWatch.Start;

  try
    // Process items to be removed
    var Manager := TRemovalManager.Create(Config.Folders, Config.GetAllRootFolders);
    try
      try
        // Get the list of products to be removed
        Manager.Cascade := Cascade;
        Manager.Force := Force;
        Manager.IncludeManual := IncludeManual;
        Manager.ProcessSelected(ProductIds);

        // Delete the folders
        var Deleter := TFolderDeleter.Create;
        try
          for var Item in Manager.RemovalItems do
          begin
            var ProductFolder := Item.ProductPath;
            if ProductFolder <> '' then Deleter.AddFolder(ProductFolder);
          end;
          Deleter.DeleteAll;
        finally
          Deleter.Free;
        end;

        // Summary
        CheckAppTerminated;
        for var Item in Manager.RemovalItems do
          case Item.Status of
            TRemovalStatus.Failed:
              // this line should never be executed, actually, because if there are items failed, an exception will be
              // raised in Manager.ProcessSelected call.
              Logger.Info(Format('%s -> FAILED', [Item.ProductId]));
          else
            Logger.Info(Format('%s -> REMOVED', [Item.ProductId]));
          end;
      finally
  //      LogFetchSummary(Manager.FetchItems);
      end;
    finally
      Manager.Free;
    end;

    // Build to unregister deleted products
    // For now, I don't think there is a reason for -nobuild parameter
  //  if not NoBuild then
    ExecuteBuildAction(ProductIds, False);
  finally
    //remove even if there was an error trying to uninstall (maybe because there aren't any more products)
    RemoveFromWindowsPathIfNoProducts;
  end;

  AllStopWatch.Stop;
  Logger.Info('');
  Logger.Info('Uninstall finished. Elapsed time: ' + AllStopWatch.Elapsed.ToString);
end;

procedure RegisterUninstallCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('uninstall', '', 'uninstalls the specified product(s)',
    'Uninstall removes the product(s) files from the disk and then perform a build to unregister the removed products from the IDEs.',
    'uninstall <product-ids>');
  cmd.Examples.Add('uninstall tms.biz.aurelius');
  cmd.Examples.Add('uninstall tms.biz.aurelius tms.fnc.maps -cascade');
  cmd.Examples.Add('uninstall tms.biz.* tms.vcl.*');

  var option := cmd.RegisterUnNamedOption<string>('The ids of the products to be uninstalled', 'product-ids',
    procedure(const Value: string)
    begin
      ProductIds := ProductIds + SplitString(Value, ',');
    end);
  option.Required := True;
  option.AllowMultiple := True;

  option := cmd.RegisterOption<Boolean>('cascade', '', 'also uninstall dependencies of selected products',
    procedure(const Value: Boolean)
    begin
      Cascade := Value;
    end
  );
  option.HasValue := False;

  option := cmd.RegisterOption<Boolean>('force', '', 'force uninstall even if there are other products depending on it',
    procedure(const Value: Boolean)
    begin
      Force := Value;
    end
  );
  option.HasValue := False;

  //Let's avoid this option for the moment. It is safer if we uninstall only what we installed.
  //option := cmd.RegisterOption<Boolean>('include-manual', '', 'uninstall even those products that we didn''t install via tms install. this is a dangerous option, use with care!',
  //  procedure(const Value: Boolean)
 //   begin
 //     IncludeManual := Value;
 //   end
 // );
  option.HasValue := False;


//  RegisterNoBuildOption(cmd,
//    procedure(const Value: Boolean)
//    begin
//      NoBuild := Value;
//    end);

  AddCommand(cmd.Name, CommandGroups.Install, RunUninstallCommand);
end;

end.

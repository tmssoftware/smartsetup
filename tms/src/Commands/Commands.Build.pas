unit Commands.Build;

interface

procedure RegisterBuildCommand;

implementation

uses
  System.StrUtils, Commands.Logging, UCommandLine, Actions.Build, Commands.CommonOptions;

var
  FullBuild: Boolean = False;
  OnlyUnregister: Boolean = False;
  ProductIds: TArray<string>;

procedure RunBuildCommand;
begin
  InitFolderBasedCommand;
  ExecuteBuildAction(ProductIds, FullBuild, OnlyUnregister);
end;

procedure RegisterBuildCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('build', '', 'build, register and unregister all products',
    'Rebuilds all locally available products and registers them in the proper IDEs. Only products with modified files ' +
    'will be rebuilt. Missing/removed products will be unregistered.'  + sLineBreak +
    'You can optionally specify the ids of the products to be built. '+
    'If no product id is provided, it will build all installed products.' + sLineBreak +
    '''tms build'' does the "install" part of ''tms update''. Calling ''tms update'' is the same as calling ''tms fetch'' and then ''tms build''.' + sLineBreak,
    'build [<product-ids] [options]');

  var option := cmd.RegisterUnNamedOption<string>('the ids of the products to be built, optional', 'product-ids',
    procedure(const Value: string)
    begin
      ProductIds := ProductIds + SplitString(Value, ',');
    end);
  option.AllowMultiple := True;

  option := cmd.RegisterOption<Boolean>('full', '', 'force all products to be rebuilt even if not modified',
    procedure(const Value : Boolean)
    begin
      FullBuild := Value;
    end);
  option.HasValue := False;

  option := cmd.RegisterOption<Boolean>('unregister', '', 'unregister all products from the IDEs',
    procedure(const Value : Boolean)
    begin
      OnlyUnregister := Value;
    end);
  option.HasValue := False;
  option.Hidden := True;

  AddCommand(cmd.Name, CommandGroups.Install,  RunBuildCommand);
end;

end.

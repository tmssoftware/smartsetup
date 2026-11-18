unit Commands.Fetch;

interface

procedure RegisterFetchCommand;

implementation

uses
  System.StrUtils, UCommandLine, Commands.CommonOptions, Commands.Logging, Actions.Fetch,
  Actions.Build, Commands.GlobalConfig, Fetching.FetchExtraction;

var
  SkipExtraction: Boolean = False;
  ProductIds: TArray<string>;

procedure RunFetchCommand;
begin
  InitFolderBasedCommand;
  var Extraction := TFetchExtraction.Create(SkipExtraction, false, nil);
  if Length(ProductIds) = 0 then
    ExecuteFetchAction(ProductIds, TFetchMode.OnlyInstalled, Extraction)
  else
    ExecuteFetchAction(ProductIds, TFetchMode.DownloadNew, Extraction);
end;

procedure RegisterFetchCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('fetch', '', 'Downloads newer versions of currently installed products',
    'Checks for new versions of installed products and updates them to the latest versions.' + sLineBreak +
    'You can optionally specify the ids of the products to be downloaded. '+
    'If no product id is provided, it will download all newer versions of the installed products.' + sLineBreak +
    'If you specify the id of a product that is not installed, it will download it.' + sLineBreak +
    '''tms fetch'' does the "download" part of tms update. Calling ''tms update'' is the same as calling ''tms fetch'' and then ''tms build''.' + sLineBreak +
    'More information: https://doc.tmssoftware.com/smartsetup/reference/tms-fetch.html',
    'fetch [<product-ids>] [options]');

  cmd.Examples.Add('fetch');
  cmd.Examples.Add('fetch tms.biz.aurelius,tms.fnc.maps');
  cmd.Examples.Add('fetch tms.biz.* tms.vcl.*');

  var option := cmd.RegisterUnNamedOption<string>('the ids of the products to be downloaded, optional', 'product-ids',
    procedure(const Value: string)
    begin
      ProductIds := ProductIds + SplitString(Value, ',');
    end);
  option.AllowMultiple := True;

  RegisterRepoOption(cmd);

  option := cmd.RegisterOption<Boolean>('skip-extraction', '', 'only fetch the products, do not extract them to build.',
    procedure(const Value: Boolean)
    begin
      SkipExtraction := Value;
    end);
  option.HasValue := False;


  AddCommand(cmd.Name, CommandGroups.Install, RunFetchCommand);
end;

end.

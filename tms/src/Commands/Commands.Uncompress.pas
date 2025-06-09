unit Commands.Uncompress;

interface

uses
  System.SysUtils, System.StrUtils, VSoft.CommandLine.Options, UCommandLine, UMultiLogger;

procedure RegisterUncompressCommand;

implementation

uses
  Commands.CommonOptions, Commands.Logging, Commands.GlobalConfig, UGenericDecompressor, IOUtils, UTmsBuildSystemUtils;

var
  BundleFileNames: TArray<string>;
  TargetFolder: string;
  IgnoreProductName: boolean;

function GetProductName(const fname: string): string;
begin
  if IgnoreProductName then exit('');

  var idx := fname.IndexOf('_');
  if idx < 0 then raise Exception.Create('The filename "' + fname + '" doesn''t include the separator "_". We can''t infer the product name from it.');
  Result := fname.Substring(0, idx);

end;

procedure RunUncompressCommand;
begin
  InitFolderBasedCommand;
  for var bundle in BundleFileNames do
  begin
    var WildcardBundles := TDirectory.GetFiles(TPath.GetDirectoryName(TPath.GetFullPath(bundle)), TPath.GetFileName(TPath.GetFullPath(bundle)));
    for var WildcardBundle in WildcardBundles do
    begin
      var FinalTargetFolder := TPath.GetFullPath(TPath.Combine(TargetFolder, GetProductName(TPath.GetFileName(WildcardBundle))));
      TDirectory_CreateDirectory(TPath.GetDirectoryName(FinalTargetFolder));
      Logger.Info('Decompressing ' + TPath.GetFileName(WildcardBundle) + ' into ' + FinalTargetFolder + '.');
      TBundleDecompressor.ExtractCompressedFile(TPath.GetFullPath(WildcardBundle), FinalTargetFolder);
      Logger.Info('Decompressed ' + TPath.GetFileName(WildcardBundle) + ' into ' + FinalTargetFolder + '.');

    end;
  end;

end;

procedure RegisterUncompressCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('uncompress', '', 'Uncompresses a specific bundle',
    'Uncompresses a smart setup bundle into a folder of your choosing, inside a folder with the product id. You can run tms build after that to build and register the bundle.',
    'uncompress <bundle-filenames> <target-folder>');
  cmd.Examples.Add('uncompress c:\bundles\tms.biz.* c:\tms\Products\');

  var option := cmd.RegisterUnNamedOption<string>('The file(s) to uncompress. You can use wildcards like c:\bundles\*.*', 'bundle-filenames',
    procedure(const Value: string)
    begin
      BundleFileNames := BundleFileNames + [Value.Trim];
    end);
  option.Required := True;
  option.AllowMultiple := False;

  option := cmd.RegisterUnNamedOption<string>('The folder where to uncompress the bundles', 'target-folder',
    procedure(const Value: string)
    begin
      TargetFolder := Value.Trim;
    end);
  option.Required := True;
  option.AllowMultiple := False;

  option := cmd.RegisterOption<Boolean>('ignore-product-name', '',
     'Uncompress the bundle in the selected folder, do not append the product name to it.',
     procedure(const Value: Boolean)
     begin
       IgnoreProductName := Value;
      end);
  option.HasValue := False;


  AddCommand(cmd.Name, CommandGroups.Install, RunUncompressCommand);
end;

end.

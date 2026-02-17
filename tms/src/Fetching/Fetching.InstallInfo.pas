unit Fetching.InstallInfo;

interface

uses
  System.Generics.Collections, System.SysUtils, System.IOUtils, System.Classes, Fetching.InfoFile;

procedure GetFetchedProducts(const RootFolder: string; Products: TObjectList<TFetchInfoFile>); overload;
procedure GetFetchedProducts(const RootFolder: string; Products: THashSet<string>; const Condition: TFunc<TFetchInfoFile, boolean>); overload;
implementation

uses
  UTmsBuildSystemUtils, Commands.GlobalConfig;

procedure GetFetchedProducts(const RootFolder: string; Products: TObjectList<TFetchInfoFile>);
begin
  if not TDirectory.Exists(RootFolder) then exit;

  var Folders := TDirectory.GetDirectories(RootFolder, '*', TSearchOption.soTopDirectoryOnly);
  for var Folder in Folders do
  begin
    var Filename := TPath.Combine(Folder, TFetchInfoFile.FileName);
    if TFile.Exists(Filename) then  Products.Add(TFetchInfoFile.FromFile(Filename));
  end;

end;

procedure GetFetchedProducts(const RootFolder: string; Products: THashSet<string>; const Condition: TFunc<TFetchInfoFile, boolean>);
begin
  if not TDirectory.Exists(RootFolder) then exit;

  var Folders := TDirectory.GetDirectories(RootFolder, '*', TSearchOption.soTopDirectoryOnly);
  for var Folder in Folders do
  begin
    var Filename := TPath.Combine(Folder, TFetchInfoFile.FileName);
    if TFile.Exists(Filename) then
    begin
       var FetchInfoFile := TFetchInfoFile.FromFile(Filename);
       try
         if Condition(FetchInfoFile) then Products.Add(FetchInfoFile.ProductId);

       finally
         FetchInfoFile.Free;
       end;
    end;
  end;

end;

end.

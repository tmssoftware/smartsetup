unit UPackageCache;
{$i ../../tmssetup.inc}
interface
uses Generics.Collections, Generics.Defaults;

type
TPackageCache = class
private
  Files: TObjectDictionary<string, TDictionary<string, TArray<string>>>;
  function Make(const s1, s2: string): string;
  function Grow(var list: TArray<string>): integer;
  function GetFilesSingleExt(const BasePath, ext: string): TDictionary<string, TArray<string>>; overload;
public
  constructor Create;
  destructor Destroy; override;
  function GetFiles(const BasePath: string; const exts: TArray<string>): TArray<TDictionary<string, TArray<string>>>; overload;
  function GetFilesForPkg(const BasePath: string; const exts: TArray<string>; const FullPackageName: string): TArray<string>; overload;
end;

implementation
uses SysUtils, IOUtils, UTmsBuildSystemUtils;

{ TPackageCache }

constructor TPackageCache.Create;
begin
  Files := TObjectDictionary<string, TDictionary<string, TArray<string>>>.Create([doOwnsValues]);
end;

destructor TPackageCache.Destroy;
begin
  Files.Free;
  inherited;
end;

function TPackageCache.GetFilesSingleExt(const BasePath, ext: string):TDictionary<string, TArray<string>>;
var
  FileDict: TDictionary<string, TArray<string>>;
begin
  if not Files.TryGetValue(Make(BasePath, ext), FileDict) then
  begin
    var AllFiles := TList<string>.Create;
    try
      FindProjects(BasePath, ext, AllFiles, true);
      FileDict := TDictionary<string, TArray<string>>.Create;
      for var f in AllFiles do
      begin
        var fname := TPath.GetFileName(f).ToLowerInvariant;
        var FileList: TArray<string>;
        if FileDict.TryGetValue(fname, FileList) then
        begin
           var idx := Grow(FileList);
           FileList[idx] := f;
           FileDict.AddOrSetValue(fname, FileList); // the array could change address when resized. More elegant would be to hold a class with an array in the dictionary, but this adds yet another object to free.
        end else
        begin
          FileDict.Add(fname, TArray<string>.Create(f));
        end;
      end;
      Files.Add(Make(BasePath, ext), FileDict);
    finally
      AllFiles.Free;
    end;
  end;
  Result := FileDict;
end;

function TPackageCache.GetFiles(const BasePath: string; const exts: TArray<string>): TArray<TDictionary<string, TArray<string>>>;
begin
  Result := nil;
  for var ext in exts do
  begin
    Result := Result + [GetFilesSingleExt(BasePath, ext)];
  end;

  if Result = nil then raise Exception.Create('Internal error: exts array must be non-empty.');

end;


function TPackageCache.GetFilesForPkg(const BasePath: string; const exts: TArray<string>;
  const FullPackageName: string): TArray<string>;
begin
  for var ext in exts do
  begin
    var FileDict := GetFilesSingleExt(BasePath, ext);
    if FileDict.TryGetValue(FullPackageName.ToLowerInvariant + ext.ToLowerInvariant, Result) then exit;
  end;
  Result := nil;
end;

function TPackageCache.Grow(var list: TArray<string>): integer;
begin
  //We won't resize the array 1 by one, we will just keep '' at the end.
  Result := Length(List) - 1;
  while (Result >= 0) and (list[Result] = '') do dec(Result);
  if (Result = Length(list) - 1) then
  begin
    SetLength(list, Length(list) + 20);
  end;
  inc(Result);

end;

function TPackageCache.Make(const s1, s2: string): string;
begin
  Result := s1 + '|' + s2;
end;

end.

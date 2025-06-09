unit Deget.Filer.Types;

interface

uses
  System.Generics.Collections, System.SysUtils, Deget.CoreTypes, Deget.Nullable;

type
  TPackageUsage = (RuntimeAndDesignTime, Runtime, DesignTime);

  TIncludeFile = class
  private
    FFileName: string;
  public
    constructor Create(const AFileName: string);
    property FileName: string read FFileName write FFileName;
  end;

  TIncludeFiles<T: TIncludeFile, constructor> = class
  strict private
    FFiles: TObjectList<T>;
    FExt: string;
    function GetItem(Index: Integer): T;
  public
    constructor Create(const AExt: string);
    destructor Destroy; override;
    function Add(const AFileName: string): T;
    function Find(const AFileName: string): T;
    procedure Clear;
    function GetEnumerator: TEnumerator<T>;
    function Count: Integer;
    property Items[Index: Integer]: T read GetItem; default;
  end;

  TIncludeFiles = class(TIncludeFiles<TIncludeFile>)
  end;

  TPasIncludeFile = class(TIncludeFile)
  strict private
    FFormName: string;
  public
    constructor Create(const AFileName, AFormName: string);
    property FormName: string read FFormName write FFormName;
  end;

  TCppIncludeFile = class(TIncludeFile)
  strict private
    FFormName: string;
    FHeaderFile: string;
  public
    constructor Create(const AFileName, AFormName, AHeaderFile: string);
    property FormName: string read FFormName write FFormName;
    property HeaderFile: string read FHeaderFile write FHeaderFile;
  end;

  TRcCompileFile = class(TPasIncludeFile);

  TPasIncludeFiles = class(TIncludeFiles<TPasIncludeFile>)
  public
    procedure Add(const AFileName, AFormName: string);
  end;

  TRcCompileFiles = class(TPasIncludeFiles);

  TCppIncludeFiles = class(TIncludeFiles<TCppIncludeFile>)
  public
    procedure Add(const AFileName, AFormName, AHeaderFile: string);
  end;

  TPlatformAndConfig = record
  private
    FIDEName: TIDEName;
    FPlatform: Nullable<TPlatform>;
    FBuildConfig: Nullable<TBuildConfig>;
  public
    constructor Create(const AIDEName: TIDEName; const APlatform: Nullable<TPlatform>; const ABuildConfig: Nullable<TBuildConfig>);
    property IDEName: TIDEName read FIDEName;
    property Platform: Nullable<TPlatform> read FPlatform;
    property BuildConfig: Nullable<TBuildConfig> read FBuildConfig;
  end;

  TPropertyGroupEntry = class
  private
    FPlatformAndConfig: TPlatformAndConfig;
    FUnitSearchPath: Nullable<string>;
    FExeOutputPath: Nullable<string>;
    FDefines: Nullable<string>;
  public
    property PlatformAndConfig: TPlatformAndConfig read FPlatformAndConfig;
    property UnitSearchPath: Nullable<string> read FUnitSearchPath;
    property ExeOutputPath: Nullable<string> read FExeOutputPath;
    property Defines: Nullable<string> read FDefines;

    constructor Create(const APlatformAndConfig: TPlatformAndConfig; const AUnitSearchPath, AExeOutputPath, ADefines: Nullable<string>);

    function Clone(const AdaptPath: TFunc<string, string>): TPropertyGroupEntry;
  end;

  TPropertyGroupEntryList = class
  private
    FList: TObjectList<TPropertyGroupEntry>;
  public
    constructor Create;
    destructor Destroy; override;

    function Count: integer;
    procedure Add(const Entry: TPropertyGroupEntry);
    procedure CopyFrom(const Source: TPropertyGroupEntryList; const AdaptPath: TFunc<string, string>);

    property List: TObjectList<TPropertyGroupEntry> read FList;

    function GetValue(const Platform: TPlatform; const Config: string; const Getter: TFunc<TPropertyGroupEntry, Nullable<string>>): Nullable<string>;
    function GetValueWithInherited(const Platform: TPlatform; const Config, InheritedString: string; const Getter: TFunc<TPropertyGroupEntry, Nullable<string>>; const ConcatString: string): string;
  end;


implementation

{ TIncludeFile }

constructor TIncludeFile.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
end;

{ TPasIncludeFile }

constructor TPasIncludeFile.Create(const AFileName, AFormName: string);
begin
  inherited Create(AFileName);
  FFormName := AFormName;
end;

{ TPasIncludeFiles }

procedure TPasIncludeFiles.Add(const AFileName, AFormName: string);
begin
  inherited Add(AFileName).FormName := AFormName;
end;

{ TIncludeFiles<T> }

function TIncludeFiles<T>.Add(const AFileName: string): T;
begin
  if (FExt <> '') and (ExtractFileExt(AFileName).ToLower <> FExt) then
    raise Exception.CreateFmt('Extension %s expected for file %s', [FExt, AFileName]);

  Result := T.Create;
  FFiles.Add(Result);
  Result.FFileName := AFileName;
end;

procedure TIncludeFiles<T>.Clear;
begin
  FFiles.Clear;
end;

function TIncludeFiles<T>.Count: Integer;
begin
  Result := FFiles.Count;
end;

constructor TIncludeFiles<T>.Create(const AExt: string);
begin
  inherited Create;
  FFiles := TObjectList<T>.Create;
  FExt := AExt;
end;

destructor TIncludeFiles<T>.Destroy;
begin
  FFiles.Free;
  inherited;
end;

function TIncludeFiles<T>.Find(const AFileName: string): T;
begin
  for var I := 0 to FFiles.Count - 1 do
    if SameText(FFiles[I].FileName, AFileName) then
      Exit(FFiles[I]);
  Result := nil;
end;

function TIncludeFiles<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := FFiles.GetEnumerator;
end;

function TIncludeFiles<T>.GetItem(Index: Integer): T;
begin
  Result := FFiles[Index];
end;

{ TPlatformAndConfig }

constructor TPlatformAndConfig.Create(const AIDEName: TIDEName;
  const APlatform: Nullable<TPlatform>;
  const ABuildConfig: Nullable<TBuildConfig>);
begin
  FIDEName := AIDEName;
  FPlatform := APlatform;
  FBuildConfig := ABuildConfig;
end;

{ TPropertyGroupEntry }

function TPropertyGroupEntry.Clone(const AdaptPath: TFunc<string, string>): TPropertyGroupEntry;
begin
  var NewUnitSearchPath := FUnitSearchPath;
  if Assigned(AdaptPath) and FUnitSearchPath.HasValue then NewUnitSearchPath := AdaptPath(FUnitSearchPath);
  var NewExeOutputPath := FExeOutputPath;
  if Assigned(AdaptPath) and FExeOutputPath.HasValue then NewExeOutputPath := AdaptPath(FExeOutputPath);
  var NewDefines := FDefines;

  Result := TPropertyGroupEntry.Create(FPlatformAndConfig, NewUnitSearchPath, NewExeOutputPath, NewDefines);
end;

constructor TPropertyGroupEntry.Create(const APlatformAndConfig: TPlatformAndConfig;
  const AUnitSearchPath, AExeOutputPath, ADefines: Nullable<string>);
begin
  FPlatformAndConfig := APlatformAndConfig;
  FUnitSearchPath := AUnitSearchPath;
  FExeOutputPath := AExeOutputPath;
  FDefines := ADefines;
end;

{ TPropertyGroupEntryList }

procedure TPropertyGroupEntryList.Add(const Entry: TPropertyGroupEntry);
begin
  FList.Add(Entry);
end;

procedure TPropertyGroupEntryList.CopyFrom(
  const Source: TPropertyGroupEntryList; const AdaptPath: TFunc<string, string>);
begin
  FList.Clear;
  for var item in Source.List do
  begin
    Add(item.Clone(AdaptPath));
  end;
end;

function TPropertyGroupEntryList.Count: integer;
begin
  Result := FList.Count;
end;

constructor TPropertyGroupEntryList.Create;
begin
  FList := TObjectList<TPropertyGroupEntry>.Create;
end;

destructor TPropertyGroupEntryList.Destroy;
begin
  FList.Destroy;
  inherited;
end;

function TPropertyGroupEntryList.GetValue(const Platform: TPlatform;
  const Config: string;
  const Getter: TFunc<TPropertyGroupEntry, Nullable<string>>): Nullable<string>;
begin
  //The order is:
  // If the value for Platform and Config is set, we use that.
  // If not, if the value for (null, Config) is set we use that.
  // If not, if the value for (null, null) is set, we use that.
  // There is no way to set (Platform, null), we can't set an output path for all Win64 (debug and release).
  Result := SNull;
  for var Entry in FList do
  begin
    if Entry.PlatformAndConfig.FPlatform.HasValue and (Entry.FPlatformAndConfig.FPlatform.Value = Platform) then
    begin
      if Entry.PlatformAndConfig.BuildConfig.HasValue and SameText(BuildConfigs[Entry.PlatformAndConfig.BuildConfig.Value], Config) then
      begin
        var Value := Getter(Entry);
        if (Value.HasValue) and (Value <> '') then exit(Value); //perfect match, we can exit.
      end;
    end;

    if not Entry.PlatformAndConfig.FPlatform.HasValue then
    begin
      if Entry.PlatformAndConfig.BuildConfig.HasValue and SameText(BuildConfigs[Entry.PlatformAndConfig.BuildConfig.Value], Config) then
      begin
        var Value := Getter(Entry);
        if (Value.HasValue) and (Value <> '') then Result := Value; //(null, config). We might still find the perfect match.
      end;
    end;

    if not Entry.PlatformAndConfig.FPlatform.HasValue then
    begin
      if not Entry.PlatformAndConfig.BuildConfig.HasValue then
      begin
        var Value := Getter(Entry);
        if (Value.HasValue) and (Value <> '') and (not Result.HasValue) then Result := Value; //(null, null). We might still find the perfect match, or (null, config).
      end;
    end;

  end;
end;

function Find(const s: string; const Values: TArray<string>): integer;
begin
  for var i := Low(Values) to High(Values) do
  begin
    if SameText(s, Values[i].Trim) then exit(i);
  end;

  Result := -1;
end;

function Remove(const ParsedValues: TArray<string>; const InheritedValue: integer; const ConcatString: string): string;
begin
  Result := '';
  for var i := Low(ParsedValues) to High(ParsedValues) do
  begin
    if i = InheritedValue then continue;
    if Result <> '' then Result := Result + ConcatString;
    Result := Result + ParsedValues[i];
  end;
end;

//Returns the value concatenated with the base values. Say, a base config defines "DEBUG" and a more
//specialized config returns "RTTI". This method will return "DEBUG;RTTI".
function TPropertyGroupEntryList.GetValueWithInherited(const Platform: TPlatform;
  const Config, InheritedString: string;
  const Getter: TFunc<TPropertyGroupEntry, Nullable<string>>; const ConcatString: string): string;
var
  Values: Array[0..3] of string;
begin
  //Order is important. We must return "DEBUG;RTTI", not "RTTI;DEBUG". The more specialized values might override the base values.
  //The order is:
  // (Platform, Config)
  // (null, Config)
  // (Platform, null)
  // (null, null)
  // Also there is a checkbox "inherit" in the ide where you can choose if to inherit it or not.
  // If inherit is off, then the node will be:
  // <DCC_Define>SomeConfig</DCC_Define>
  // instead of
  // <DCC_Define>SomeConfig;$(DCC_Define)</DCC_Define>
  for var Entry in FList do
  begin
    if Entry.PlatformAndConfig.FPlatform.HasValue and (Entry.FPlatformAndConfig.FPlatform.Value = Platform) then
    begin
      if Entry.PlatformAndConfig.BuildConfig.HasValue and SameText(BuildConfigs[Entry.PlatformAndConfig.BuildConfig.Value], Config) then
      begin
        var Value := Getter(Entry);
        if (Value.HasValue) and (Value <> '') then Values[3] := Value.Value;
      end;
    end;

    if not Entry.PlatformAndConfig.FPlatform.HasValue then
    begin
      if Entry.PlatformAndConfig.BuildConfig.HasValue and SameText(BuildConfigs[Entry.PlatformAndConfig.BuildConfig.Value], Config) then
      begin
        var Value := Getter(Entry);
        if (Value.HasValue) and (Value <> '') then Values[2] := Value; //(null, config).
      end;
    end;

    if Entry.PlatformAndConfig.FPlatform.HasValue and (Entry.FPlatformAndConfig.FPlatform.Value = Platform) then
    begin
      if not Entry.PlatformAndConfig.BuildConfig.HasValue then
      begin
        var Value := Getter(Entry);
        if (Value.HasValue) and (Value <> '') then Values[1] := Value; //(Platform, null).
      end;
    end;


    if not Entry.PlatformAndConfig.FPlatform.HasValue then
    begin
      if not Entry.PlatformAndConfig.BuildConfig.HasValue then
      begin
        var Value := Getter(Entry);
        if (Value.HasValue) and (Value <> '') then Values[0] := Value; //(null, null).
      end;
    end;

  end;

  Result := '';
  for var i := Low(Values) to High(Values) do
  begin
    var Value := Values[i];
    if Value = '' then continue;
    var ParsedValues := Value.Split([ConcatString], TStringSplitOptions.ExcludeEmpty);
    var InheritedValue := Find(InheritedString, ParsedValues);
    if InheritedValue < 0 then Result := '' else Value := Remove(ParsedValues, InheritedValue, ConcatString);
    if Value = '' then continue;

    if Result <> '' then Result := Result + ConcatString;
    Result := Result + Value;
  end;

end;


{ TCppIncludeFile }

constructor TCppIncludeFile.Create(const AFileName, AFormName,
  AHeaderFile: string);
begin
  FFileName := AFileName;
  FFormName := AFormName;
  FHeaderFile := AHeaderFile;
end;

{ TCppIncludeFiles }

procedure TCppIncludeFiles.Add(const AFileName, AFormName, AHeaderFile: string);
begin
  var Item := inherited Add(AFileName);
  Item.FormName := AFormName;
  Item.HeaderFile := AHeaderFile;

end;

end.

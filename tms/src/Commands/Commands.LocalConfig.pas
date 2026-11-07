unit Commands.LocalConfig;

interface
uses Classes, SysUtils, UConfigDefinition, BBYaml.Writer, UConfigSources, Deget.Nullable;

type
TSourcedConfig = class
  private
    class var
      OverrideConfigSources: Nullable<TConfigSourceSet>;
    var
      DefaultConfigSources: TConfigSourceSet;
      FInstance: TConfigDefinition;
    function ConfigSources: TConfigSourceSet;

  public
    constructor Create(const aDefaultConfigSources: TConfigSourceSet);
    destructor Destroy; override;

    property Instance: TConfigDefinition read FInstance;
    function FileName: string;
    function SingleSource: TConfigSource;
    function WritingFormat: TWritingFormat;
    class procedure SetOverrideConfigSources(const aConfigSource: TConfigSourceSet); static;

end;


implementation
uses Commands.GlobalConfig;

constructor TSourcedConfig.Create(const aDefaultConfigSources: TConfigSourceSet);
begin
  inherited Create;
  DefaultConfigSources := aDefaultConfigSources;
  FInstance := NewConfig(ConfigSources);
end;

destructor TSourcedConfig.Destroy;
begin
  FInstance.Free;
  inherited;
end;

function TSourcedConfig.ConfigSources: TConfigSourceSet;
begin
  if OverrideConfigSources.HasValue then exit(OverrideConfigSources.Value);
  exit(DefaultConfigSources);
end;


class procedure TSourcedConfig.SetOverrideConfigSources(const aConfigSource: TConfigSourceSet);
begin
  if OverrideConfigSources.HasValue then raise Exception.Create('-config-source can only be specified once.');
  OverrideConfigSources := aConfigSource;
end;

function TSourcedConfig.SingleSource: TConfigSource;
begin
  Result := GetConfigSingleSource(ConfigSources);
end;

function TSourcedConfig.FileName: string;
begin
  Result := GetConfigFileName(ConfigSources);
end;

function TSourcedConfig.WritingFormat: TWritingFormat;
begin
  Result := GetWritingFormat(ConfigSources);
end;

end.

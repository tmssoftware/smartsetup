unit UConfigSources;
{$i ../../tmssetup.inc}

interface
type
  TConfigSource =
  (
    Main,
    Global,
    Local
  );

  TConfigSourceSet = Set of TConfigSource;
const
  AllConfigSources = [Low(TConfigSource)..High(TConfigSource)];

implementation

end.

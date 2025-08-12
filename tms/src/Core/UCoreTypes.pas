unit UCoreTypes;
{$i ../../tmssetup.inc}

interface

type
  TEngineLevel = (Project, Ide, Platform, Package);

type
    TFramework = 0..10;
    TFrameworkSet = set of TFramework;

    TOperatingSystem = (windows, linux, mac);
    TOperatingSystemSet = set of TOperatingSystem;

implementation

end.

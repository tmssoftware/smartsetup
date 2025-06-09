unit UProjectBuilderInterface;
{$i ../../tmssetup.inc}

interface
uses UBuildInfo;

type
  IProjectBuilder = interface
    procedure Build(BuildInfo: TBuildInfo);
  end;
implementation

end.

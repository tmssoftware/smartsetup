unit UProjectInstallerConstants;
{$i ../../tmssetup.inc}

interface
type
  InstallerConstants = record
  public
  const
    UninstallExtension = '.uninstall.json';

    ProjectNameJsonId = 'projectName';
    ProjectLinksJsonId = 'projectLinks';
    ProjectFileLinksJsonId = 'projectFileLinks';
  end;

implementation

end.

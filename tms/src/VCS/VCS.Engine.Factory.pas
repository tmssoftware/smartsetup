unit VCS.Engine.Factory;
{$I ../../tmssetup.inc}

interface
uses VCS.CoreTypes, VCS.Engine.Virtual;

type
  TVCSFactory = class
  private
    class var FInstance: TVCSFactory;
  public
    class constructor Create;
    class destructor Destroy;

    destructor Destroy; override;

    class function Instance: TVCSFactory;

    function GetEngine(const Protocol: TVCSProtocol): IVCSEngine;
  end;

implementation
uses SysUtils, VCS.Engine.Git, VCS.Engine.Svn, VCS.Engine.ZipFile, Commands.GlobalConfig;

{ TVCSFactory }

class constructor TVCSFactory.Create;
begin
  FInstance := TVCSFactory.Create;
end;

class destructor TVCSFactory.Destroy;
begin
  FInstance.Free;
end;

destructor TVCSFactory.Destroy;
begin
  inherited;
end;

function TVCSFactory.GetEngine(const Protocol: TVCSProtocol): IVCSEngine;
begin
  case Protocol of
    TVCSProtocol.Git: exit(TGitEngine.Create(Config.GitConfig.GitCommand, Config.GitConfig.Clone, Config.GitConfig.Pull));
    TVCSProtocol.Svn: exit(TSvnEngine.Create(Config.SvnConfig.SvnCommand, Config.SvnConfig.Checkout, Config.SvnConfig.Update));
    TVCSProtocol.ZipFile: exit(TZipFileEngine.Create);
    else raise Exception.Create('Invalid VCS protocol.');
  end;
end;

class function TVCSFactory.Instance: TVCSFactory;
begin
  exit(FInstance);
end;

end.

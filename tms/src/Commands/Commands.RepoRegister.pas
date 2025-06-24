unit Commands.RepoRegister;
{$I ../../tmssetup.inc}
interface
procedure RegisterRepoRegisterCommand;

implementation
uses VSoft.CommandLine.Options, UCommandLine, Commands.CommonOptions,
     VCS.CoreTypes, VCS.Registry, Commands.GlobalConfig, VCS.Manager, Commands.Logging,
     UMultiLogger, ULogger;

var
  Protocol: TVCSProtocol;
  Url: String;

procedure RunRepoRegisterCommand;
begin
  InitFolderBasedCommand;
  var HasErrors := true;
  Logger.StartSection(TMessageType.VCSRegister, 'Registering repository at ' + Url);
  try
    Logger.Info('Registering repository at ' + Url);
    //To get the product id, we need tmsbuild.yaml.
    var Product := TVCSManager.GetProduct(Protocol, Url);
    try
      RegisteredVCSRepos.Add(Product.Id, Protocol, Url, Product.Name, Product.Description, TProductRegistry.LocalServer.Name);
      RegisteredVCSRepos.Save;
      Logger.Info('Repository at "' + Url + '" registered with the name "' + Product.Id + '"');
    finally
      Product.Free;
    end;
    HasErrors := false;
  finally
    Logger.FinishSection(TMessageType.VCSRegister, HasErrors);
  end;
end;


procedure RegisterRepoRegisterCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('repo-register', '', 'Registers a GIT or SVN repository to be used with smart setup.',
    'Use this command to register a GIT or SVN repository that supports smartsetup, so the system knows where to find it.',
    'repo-register <GIT/SVN> <url>');

  var option := cmd.RegisterUnNamedOption<string>('Type of version control used. Can be GIT or SVN', 'repo-type',
    procedure(const Value: string)
    begin
      Protocol := TRegisteredProduct.GetProtocolFromString(Value, false);
    end);
  option.Required := True;
  option.AllowMultiple := False;

  option := cmd.RegisterUnNamedOption<string>('Url of the remote repository', 'url',
  procedure(const Value: string)
  begin
    Url := Value;
  end);
  option.Required := True;
  option.AllowMultiple := False;

  cmd.Examples.Add('repo-register git https://github.com/landgraf-dev/aws-sdk-delphi.git');

  AddCommand(cmd.Name, CommandGroups.Config, RunRepoRegisterCommand);
end;
end.

unit Commands.RepoUnregister;
{$I ../../tmssetup.inc}
interface
procedure RegisterRepoUnregisterCommand;

implementation
uses VSoft.CommandLine.Options, UCommandLine, Commands.CommonOptions,
     VCS.CoreTypes, VCS.Registry, Commands.GlobalConfig, VCS.Manager, Commands.Logging,
     UMultiLogger, ULogger, StrUtils, UTmsBuildSystemUtils, IOUtils, SysUtils;


var
  ProductIds: TArray<string>;

procedure RunRepoUnregisterCommand;
begin
  InitFolderBasedCommand;
  var HasErrors := true;
  Logger.StartSection(TMessageType.VCSRegister, 'Unegistering products from repositories');
  try
    for var ProductId in ProductIds do
    begin
      try
        if not RegisteredVCSRepos.Remove(ProductId) then
        begin
          HasErrors := true;
          Logger.Error('Cannot unregister ' + ProductId + '. It is not registered.');
          continue;
        end;

        Logger.Info('Unregistered product ' + ProductId);
      except on ex: Exception do
        Logger.Error('Cannot unregister ' + ProductId + '. ' + ex.Message);
      end;
    end;
  finally
    Logger.FinishSection(TMessageType.VCSRegister, HasErrors);
  end;
end;


procedure RegisterRepoUnregisterCommand;
begin
  var cmd := TOptionsRegistry.RegisterCommand('repo-unregister', '', 'Unregisters GIT or SVN repositories, or ZipFiles, that were previously registered with repo-register.',
    'Use this command to unregister one or many GIT or SVN repositories, or ZipFiles, that were previously registered with repo-register.',
    'repo-unregister <product-ids>');

  var option := cmd.RegisterUnNamedOption<string>('product-id to unregister', 'product-id',
    procedure(const Value: string)
    begin
      ProductIds := ProductIds + SplitString(Value, ',');
    end);
  option.Required := True;
  option.AllowMultiple := true;

  cmd.Examples.Add('repo-unregister tms.example.product');
  cmd.Examples.Add('repo-unregister tms.example.product1 tms.example.product2');

  AddCommand(cmd.Name, CommandGroups.Config, RunRepoUnregisterCommand);
end;
end.

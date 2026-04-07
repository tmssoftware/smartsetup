---
uid: SmartSetup.Command.Index
---

# Command Reference

This section documents all commands available in TMS Smart Setup. Run `tms help <command>` at any time to see usage information directly in the terminal.

If you are new to TMS Smart Setup, these are the commands you will use most often:

| Command                                            | Description                                        |
| -------------------------------------------------- | -------------------------------------------------- |
| [credentials](xref:SmartSetup.Command.Credentials) | Set the credentials to access a remote repository. |
| [list-remote](xref:SmartSetup.Command.ListRemote)  | List products available to be installed.           |
| [install](xref:SmartSetup.Command.Install)         | Install the specified products.                    |
| [help](xref:SmartSetup.Command.Help)               | Display usage information for a specific command.  |

---

## All commands

### Installing and updating

- [install](xref:SmartSetup.Command.Install): Downloads and installs the specified product(s), then builds and registers them in the IDEs.
- [uninstall](xref:SmartSetup.Command.Uninstall): Removes the specified product(s) from disk and unregisters them from the IDEs.
- [update](xref:SmartSetup.Command.Update): Updates currently installed products to their latest versions.
- [fetch](xref:SmartSetup.Command.Fetch): Downloads newer versions of currently installed products.
- [build](xref:SmartSetup.Command.Build): Builds, registers, and unregisters all locally available products.
- [pin](xref:SmartSetup.Command.Pin): Keeps the specified products at their current installed versions.
- [unpin](xref:SmartSetup.Command.Unpin): Removes the version pin from the specified products.
- [restore](xref:SmartSetup.Command.Restore): Restores a set of products from a snapshot file.
- [uncompress](xref:SmartSetup.Command.Uncompress): Extracts a Smart Setup bundle into a local folder.

### Informative

- [list](xref:SmartSetup.Command.List): Lists all products currently installed in the Smart Setup environment.
- [list-remote](xref:SmartSetup.Command.ListRemote): Lists products available for installation in the configured remote repositories.
- [versions-remote](xref:SmartSetup.Command.VersionsRemote): Lists all installable versions of a specific product from the configured remote repositories.
- [info](xref:SmartSetup.Command.Info): Displays information about the current folder and the tms tool itself.
- [snapshot](xref:SmartSetup.Command.Snapshot): Saves the list of currently installed products and their versions to a file.
- [server-list](xref:SmartSetup.Command.ServerList): Lists the servers configured in Smart Setup.
- [log-view](xref:SmartSetup.Command.LogView): Opens the log file in the default viewer.

### Configuration

- [credentials](xref:SmartSetup.Command.Credentials): Sets or displays the credentials used to access a remote repository.
- [spec](xref:SmartSetup.Command.Spec): Creates a new `tmsbuild.yaml` product definition file.
- [config](xref:SmartSetup.Command.Config): Opens the configuration file in the default editor for YAML files.
- [config-write](xref:SmartSetup.Command.ConfigWrite): Saves the current configuration to `tms.config.yaml`.
- [config-read](xref:SmartSetup.Command.ConfigRead): Reads a setting from the Smart Setup configuration file.
- [server-enable](xref:SmartSetup.Command.ServerEnable): Enables or disables a server in the Smart Setup configuration.
- [server-add](xref:SmartSetup.Command.ServerAdd): Adds a new server to the Smart Setup configuration.
- [server-remove](xref:SmartSetup.Command.ServerRemove): Removes a server from the Smart Setup configuration.

### Check and Repair

- [doctor](xref:SmartSetup.Command.Doctor): Checks and optionally fixes common problems in your Delphi installation.

### tms tool itself

- [version](xref:SmartSetup.Command.Version): Prints the current version of the tms utility.
- [self-update](xref:SmartSetup.Command.SelfUpdate): Updates tms Smart Setup to the latest available version.
- [help](xref:SmartSetup.Command.Help): Displays usage information for a specific command.

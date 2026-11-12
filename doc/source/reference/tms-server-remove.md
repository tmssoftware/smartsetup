---
uid: SmartSetup.Command.ServerRemove
---

# tms server-remove

Removes a server from the Smart Setup configuration.

## Synopsis

```shell
tms server-remove <name> [<global-options>]
```

## Description

Deletes a server entry from `tms.config.yaml`, or from the file specified with `-config-source`. If the specified server does not exist in that file, the command returns an error.

The command loads only the file it writes. If another configuration file also defines the server, the server stays available after you remove it, and you need to run the command again with the `-config-source` of that file.

## Arguments

| Argument | Description                                         |
| -------- | --------------------------------------------------- |
| `<name>` | Name of the server to remove. Required.             |

## Options

| Option                    | Description |
| ------------------------- | ----------- |
| `-config-source:<source>` | The configuration file to remove the server from: `main` (`tms.config.yaml`), `local` (`tms.config.local.yaml`) or `global` (`tms.config.global.yaml` in the `.smartsetup` folder of your user profile). Defaults to `main`. |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Removes a server named `myserver`:

```shell
tms server-remove myserver
```

## See Also

- [tms server-list](xref:SmartSetup.Command.ServerList)
- [tms server-add](xref:SmartSetup.Command.ServerAdd)
- [tms server-enable](xref:SmartSetup.Command.ServerEnable)

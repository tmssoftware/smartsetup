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

Deletes a server entry from `tms.config.yaml`. If the specified server does not exist, the command returns an error.

## Arguments

| Argument | Description                                         |
| -------- | --------------------------------------------------- |
| `<name>` | Name of the server to remove. Required.             |

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

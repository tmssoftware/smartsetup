---
uid: SmartSetup.Command.ServerEnable
---

# tms server-enable

Enables or disables a server in the Smart Setup configuration.

## Synopsis

```shell
tms server-enable <name> [<enable>] [<global-options>]
```

## Description

Changes the enabled state of an existing server in `tms.config.yaml`. If the specified server does not exist, the command returns an error.

Disabled servers are not contacted during [tms fetch](xref:SmartSetup.Command.Fetch), [tms install](xref:SmartSetup.Command.Install), or [tms update](xref:SmartSetup.Command.Update) operations.

## Arguments

| Argument     | Description                                                                             |
| ------------ | --------------------------------------------------------------------------------------- |
| `<name>`     | Name of the server to enable or disable. Required.                                      |
| `[<enable>]` | `true` to enable the server, `false` to disable it. Defaults to `true` if omitted.    |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Enables a server named `community`:

```shell
tms server-enable community
```

Disables a server named `community`:

```shell
tms server-enable community false
```

## See Also

- [tms server-list](xref:SmartSetup.Command.ServerList)
- [tms server-add](xref:SmartSetup.Command.ServerAdd)
- [tms server-remove](xref:SmartSetup.Command.ServerRemove)

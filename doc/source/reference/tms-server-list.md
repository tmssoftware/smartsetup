---
uid: SmartSetup.Command.ServerList
---

# tms server-list

Lists the servers configured in Smart Setup.

## Synopsis

```shell
tms server-list [<options>] [<global-options>]
```

## Description

Displays all servers registered in `tms.config.yaml`, along with their type, URL, and enabled/disabled status. Built-in servers (such as `tms` and `community`) show only their name and status since their URL is managed internally.

## Options

| Option   | Description                              |
| -------- | ---------------------------------------- |
| `-json`  | Outputs the server list in JSON format.  |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Lists all configured servers:

```shell
tms server-list
```

Lists all configured servers in JSON format:

```shell
tms server-list -json
```

## See Also

- [tms server-add](xref:SmartSetup.Command.ServerAdd)
- [tms server-enable](xref:SmartSetup.Command.ServerEnable)
- [tms server-remove](xref:SmartSetup.Command.ServerRemove)

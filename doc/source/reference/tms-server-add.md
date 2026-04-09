---
uid: SmartSetup.Command.ServerAdd
---

# tms server-add

Adds a new server to the Smart Setup configuration.

## Synopsis

```shell
tms server-add <name> <type> <url> [<enable>] [<global-options>]
```

## Description

Registers a new download server in `tms.config.yaml`. If a server with the specified name already exists, the command returns an error.

The server type controls how Smart Setup communicates with the server:

- `api` — a TMS API server that provides licensed products. Requires credentials set via [tms credentials](xref:SmartSetup.Command.Credentials).
- `zipfile` — a server that provides the list of installable products as a download zip file. The URL can be an `https://` or `file://` path.

The server is enabled by default. Pass `false` as the last argument to add the server in a disabled state.

## Arguments

| Argument     | Description                                                              |
| ------------ | ------------------------------------------------------------------------ |
| `<name>`     | A unique name for the server. Required.                                  |
| `<type>`     | Server type: `api` or `zipfile`. Required.                               |
| `<url>`      | URL of the server. Required.                                             |
| `[<enable>]` | Whether to enable the server immediately. Defaults to `true` if omitted. |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Adds an enabled zipfile server at a remote URL:

```shell
tms server-add myserver zipfile https://mycompany.com/packages
```

Adds a zipfile server pointing to a local zip file:

```shell
tms server-add myserver zipfile file://c:\packages\myzip.zip
```

Adds a server in disabled state:

```shell
tms server-add myserver zipfile https://mycompany.com/packages false
```

## See Also

- [tms server-list](xref:SmartSetup.Command.ServerList)
- [tms server-enable](xref:SmartSetup.Command.ServerEnable)
- [tms server-remove](xref:SmartSetup.Command.ServerRemove)
- [tms credentials](xref:SmartSetup.Command.Credentials)

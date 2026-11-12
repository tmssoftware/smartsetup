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

Registers a new download server in `tms.config.yaml`, or in the file specified with `-config-source`. If a server with the specified name already exists in that file, the command returns an error.

The command loads only the file it writes. If a file with higher priority defines a server with the same name, the command succeeds, but the values in that file keep their priority over the ones you add. For example, a server in `tms.config.local.yaml` has priority over one with the same name in `tms.config.yaml`.

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

## Options

| Option                    | Description |
| ------------------------- | ----------- |
| `-config-source:<source>` | The configuration file to add the server to: `main` (`tms.config.yaml`), `local` (`tms.config.local.yaml`) or `global` (`tms.config.global.yaml` in the `.smartsetup` folder of your user profile). Defaults to `main`. |

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

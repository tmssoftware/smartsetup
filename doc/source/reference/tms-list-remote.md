---
uid: SmartSetup.Command.ListRemote
---

# tms list-remote

Lists products available for installation in the configured remote repositories.

## Synopsis

```shell
tms list-remote [<options>] [<global-options>]
```

## Description

Queries each enabled server defined in the configuration and prints the products available for installation. For each product, the output shows the product ID, its latest version, and the server it comes from.

Only products the current account is licensed to install are included in the output. Products that are internal or unlicensed are excluded.

Use `-server` to limit results to a single server. Use `-json` to produce machine-readable output suitable for scripting.

## Options

| Option              | Description                                                         |
| ------------------- | ------------------------------------------------------------------- |
| `-json`             | Outputs the product list in JSON format instead of plain text.      |
| `-server:<server>`  | Limits results to the specified server name.                        |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Lists all products available from all enabled remote servers:

```shell
tms list-remote
```

Lists products available from a specific server:

```shell
tms list-remote -server:tms
```

Outputs the available product list as JSON, for use in scripts or automation:

```shell
tms list-remote -json
```

## See Also

- [tms list](xref:SmartSetup.Command.List)
- [tms install](xref:SmartSetup.Command.Install)

---
uid: SmartSetup.Command.VersionsRemote
---

# tms versions-remote

Lists all installable versions of a specific product from the configured remote repositories.

## Synopsis

```shell
tms versions-remote <product-id> [<options>] [<global-options>]
```

## Description

Queries the enabled servers defined in the configuration and returns all available versions of the specified product. Only versions the current account is licensed to install are included.

Use `-json` to produce machine-readable output suitable for scripting.

## Arguments

| Argument       | Description                                             |
| -------------- | ------------------------------------------------------- |
| `<product-id>` | The ID of the product to query. Required.               |

## Options

| Option  | Description                                         |
| ------- | --------------------------------------------------- |
| `-json` | Outputs the version list in JSON format.            |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Lists all installable versions of a product:

```shell
tms versions-remote tms.biz.aurelius
```

Outputs the version list as JSON:

```shell
tms versions-remote tms.biz.aurelius -json
```

## See Also

- [tms list-remote](xref:SmartSetup.Command.ListRemote)
- [tms install](xref:SmartSetup.Command.Install)

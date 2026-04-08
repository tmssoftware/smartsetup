---
uid: SmartSetup.Command.Fetch
---

# tms fetch

Downloads newer versions of currently installed products.

## Synopsis

```shell
tms fetch [<product-ids>] [<global-options>]
```

## Description

Checks for new versions of installed products and downloads them locally. If no product IDs are provided, all installed products are checked and any newer versions are downloaded.

If a product ID is specified that is not currently installed, it is downloaded anyway.

tms fetch performs the download step of [tms update](xref:SmartSetup.Command.Update). Running [tms update](xref:SmartSetup.Command.Update) is equivalent to running tms fetch followed by [tms build](xref:SmartSetup.Command.Build).

For licensed products from a TMS API server, fetch contacts the server using stored credentials, checks for a newer version, and downloads a compressed bundle. The bundle is extracted into the products folder, making the full product contents — source code, documentation, and other files — available locally.

For public products from a community server, fetch runs a `git clone` if the product is new, or a `git pull` if a working copy already exists. The full product contents are available in the products folder after fetch completes.

## Arguments

| Argument        | Description                                                                                                      |
| --------------- | ---------------------------------------------------------------------------------------------------------------- |
| `<product-ids>` | One or more product IDs to download. Multiple IDs can be separated by commas or provided as separate arguments. Supports wildcard patterns such as `tms.biz.*`. Optional — if omitted, all installed products are updated. |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Downloads newer versions of all currently installed products:

```shell
tms fetch
```

Downloads newer versions of two specific products:

```shell
tms fetch tms.biz.aurelius,tms.fnc.maps
```

Downloads newer versions of all products in two product families using wildcards:

```shell
tms fetch tms.biz.* tms.vcl.*
```

## See Also

- [tms build](xref:SmartSetup.Command.Build)
- [tms update](xref:SmartSetup.Command.Update)
- [tms install](xref:SmartSetup.Command.Install)

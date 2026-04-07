---
uid: SmartSetup.Command.Update
---

# tms update

Updates currently installed products to their latest versions.

## Synopsis

```shell
tms update [<product-ids>] [<global-options>]
```

## Description

Checks for newer versions of installed products, downloads them, and rebuilds the packages. Running [tms update](xref:SmartSetup.Command.Update) is equivalent to running [tms fetch](xref:SmartSetup.Command.Fetch) followed by [tms build](xref:SmartSetup.Command.Build).

If no product IDs are provided, all installed products are updated. If one or more product IDs are specified, only those products are updated. Specifying a product ID that is not currently installed raises an error.

Pinned products are not updated. Use [tms unpin](xref:SmartSetup.Command.Unpin) first to allow a pinned product to be updated.

## Arguments

| Argument        | Description                                                                                                                                                                          |
| --------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `<product-ids>` | One or more product IDs to update. Multiple IDs can be separated by commas or provided as separate arguments. Supports wildcard patterns such as `tms.biz.*`. Optional — if omitted, all installed products are updated. |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Updates all installed products:

```shell
tms update
```

Updates two specific products:

```shell
tms update tms.biz.aurelius,tms.fnc.maps
```

Updates all products in two product families:

```shell
tms update tms.biz.* tms.vcl.*
```

## See Also

- [tms fetch](xref:SmartSetup.Command.Fetch)
- [tms build](xref:SmartSetup.Command.Build)
- [tms install](xref:SmartSetup.Command.Install)
- [tms pin](xref:SmartSetup.Command.Pin)

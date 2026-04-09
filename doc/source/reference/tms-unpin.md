---
uid: SmartSetup.Command.Unpin
---

# tms unpin

Removes the version pin from the specified products.

## Synopsis

```shell
tms unpin <product-ids> [<global-options>]
```

## Description

Clears the pinned flag from one or more installed products. After unpinning, the products will be included in version updates when running [tms update](xref:SmartSetup.Command.Update).

## Arguments

| Argument        | Description                                                                                                                                                                   |
| --------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `<product-ids>` | One or more product IDs to unpin. Required. Multiple IDs can be separated by commas or provided as separate arguments. Supports wildcard patterns such as `tms.biz.*` or `*`. |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Unpins all products in the `tms.biz` family:

```shell
tms unpin tms.biz.*
```

Unpins two specific products:

```shell
tms unpin tms.biz.aurelius,tms.fnc.maps
```

Unpins all currently installed products:

```shell
tms unpin *
```

## See Also

- [tms pin](xref:SmartSetup.Command.Pin)
- [tms update](xref:SmartSetup.Command.Update)

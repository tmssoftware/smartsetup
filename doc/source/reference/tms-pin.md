---
uid: SmartSetup.Command.Pin
---

# tms pin

Keeps the specified products at their current installed versions.

## Synopsis

```shell
tms pin <product-ids> [<global-options>]
```

## Description

Marks one or more installed products as pinned. A pinned product is excluded from version updates when running [tms update](xref:SmartSetup.Command.Update). The product remains installed at its current version until it is explicitly unpinned with [tms unpin](xref:SmartSetup.Command.Unpin).

Pinning does not affect [tms install](xref:SmartSetup.Command.Install) — installing a pinned product by name will still download the specified or latest version. Pin state is preserved when restoring from a snapshot if `-with-versions` is used with [tms restore](xref:SmartSetup.Command.Restore).

## Arguments

| Argument        | Description                                                                                                                                                                 |
| --------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `<product-ids>` | One or more product IDs to pin. Required. Multiple IDs can be separated by commas or provided as separate arguments. Supports wildcard patterns such as `tms.biz.*` or `*`. |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Pins all products in the `tms.biz` family:

```shell
tms pin tms.biz.*
```

Pins two specific products:

```shell
tms pin tms.biz.aurelius,tms.fnc.maps
```

Pins all currently installed products:

```shell
tms pin *
```

## See Also

- [tms unpin](xref:SmartSetup.Command.Unpin)
- [tms update](xref:SmartSetup.Command.Update)
- [tms restore](xref:SmartSetup.Command.Restore)

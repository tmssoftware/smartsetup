---
uid: SmartSetup.Command.Build
---

# tms build

Builds, registers, and unregisters all locally available products.

## Synopsis

```shell
tms build [<product-ids>] [<options>] [<global-options>]
```

## Description

Rebuilds locally available products and registers them in the appropriate IDEs. Only products with modified files are rebuilt — unchanged products are skipped. Products that have been removed or are no longer present are unregistered from the IDEs.

If one or more product IDs are specified, only those products are built. If no product IDs are provided, all installed products are processed.

tms build performs the install step of [tms update](xref:SmartSetup.Command.Update). Running [tms update](xref:SmartSetup.Command.Update) is equivalent to running [tms fetch](xref:SmartSetup.Command.Fetch) followed by tms build.

Products are built in parallel where possible, respecting inter-product dependencies.

## Arguments

| Argument        | Description                                                                                                                                                               |
| --------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `<product-ids>` | One or more product IDs to build. Multiple IDs can be separated by commas or provided as separate arguments. Supports wildcard patterns such as `tms.biz.*`. Optional — if omitted, all installed products are built. |

## Options

| Option        | Description                                                                                    |
| ------------- | ---------------------------------------------------------------------------------------------- |
| `-full`       | Forces all products to be rebuilt even if their files have not changed since the last build.   |
| `-unregister` | Unregisters all products from the IDEs without deleting them. Running tms build afterwards will re-register them. |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Builds all installed products, skipping any that are unchanged:

```shell
tms build
```

Forces a complete rebuild of all installed products:

```shell
tms build -full
```

Builds only two specific products:

```shell
tms build tms.biz.aurelius,tms.fnc.maps
```

Builds all products in a product family:

```shell
tms build tms.biz.*
```

Unregisters all products from the IDEs (without deleting them):

```shell
tms build -unregister
```

## See Also

- [tms fetch](xref:SmartSetup.Command.Fetch)
- [tms update](xref:SmartSetup.Command.Update)
- [tms install](xref:SmartSetup.Command.Install)

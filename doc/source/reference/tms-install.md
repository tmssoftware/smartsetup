---
uid: SmartSetup.Command.Install
---

# tms install

Downloads and installs the specified product(s), then builds and registers them in the IDEs.

## Synopsis

```shell
tms install <product-ids> [<options>] [<global-options>]
```

## Description

Downloads the specified products to the local products folder, compiles their packages, and registers them in all configured IDEs. This command combines a fetch and a build step in sequence: it first downloads the products and then performs a full build and registration.

If a product is already installed, [tms install](xref:SmartSetup.Command.Install) downloads the latest available version and rebuilds it.

Products are built in parallel where possible, respecting inter-product dependencies. After a successful install, the product's compiled packages are available in the IDEs and its BPL output folder is added to the Windows PATH if needed.

Use `-nobuild` to skip the build step and only download the product files.

## Arguments

| Argument        | Description                                                                                                                                                                                           |
| --------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `<product-ids>` | One or more product IDs to install. Required. Multiple IDs can be separated by commas or provided as separate arguments. Supports wildcard patterns such as `tms.biz.*`. |

## Options

| Option     | Description                                                             |
| ---------- | ----------------------------------------------------------------------- |
| `-nobuild` | Skips the build step. Only downloads the product files to disk without compiling or registering in any IDE. |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Installs all products in the `tms.biz` family:

```shell
tms install tms.biz.*
```

Installs two specific products:

```shell
tms install tms.biz.aurelius tms.fnc.maps
```

Installs products from two families:

```shell
tms install tms.biz.* tms.vcl.*
```

Downloads product files without building or registering in the IDEs:

```shell
tms install tms.biz.aurelius -nobuild
```

## See Also

- [tms uninstall](xref:SmartSetup.Command.Uninstall)
- [tms fetch](xref:SmartSetup.Command.Fetch)
- [tms build](xref:SmartSetup.Command.Build)
- [tms update](xref:SmartSetup.Command.Update)

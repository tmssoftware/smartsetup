---
uid: SmartSetup.Command.Uninstall
---

# tms uninstall

Removes the specified product(s) from disk and unregisters them from the IDEs.

## Synopsis

```shell
tms uninstall <product-ids> [<options>] [<global-options>]
```

## Description

Deletes the product files from the local products folder and then performs a build to unregister the removed products from all configured IDEs. After uninstall, the product packages are no longer available in the IDEs.

Only products that were originally installed via [tms install](xref:SmartSetup.Command.Install) can be removed. If no products remain installed after uninstall, the BPL output folder is also removed from the Windows PATH.

By default, [tms uninstall](xref:SmartSetup.Command.Uninstall) refuses to remove a product if other installed products depend on it. Use `-force` to override this check, or use `-cascade` to also remove the dependent products automatically.

## Arguments

| Argument        | Description                                                                                                                                                              |
| --------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `<product-ids>` | One or more product IDs to uninstall. Required. Multiple IDs can be separated by commas or provided as separate arguments. Supports wildcard patterns such as `tms.biz.*`. |

## Options

| Option     | Description                                                                                                                                     |
| ---------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| `-cascade` | Also uninstalls the dependencies of the selected products. Useful when removing a product along with everything it depends on.                  |
| `-force`   | Removes the product even if other installed products depend on it. Without this option, uninstall fails when dependents are found.              |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Uninstalls a single product:

```shell
tms uninstall tms.biz.aurelius
```

Uninstalls a product and all of its dependencies:

```shell
tms uninstall tms.biz.aurelius -cascade
```

Forces removal of a product even though other products depend on it:

```shell
tms uninstall tms.biz.aurelius -force
```

Uninstalls all products in two families:

```shell
tms uninstall tms.biz.* tms.vcl.*
```

## See Also

- [tms install](xref:SmartSetup.Command.Install)
- [tms build](xref:SmartSetup.Command.Build)

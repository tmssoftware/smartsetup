---
uid: SmartSetup.Command.List
---

# tms list

Lists all products currently installed in the Smart Setup environment.

## Synopsis

```shell
tms list [<options>] [<global-options>]
```

## Description

Reads the local Smart Setup environment and prints each installed product with its version and source server. Products marked with `*` were not fetched from a remote server and exist only in local state.

By default, one line is printed per product. Use `-detailed` to also see which IDEs and platforms have been built and registered for each product. Use `-json` to produce machine-readable output suitable for scripting.

## Options

| Option      | Description                                                              |
| ----------- | ------------------------------------------------------------------------ |
| `-detailed` | Displays the IDEs and platforms built and registered for each product.   |
| `-json`     | Outputs the product list in JSON format instead of plain text.           |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Lists all installed products with their versions and source servers:

```shell
tms list
```

Lists all installed products with IDE and platform details:

```shell
tms list -detailed
```

Outputs the installed product list as JSON, for use in scripts or automation:

```shell
tms list -json
```

## See Also

- [tms list-remote](xref:SmartSetup.Command.ListRemote)
- [tms install](xref:SmartSetup.Command.Install)

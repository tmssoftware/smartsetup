---
uid: SmartSetup.Command.Info
---

# tms info

Displays information about the current folder and the tms tool itself.

## Synopsis

```shell
tms info [<options>] [<global-options>]
```

## Description

Prints a summary of the current Smart Setup environment, including the tms version, the location of the tms executable, the working folder, whether the folder has been initialized, whether credentials are configured, the active configuration file path, and the IDEs installed in this machine.

The installed IDEs (`installed ides`) are the ones Smart Setup can build for, using the same check as [tms build](xref:SmartSetup.Command.Build): the IDE is registered in Windows (we ignore the alternate registry key and list those IDEs available in the main registry key), or you set a compiler path for it in `configuration for all products`, and the compiler is in that folder. Compiler paths set only for a specific product are not considered. The IDEs are listed by their ids, like `delphi12` or `lazarus`. If the configuration file can't be loaded, `installed ides` is left out.

Use `-json` to produce machine-readable output suitable for scripting.

## Options

| Option  | Description                                    |
| ------- | ---------------------------------------------- |
| `-json` | Outputs the information in JSON format.        |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Displays environment information for the current folder:

```shell
tms info
```

Outputs the environment information as JSON:

```shell
tms info -json
```

The JSON output includes the installed IDEs as an array, for example `"installed ides": ["delphi12", "delphi13"]`.

## See Also

- [tms version](xref:SmartSetup.Command.Version)
- [tms list](xref:SmartSetup.Command.List)

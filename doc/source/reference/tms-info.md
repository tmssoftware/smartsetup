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

Prints a summary of the current Smart Setup environment, including the tms version, the location of the tms executable, the working folder, whether the folder has been initialized, whether credentials are configured, and the active configuration file path.

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

## See Also

- [tms version](xref:SmartSetup.Command.Version)
- [tms list](xref:SmartSetup.Command.List)

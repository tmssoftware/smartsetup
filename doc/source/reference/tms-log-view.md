---
uid: SmartSetup.Command.LogView
---

# tms log-view

Opens the log file in the default viewer.

## Synopsis

```shell
tms log-view [<options>] [<global-options>]
```

## Description

Opens the Smart Setup log file using the system default application. By default, the HTML log file is opened. Use `-text` to open the plain text log instead.

Use `-print` to write the log file path to the console without opening it, which is useful in scripts that need to locate and process the log file directly.

## Options

| Option   | Description                                                          |
| -------- | -------------------------------------------------------------------- |
| `-print` | Writes the log file path to the console without opening the file.    |
| `-text`  | Opens the plain text log file instead of the HTML log file.          |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Opens the HTML log in the default viewer:

```shell
tms log-view
```

Opens the plain text log in the default viewer:

```shell
tms log-view -text
```

Prints the log file path without opening it:

```shell
tms log-view -print
```

## See Also

- [tms info](xref:SmartSetup.Command.Info)

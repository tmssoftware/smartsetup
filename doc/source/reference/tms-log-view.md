---
uid: SmartSetup.Command.LogView
---

# tms log-view

Opens the log file in the default viewer.

## Synopsis

```shell
tms log-view [<session-id>] [<options>] [<global-options>]
```

## Description

Opens the Smart Setup log file using the system default application. By default, the HTML log file is opened. Use `-text` to open the plain text log instead.

Every command that writes a log prints a line like `Session Id: 2026-10-09-14-30-05.123` and keeps a copy of its log under that id. Smart Setup keeps the logs of the last 10 sessions, in both text and HTML format. Pass a session id to open the log of an older session instead of the latest one.

Use `-print` to write the log file path to the console without opening it, which is useful in scripts that need to locate and process the log file directly.

## Arguments

| Argument      | Description                                                                                |
| ------------- | ------------------------------------------------------------------------------------------ |
| `<session-id>` | Optional. The session id of an older log. If not specified, the latest log is opened.      |

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

Opens the text log of an older session:

```shell
tms log-view 2026-10-09-14-30-05.123 -text
```

## See Also

- [tms info](xref:SmartSetup.Command.Info)

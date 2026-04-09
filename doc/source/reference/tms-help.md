---
uid: SmartSetup.Command.Help
---

# tms help

Displays usage information for a specific command.

## Synopsis

```shell
tms help <command> [<global-options>]
```

## Description

Prints the usage, description, and available options for the specified command. To see the list of all available commands, run `tms` without arguments.

Full documentation is available at [https://doc.tmssoftware.com/smartsetup](https://doc.tmssoftware.com/smartsetup).

## Arguments

| Argument    | Description                                    |
| ----------- | ---------------------------------------------- |
| `<command>` | The command to display help for. Required.     |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Displays usage information for the `install` command:

```shell
tms help install
```

Displays usage information for the `doctor` command:

```shell
tms help doctor
```

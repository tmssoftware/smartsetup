---
uid: SmartSetup.Command.ConfigRead
---

# tms config-read

Reads a setting from the Smart Setup configuration file.

## Synopsis

```shell
tms config-read [<variable-name>] [<options>] [<global-options>]
```

## Description

Reads and prints the value of a specific configuration setting from `tms.config.yaml`. The variable name uses a colon-separated path that matches the structure of the configuration file (e.g., `tms-smart-setup-options:git:git-location`).

If no variable name is provided, the full configuration is output. Use `-cmd` to see all settings formatted as `-p` parameters, which is useful for constructing [tms config-write](xref:SmartSetup.Command.ConfigWrite) commands.

## Arguments

| Argument          | Description                                                                 |
| ----------------- | --------------------------------------------------------------------------- |
| `<variable-name>` | Colon-separated path to the configuration setting to read. Optional.        |

## Options

| Option   | Description                                                                                                      |
| -------- | ---------------------------------------------------------------------------------------------------------------- |
| `-json`  | Outputs the value in JSON format.                                                                               |
| `-cmd`   | Outputs the setting as a `-p` parameter suitable for use with [tms config-write](xref:SmartSetup.Command.ConfigWrite) or other commands.            |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Reads the configured git executable path:

```shell
tms config-read tms-smart-setup-options:git:git-location
```

Lists all configuration settings as `-p` parameters:

```shell
tms config-read -cmd
```

## See Also

- [tms config-write](xref:SmartSetup.Command.ConfigWrite)
- [tms config](xref:SmartSetup.Command.Config)

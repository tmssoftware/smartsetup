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

Reads and prints the value of a specific configuration setting. The variable name uses a colon-separated path that matches the structure of the configuration file (e.g., `tms-smart-setup-options:git:git-location`).

By default, the command shows the merged configuration, which is the value actually in effect: `tms.config.global.yaml`, then `tms.config.yaml`, then `tms.config.local.yaml`, with each file overriding the ones before it. Use `-config-source` to read the values in a single file instead.

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
| `-config-source:<source>` | Reads only one configuration file: `main` (`tms.config.yaml`), `local` (`tms.config.local.yaml`) or `global` (`tms.config.global.yaml` in the `.smartsetup` folder of your user profile). If omitted, reads the merged configuration from all of them. |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Reads the configured git executable path:

```shell
tms config-read tms-smart-setup-options:git:git-location
```

Reads the git executable path set in `tms.config.local.yaml`, ignoring the other files:

```shell
tms config-read tms-smart-setup-options:git:git-location -config-source:local
```

Lists all configuration settings as `-p` parameters:

```shell
tms config-read -cmd
```

## See Also

- [tms config-write](xref:SmartSetup.Command.ConfigWrite)
- [tms config](xref:SmartSetup.Command.Config)

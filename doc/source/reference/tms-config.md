---
uid: SmartSetup.Command.Config
---

# tms config

Opens the configuration file in the default editor for YAML files.

## Synopsis

```shell
tms config [<options>] [<global-options>]
```

## Description

Opens `tms.config.yaml` in the system's default YAML editor so you can review and modify the configuration. If the configuration file does not exist, [tms config](xref:SmartSetup.Command.Config) creates a new standard configuration file first and then opens it.

## Options

| Option   | Description                                                           |
| -------- | --------------------------------------------------------------------- |
| `-print` | Writes the configuration file path to the console instead of opening it. |
| `-reset` | Generates a new configuration file, replacing any existing one.      |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Opens `tms.config.yaml` in the default editor, creating it first if it does not exist:

```shell
tms config
```

Replaces the existing configuration file with a fresh default configuration and opens it:

```shell
tms config -reset
```

Prints the path to the configuration file without opening it, useful in scripts:

```shell
tms config -print
```

## See Also

- [tms config-read](xref:SmartSetup.Command.ConfigRead)
- [tms config-write](xref:SmartSetup.Command.ConfigWrite)
- [Configuration](xref:SmartSetup.Configuration)

---
uid: SmartSetup.Command.ConfigWrite
---

# tms config-write

Saves the current configuration to `tms.config.yaml`.

## Synopsis

```shell
tms config-write [<global-options>]
```

## Description

Reads one configuration file and writes it back. By default, this is `tms.config.yaml`. Use `-config-source` to write `tms.config.local.yaml` or `tms.config.global.yaml` instead. This command is primarily useful in combination with the global `-p` option, which overrides individual configuration values before they are written. This allows scripted, non-interactive configuration changes without manually editing the YAML file.

The command loads only the file it writes, so values from the other configuration files are never copied into it. Because of this, a value you write can still be overridden when the configuration is merged: `tms.config.local.yaml` has priority over `tms.config.yaml`, which has priority over `tms.config.global.yaml`. Use [tms config-read](xref:SmartSetup.Command.ConfigRead) without `-config-source` to see the value in effect.

Files specified with the global `-add-config` option work like a list of `-p` parameters: their values are merged and written to the file.

{{#Warning}}
This command reformats the configuration file and removes any comments that were manually added.
{{/Warning}}

## Options

| Option                    | Description |
| ------------------------- | ----------- |
| `-config-source:<source>` | The configuration file to load and write: `main` (`tms.config.yaml`), `local` (`tms.config.local.yaml`) or `global` (`tms.config.global.yaml` in the `.smartsetup` folder of your user profile). Defaults to `main`. |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Sets the target Delphi versions and platforms, then saves:

```shell
tms config-write -p:configuration-for-all-products:delphi-versions=[delphi11,delphi12] -p:configuration-for-all-products:platforms=[win32intel,win64intel]
```

Sets the path to the git executable, then saves:

```shell
tms config-write -p:tms-smart-setup-options:git:git-location=c:\tools\git.exe
```

Sets the path to the git executable only for this machine, in `tms.config.local.yaml`:

```shell
tms config-write -p:tms-smart-setup-options:git:git-location=c:\tools\git.exe -config-source:local
```

## See Also

- [tms config-read](xref:SmartSetup.Command.ConfigRead)
- [tms config](xref:SmartSetup.Command.Config)

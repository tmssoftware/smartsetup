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

Reads the current Smart Setup configuration and writes it back to `tms.config.yaml`. This command is primarily useful in combination with the global `-p` option, which overrides individual configuration values before they are written. This allows scripted, non-interactive configuration changes without manually editing the YAML file.

{{#Warning}}
This command reformats the configuration file and removes any comments that were manually added.
{{/Warning}}

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

## See Also

- [tms config-read](xref:SmartSetup.Command.ConfigRead)
- [tms config](xref:SmartSetup.Command.Config)

---
uid: SmartSetup.Command.Doctor
---

# tms doctor

Checks and optionally fixes common problems in your Delphi installation.

## Synopsis

```shell
tms doctor [<options>] [<global-options>]
```

## Description

Runs a set of diagnostic checks against your Delphi installation and reports any issues found. The checks cover the Windows PATH, Delphi environment paths, library paths, and duplicated BPL registrations.

By default, the command reports problems without making any changes. Pass `-fix` to apply corrections. When `-fix` is used, the command prompts for confirmation before applying each fix. Pass `-fix -do-not-confirm` to apply all yes/no fixes automatically without prompting.

{{#Note}}
Fixes that require selecting among multiple options cannot be applied automatically with `-do-not-confirm`. Run `tms doctor -fix` without `-do-not-confirm` to handle those interactively.
{{/Note}}

## Options

| Option            | Description                                                                                                            |
| ----------------- | ---------------------------------------------------------------------------------------------------------------------- |
| `-fix`            | Applies fixes for the issues found. Prompts for confirmation before each change unless `-do-not-confirm` is also set. |
| `-do-not-confirm` | Skips confirmation prompts when used together with `-fix`. Applies yes/no fixes automatically.                        |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Runs all checks and reports issues without making changes:

```shell
tms doctor
```

Runs all checks and applies fixes, prompting for confirmation before each one:

```shell
tms doctor -fix
```

Runs all checks and applies yes/no fixes automatically without prompting:

```shell
tms doctor -fix -do-not-confirm
```

## See Also

- [tms info](xref:SmartSetup.Command.Info)

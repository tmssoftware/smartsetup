---
uid: SmartSetup.Command.SelfUpdate
---

# tms self-update

Updates tms Smart Setup to the latest available version.

## Synopsis

```shell
tms self-update [<options>] [<global-options>]
```

## Description

Fetches the latest version of the tms tool from the configured remote repository and installs it in place of the current executable. If a newer version has already been downloaded, the command extracts and applies it without fetching again.

When no newer version is available, the command reports that the current installation is up to date.

## Options

| Option     | Description                                                                          |
| ---------- | ------------------------------------------------------------------------------------ |
| `-nofetch` | Does not fetch newer versions from the server. Uses only what is already downloaded. |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Checks for a newer version online and installs it if available:

```shell
tms self-update
```

Installs a previously downloaded update without contacting the server:

```shell
tms self-update -nofetch
```

## See Also

- [tms version](xref:SmartSetup.Command.Version)

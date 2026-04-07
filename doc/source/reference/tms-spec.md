---
uid: SmartSetup.Command.Spec
---

# tms spec

Creates a new `tmsbuild.yaml` product definition file.

## Synopsis

```shell
tms spec [<options>] [<global-options>]
```

## Description

Generates a `tmsbuild.yaml` file that describes a product so it can be built and distributed with Smart Setup. By default, the command runs interactively: it scans the current directory for `.dproj` files, asks questions about the product ID, name, supported IDEs, and dependencies, and writes the resulting file.

Run this command from the root folder of the product whose packages you want to describe. The `.dproj` files in the folder tree are used to populate the package and platform configuration automatically.

Use `-non-interactive` to skip all prompts and generate a default file that can be edited manually or populated using `-spec` (`-s`) parameters.

## Options

| Option                    | Description                                                                                                                       |
| ------------------------- | --------------------------------------------------------------------------------------------------------------------------------- |
| `-non-interactive`        | Creates a default `tmsbuild.yaml` without prompting. Edit the file manually or use `-s` to supply values.                       |
| `-template:<file>`        | Loads initial values from an existing `tmsbuild.yaml` before prompting or applying `-s` overrides.                               |
| `-spec:<key=value>` / `-s:<key=value>` | Sets a specific property in the output `tmsbuild.yaml`. Can be specified multiple times. Uses the same key syntax as `-p` but targets the product spec instead of the configuration. |
| `-json`                   | Saves the output in JSON format. The resulting file cannot be read by Smart Setup, but is useful for querying values with a JSON parser. |
| `-cmd`                    | Saves the output as a command file listing the `-s` parameters needed to regenerate it. Useful for scripting.                    |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Runs the interactive wizard to create a `tmsbuild.yaml` in the current folder:

```shell
tms spec
```

Creates a default file non-interactively:

```shell
tms spec -non-interactive
```

Creates a file based on an existing template, overriding the product ID:

```shell
tms spec -template:existing.tmsbuild.yaml -s:application:id=myorg.myproduct
```

## See Also

- [tms build](xref:SmartSetup.Command.Build)

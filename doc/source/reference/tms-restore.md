---
uid: SmartSetup.Command.Restore
---

# tms restore

Restores a set of products from a snapshot file.

## Synopsis

```shell
tms restore [<filename>] [<options>] [<global-options>]
```

## Description

Reads a snapshot file created by [tms snapshot](xref:SmartSetup.Command.Snapshot) and installs the products listed in it. By default, each product is installed at its latest available version. Use `-with-versions` to restore the exact versions recorded in the snapshot, including their pinned state.

If no filename is provided, `tms restore` uses the filename configured as the single automatic snapshot entry in `tms.config.yaml`. If no such entry is configured or there is more than one, the filename argument is required.

Products without a server entry in the snapshot (local products) are skipped by default. Use `-include-local` to attempt to install them, though this will likely fail for products not available on any configured server.

## Arguments

| Argument     | Description                                                                                                                                                 |
| ------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `<filename>` | Path to the snapshot file to restore from. Optional if a single automatic snapshot filename is configured in `tms.config.yaml`; required otherwise. |

## Options

| Option                   | Description                                                                                                                                                  |
| ------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `-with-versions`         | Restores each product to the exact version recorded in the snapshot, and preserves the pinned state. Without this option, products are updated to their latest version. |
| `-nobuild`               | Skips the build step. Downloads product files only without compiling or registering in any IDE.                                                              |
| `-include:<pattern>`     | Restores only products whose ID matches the given pattern. Can be specified multiple times. If omitted, all products in the snapshot are restored.           |
| `-exclude:<pattern>`     | Skips products whose ID matches the given pattern. Can be specified multiple times.                                                                          |
| `-include-local`         | Attempts to restore products that have no server entry in the snapshot (local products). This will usually fail for products not on a configured server.    |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Restores all products from the configured automatic snapshot file, updating each to its latest version:

```shell
tms restore
```

Restores products from a specific file at the exact versions saved in the snapshot:

```shell
tms restore c:\backups\tms.snapshot.yaml -with-versions
```

Restores all products except those in the `tms.biz` family:

```shell
tms restore -exclude:tms.biz.* -exclude:example.test.*
```

## See Also

- [Versioning](xref:SmartSetup.Versioning#Snapshots)
- [tms snapshot](xref:SmartSetup.Command.Snapshot)
- [tms install](xref:SmartSetup.Command.Install)
- [tms pin](xref:SmartSetup.Command.Pin)

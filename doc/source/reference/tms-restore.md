---
uid: SmartSetup.Command.Restore
---

# tms restore

Restores a set of products from a snapshot file or a backup file.

## Synopsis

```shell
tms restore [<filename>] <-auto-register/-skip-register> [<options>] [<global-options>]
```

## Description

Reads a snapshot file created by [tms snapshot](xref:SmartSetup.Command.Snapshot), or a backup file created by [tms backup](xref:SmartSetup.Command.Backup), and installs the products listed in it. By default, each product is restored to the exact version recorded in the snapshot, including its pinned state. Use `-latest` to install the latest available version of each product instead.

You must specify either `-auto-register` or `-skip-register` to control whether restored products are registered in the IDE.

If no filename is provided, `tms restore` uses the filename configured as the single automatic snapshot entry in `tms.config.yaml`. If no such entry is configured or there is more than one, the filename argument is required.

## Restoring from a Backup File

A backup file is a `.zip` file that contains a snapshot and the product files of some or all of the products in it. When restoring from a backup file:

- Products whose files are in the backup file are installed from the backup file, without downloading anything.
- Products whose files are not in the backup file are downloaded from their server, as when restoring from a snapshot file.
- If the backup file has no snapshot, the products to restore are the product files stored in it. If there are several versions of the same product, the latest one is restored.

The `-latest` option only affects products downloaded from a server. Products in the backup file are always restored to the version stored in it.

Use [tms backup-info](xref:SmartSetup.Command.BackupInfo) to see which products a backup file contains, and which ones would be downloaded from a server.

## Local Products

Products without a server entry in the snapshot (local products) can't be downloaded, so they are skipped by default. Use `-include-local` to attempt to install them, though this will likely fail for products not available on any configured server.

This doesn't apply to local products whose files are in the backup file. Those are always restored from the backup file, without `-include-local`.

## Arguments

| Argument     | Description                                                                                                                                                 |
| ------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `<filename>` | Path to the snapshot file (`.yaml`) or backup file (`.zip`) to restore from. Optional if a single automatic snapshot filename is configured in `tms.config.yaml`; required otherwise. |

## Options

| Option                   | Description                                                                                                                                                  |
| ------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `-auto-register`         | Registers the restored products in the IDE according to the configuration. You must specify either this option or `-skip-register`.                          |
| `-skip-register`         | Does not register the restored products in the IDE; only builds them. You must specify either this option or `-auto-register`.                               |
| `-latest`                | Restores each product to its latest available version instead of the exact version recorded in the snapshot.                                                 |
| `-nobuild`               | Skips the build step. Downloads product files only without compiling.                                                                                        |
| `-include:<pattern>`     | Restores only products whose ID matches the given pattern. Can be specified multiple times. If omitted, all products in the snapshot are restored.           |
| `-exclude:<pattern>`     | Skips products whose ID matches the given pattern. Can be specified multiple times.                                                                          |
| `-include-local`         | Attempts to restore products that have no server entry in the snapshot (local products). This will usually fail for products not on a configured server. Not needed for local products whose files are in the backup file, as those are always restored. |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Restores all products from the configured automatic snapshot file at the exact versions in the snapshot, registering them in the IDE:

```shell
tms restore -auto-register
```

Restores products from a specific file, skipping IDE registration, and using the latest available versions instead of the snapshot versions:

```shell
tms restore c:\backups\tms.snapshot.yaml -skip-register -latest
```

Restores all products from a backup file created with `tms backup`, registering them in the IDE:

```shell
tms restore c:ackupsackup.zip -auto-register
```

Restores all products except those in the `tms.biz` family:

```shell
tms restore -skip-register -exclude:tms.biz.* -exclude:example.test.*
```

## See Also

- [Versioning](xref:SmartSetup.Versioning#snapshots)
- [tms snapshot](xref:SmartSetup.Command.Snapshot)
- [tms backup](xref:SmartSetup.Command.Backup)
- [tms backup-info](xref:SmartSetup.Command.BackupInfo)
- [tms install](xref:SmartSetup.Command.Install)
- [tms pin](xref:SmartSetup.Command.Pin)

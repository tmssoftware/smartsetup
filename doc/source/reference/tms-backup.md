---
uid: SmartSetup.Command.Backup
---

# tms backup

Saves products into a backup file that can be restored with [tms restore](xref:SmartSetup.Command.Restore).

## Synopsis

```shell
tms backup <filename> -include:<products> [-exclude:<products>] [-bundles-only] [<global-options>]
tms backup <filename> -as-installed [-include:<products>] [-exclude:<products>] [<global-options>]
```

## Description

Creates a `.zip` file with the product files of the selected products, plus a `tms.snapshot.yaml` file that lists them. Products are stored without compression, because the product files are already compressed.

Use [tms backup-info](xref:SmartSetup.Command.BackupInfo) to see what a backup file contains.

The command has two modes:

- **Official releases (default):** backs up the official release of each product you specify with `-include`, downloading the ones that aren't already available locally. Dependencies are added even if you don't specify them. Products don't need to be installed to be backed up. In this mode, `-include` is required.
- **As installed (`-as-installed`):** backs up the products as they are installed on your disk, including any changes you made to their files. Compiled package outputs are left out. Only products in the Smart Setup `Products` folder are backed up, since those are the only ones `tms restore` can restore. If you also use `-include`, only the installed products you specify are backed up, without their dependencies, and products that are not installed are ignored.

Products from Git or SVN are stored in the backup file as a zip of their folder, unless you use `-bundles-only`. With `-bundles-only`, those products are still listed in the snapshot, so a restore installs them from their server instead of from the backup file.

The backup is written to a temporary file first, and only replaces `<filename>` once it is complete. If there is an error, no backup file is created. After the backup, old downloaded versions are removed from the downloads folder, as with other commands that download products.

## Arguments

| Argument     | Description                                         |
| ------------ | --------------------------------------------------- |
| `<filename>` | Path to the backup file to create. Required.        |

## Options

| Option                 | Description                                                                                                                                                                                                    |
| ---------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `-include:<products>`  | Products to put in the backup. Can be specified multiple times, and accepts masks such as `tms.biz.*` and versions such as `tms.flexcel.vcl:7.27`. Required unless `-as-installed` is specified. See the Description for how it behaves in each mode. |
| `-exclude:<products>`  | Products to leave out of the backup. Can be specified multiple times. If not specified, no product is excluded.                                                                                               |
| `-as-installed`        | Backs up the products as installed on your disk. If not specified, the official releases are backed up, without any changes you might have made to the products.                                             |
| `-bundles-only`        | Doesn't put products from Git or SVN in the backup file. They are still added to the snapshot, so a restore gets them from their server. Cannot be used with `-as-installed`.                                   |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Backs up the official releases of all `tms.*` products except `tms.fnc.uipack`, with their dependencies:

```shell
tms backup c:\test\backup.zip -include:tms.* -exclude:tms.fnc.uipack
```

Backs up version 7.27 of `tms.flexcel.vcl` and the latest version of `tms.biz.xdata`:

```shell
tms backup c:\test\backup.zip -include:tms.flexcel.vcl:7.27 -include:tms.biz.xdata
```

Backs up all installed products as they are on your disk:

```shell
tms backup c:\test\backup.zip -as-installed
```

## See Also

- [tms restore](xref:SmartSetup.Command.Restore)
- [tms backup-info](xref:SmartSetup.Command.BackupInfo)
- [tms snapshot](xref:SmartSetup.Command.Snapshot)

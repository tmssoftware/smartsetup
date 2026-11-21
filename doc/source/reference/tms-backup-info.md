---
uid: SmartSetup.Command.BackupInfo
---

# tms backup-info

Shows the products and versions inside a backup file created with [tms backup](xref:SmartSetup.Command.Backup).

## Synopsis

```shell
tms backup-info <filename> [-json] [<global-options>]
```

## Description

Reads a backup file and lists the products it describes, sorted by product ID. It does not change anything in the Smart Setup folder. [tms restore](xref:SmartSetup.Command.Restore) reads files the same way, so the list shows what a restore of that file would do.

The file can be a backup `.zip` file or a snapshot `.yaml` file:

- **Backup `.zip` file:** if the zip contains a `tms.snapshot.yaml` file, the product list comes from that snapshot. If it doesn't, the list is built from the product files stored in the zip. When the zip has several versions of the same product, only the latest one is listed.
- **Snapshot `.yaml` file:** the product list comes from the snapshot. No product files are stored in it.

For each product, the text output says where a restore would get it from:

| Output                         | Meaning                                                                                   |
| ------------------------------ | ----------------------------------------------------------------------------------------- |
| `in the backup file`           | The product files for this version are stored in the backup file.                         |
| `restored from server "<name>"` | The product files aren't in the backup file. A restore downloads them from that server.  |
| `local, not restored`          | The product files aren't in the backup file, and the product has no server.               |

## Arguments

| Argument     | Description                                                     |
| ------------ | --------------------------------------------------------------- |
| `<filename>` | Path to the backup file (`.zip`) or snapshot file (`.yaml`). Required. |

## Options

| Option  | Description                                                          |
| ------- | -------------------------------------------------------------------- |
| `-json` | Outputs the backup contents in JSON format instead of plain text.    |

## JSON Output

With `-json`, the output is an object with these fields:

| Field          | Description                                                                      |
| -------------- | -------------------------------------------------------------------------------- |
| `filename`     | Full path of the file that was read.                                             |
| `has_snapshot` | `true` if the file contains a snapshot, or is a snapshot file itself.            |
| `products`     | An object with one entry per product, keyed by product ID.                       |

Each entry in `products` has these fields:

| Field     | Description                                                                                       |
| --------- | ------------------------------------------------------------------------------------------------- |
| `version` | The product version.                                                                              |
| `server`  | The server the product was installed from. Empty for local products. For a zip without a snapshot, it is `*backup*`. |
| `pinned`  | `true` if the product is pinned.                                                                  |
| `in_file` | `true` if the product files for this version are stored in the backup file. If `false`, a restore gets the product from its server, or skips it if it is local. |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Shows the products in a backup file:

```shell
tms backup-info c:\test\backup.snapshot.zip
```

Shows the products in a backup file in JSON format:

```shell
tms backup-info c:\test\backup.snapshot.zip -json
```

## See Also

- [tms backup](xref:SmartSetup.Command.Backup)
- [tms restore](xref:SmartSetup.Command.Restore)
- [tms snapshot](xref:SmartSetup.Command.Snapshot)
- [Versioning](xref:SmartSetup.Versioning#snapshots)

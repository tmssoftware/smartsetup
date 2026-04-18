---
uid: SmartSetup.Command.Snapshot
---

# tms snapshot

Saves the list of currently installed products and their versions to a file.

## Synopsis

```shell
tms snapshot <filename> [<global-options>]
```

## Description

Writes a YAML snapshot file containing the IDs, versions, and pin state of all currently installed products. The snapshot can later be used with [tms restore](xref:SmartSetup.Command.Restore) to reinstall the same set of products, optionally at the same versions.

## Arguments

| Argument     | Description                                      |
| ------------ | ------------------------------------------------ |
| `<filename>` | Path to the file where the snapshot will be saved. Required. |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Saves the current product state to a snapshot file:

```shell
tms snapshot c:\backups\tms.snapshot.yaml
```

## See Also

- [Versioning](xref:SmartSetup.Versioning#snapshots)
- [tms restore](xref:SmartSetup.Command.Restore)
- [tms list](xref:SmartSetup.Command.List)

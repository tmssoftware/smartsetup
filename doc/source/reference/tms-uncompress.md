---
uid: SmartSetup.Command.Uncompress
---

# tms uncompress

Extracts a Smart Setup bundle into a local folder.

## Synopsis

```shell
tms uncompress <bundle-filenames> <target-folder> [<options>] [<global-options>]
```

## Description

Extracts one or more Smart Setup bundle files into the specified folder. By default, the product ID is extracted from the bundle filename (the part before the first `_` character) and a subfolder with that name is created inside the target folder. The bundle contents are placed inside that subfolder.

After extraction, run [tms build](xref:SmartSetup.Command.Build) to compile the extracted product and register it in the IDEs.

Wildcards are supported in the bundle filename to uncompress multiple bundles in one operation.

## Arguments

| Argument            | Description                                                                                                                       |
| ------------------- | --------------------------------------------------------------------------------------------------------------------------------- |
| `<bundle-filenames>` | Path to the bundle file to extract. Wildcards are supported (e.g., `c:\bundles\tms.biz.*`). Required.                           |
| `<target-folder>`   | Path to the folder where the bundle will be extracted. A subfolder named after the product ID will be created here. Required.    |

## Options

| Option                  | Description                                                                                                       |
| ----------------------- | ----------------------------------------------------------------------------------------------------------------- |
| `-ignore-product-name`  | Extracts the bundle directly into the target folder without creating a product ID subfolder.                     |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Extracts all bundles matching `tms.biz.*` into separate product subfolders under `c:\tms\Products\`:

```shell
tms uncompress c:\bundles\tms.biz.* c:\tms\Products\
```

## See Also

- [tms build](xref:SmartSetup.Command.Build)
- [tms install](xref:SmartSetup.Command.Install)

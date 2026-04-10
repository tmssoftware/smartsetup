---
uid: SmartSetup.Command.SbomGenerate
---

# tms sbom-generate

Generates a CycloneDX SBOM file for the specified products.

## Synopsis

```shell
tms sbom-generate <product-ids> [-force] [<global-options>]
```

## Description

Creates a [CycloneDX](https://cyclonedx.org/) Software Bill of Materials (`.cdx.json`) file for each matching product. The file is written to the product's root folder and named after the product ID (for example, `tms.biz.aurelius.cdx.json`).

The generated SBOM includes product metadata (name, version, description, copyright, publisher, license, and external references) and a components section listing all declared dependencies.

By default, the command skips products that already have an SBOM file. Use `-force` to overwrite existing files.

The `<product-ids>` argument accepts one or more product IDs, separated by spaces or commas. Wildcards are supported (for example, `tms.biz.*`).

## Arguments

| Argument        | Description                                                                              |
| --------------- | ---------------------------------------------------------------------------------------- |
| `<product-ids>` | One or more product IDs (or wildcard patterns) to generate SBOMs for. Required.         |

## Options

| Option   | Description                                   |
| -------- | --------------------------------------------- |
| `-force` | Overwrites existing SBOM files.               |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Generates an SBOM for all installed products:

```shell
tms sbom-generate *
```

Generates SBOMs for specific products:

```shell
tms sbom-generate tms.biz.aurelius tms.flexcel.vcl
```

Regenerates SBOMs for all products, overwriting any existing files:

```shell
tms sbom-generate * -force
```

## See Also

- [tms list](xref:SmartSetup.Command.List)
- [tms info](xref:SmartSetup.Command.Info)

---
uid: SmartSetup.Command.SbomGenerate
---

# tms sbom-generate

Generates a CycloneDX SBOM file for the specified products.

## Synopsis

```shell
tms sbom-generate <product-ids> [-force] [-json] [<global-options>]
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
| `-json`  | Outputs the results in JSON format.           |

## JSON Output

With `-json`, the output is an object with one entry per matching product, keyed by product ID. Each entry has these fields:

| Field       | Description                                                                                         |
| ----------- | --------------------------------------------------------------------------------------------------- |
| `filename`  | Full path of the SBOM file.                                                                         |
| `generated` | `true` if the file was generated in this run. `false` if it was skipped because it already existed, or if generation failed. |
| `error`     | The error message, if generation failed. Omitted otherwise.                                         |

In JSON mode, a failure to generate the SBOM of a product is reported in its `error` field instead of as an error of the command.

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

Generates SBOMs for all `tms.biz.*` products and outputs the results as JSON:

```shell
tms sbom-generate tms.biz.* -json
```

## See Also

- [tms list](xref:SmartSetup.Command.List)
- [tms info](xref:SmartSetup.Command.Info)

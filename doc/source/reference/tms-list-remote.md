---
uid: SmartSetup.Command.ListRemote
---

# tms list-remote

Lists products available for installation in the configured remote repositories.

## Synopsis

```shell
tms list-remote [<options>] [<global-options>]
```

## Description

Queries each enabled server defined in the configuration and prints the products available for installation. For each product, the output shows the product ID, its latest version, and the server it comes from.

Only products the current account is licensed to install are included in the output. Products that are internal or unlicensed are excluded.

Use `-server` to limit results to a single server. Use `-json` to produce machine-readable output suitable for scripting.

## Options

| Option              | Description                                                         |
| ------------------- | ------------------------------------------------------------------- |
| `-json`             | Outputs the product list in JSON format instead of plain text.      |
| `-server:<server>`  | Limits results to the specified server name.                        |

## JSON Output

With `-json`, the output is an object with one entry per product, keyed by product ID. Each entry has these fields:

| Field                 | Description                                                                                                      |
| --------------------- | ---------------------------------------------------------------------------------------------------------------- |
| `version`             | The latest version. Empty for products from Git or SVN servers, like `community`.                                |
| `version_type`        | How the version is compared, for example semantic versioning for API servers, or free form for Git or SVN servers. |
| `name`                | The product name.                                                                                                |
| `license_status`      | Only for API servers, when the server returns a license status for the product.                                  |
| `vendor_id`           | Only for API servers, when the server returns a vendor for the product.                                          |
| `server`              | The server the product comes from.                                                                               |
| `description`         | The product description. For API servers, it comes from the metadata of the latest version.                     |
| `url`                 | The product URL. For Git or SVN servers, this is the URL of the repository. For API servers, it comes from the metadata of the latest version. |
| `docs`                | The URL of the product documentation. For Git or SVN servers, the `docs` in the `application` section of the product's tmsbuild.yaml. For API servers, it comes from the metadata of the latest version. |
| `homepage`            | The product homepage. For Git or SVN servers, the `homepage` in the `application` section of the product's tmsbuild.yaml. For API servers, it comes from the metadata of the latest version. |
| `version_history_url` | The URL of the version history. For Git or SVN servers, the `version history url` in the `application` section of the product's tmsbuild.yaml. For API servers, it comes from the metadata of the latest version. |
| `available_defines`   | An array with the defines the product accepts, each one with a `name` and a `description`. For API servers, it comes from the metadata of the latest version. |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Lists all products available from all enabled remote servers:

```shell
tms list-remote
```

Lists products available from a specific server:

```shell
tms list-remote -server:tms
```

Outputs the available product list as JSON, for use in scripts or automation:

```shell
tms list-remote -json
```

## See Also

- [tms list](xref:SmartSetup.Command.List)
- [tms install](xref:SmartSetup.Command.Install)

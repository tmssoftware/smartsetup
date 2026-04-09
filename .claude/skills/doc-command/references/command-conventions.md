# Command Reference Conventions

## Page Location and Naming

- File: `doc/source/reference/tms-<command-name>.md`
- Hyphenate multi-word commands: `tms-list-remote.md`, `tms-self-update.md`
- UID: `SmartSetup.Command.<PascalCaseCommandName>` — e.g., `SmartSetup.Command.ListRemote`, `SmartSetup.Command.Build`

## Page Template

````markdown
---
uid: SmartSetup.Command.<CommandName>
---

# tms <command-name>

<One-sentence description of what the command does. Matches the short description from RegisterCommand, but may be expanded for clarity.>

## Synopsis

```shell
tms <command-name> [<arguments>] [<options>] [<global-options>]
```
````

## Description

<Full description of what the command does. Base this on the long description from RegisterCommand and the implementation source. Explain the behavior, not just the interface. Use short paragraphs.>

## Arguments

| Argument          | Description                                      |
| ----------------- | ------------------------------------------------ |
| `<argument-name>` | Description. Mark required arguments explicitly. |

_Omit this section if the command takes no positional arguments._

## Options

| Option                 | Description                                                       |
| ---------------------- | ----------------------------------------------------------------- |
| `-option-name`         | Description.                                                      |
| `-option-name <value>` | Description. Include the value placeholder when HasValue is true. |

_Omit this section if the command has no command-specific options._

## Global Options

Refer to global options with a standard cross-reference — do not redocument them here:

```markdown
See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.
```

## Examples

<Brief explanation of what the example does, ending with a colon:>

```shell
tms <command-name> <example-args>
```

_Add one example per distinct use case. Use real product IDs like `tms.biz.aurelius` or `tms.biz.*` in examples. Always put the description before the code block, never after._

## See Also

- [Command name](xref:SmartSetup.Command.OtherCommand)
- [Configuration](xref:SmartSetup.Configuration)

## Option Formatting Rules

- Options use a **single dash**: `-full`, `-verbose`, `-cascade`. Never double-dash.
- Options with values use `:<value>` syntax in command-line.
- In the Options table, use `\`-option\``for flag options and`\`-option:\<value\>\`` for options that take a value.
- Hidden options (`.Hidden := True` in source) must not be documented.
- Required arguments must be stated as required in the Arguments table description.
- Optional arguments should show `[<argument>]` in the Synopsis.

## Writing Rules

- **Command name in title**: always `tms <command-name>` (lowercase, with `tms` prefix).
- **No invented behavior**: every statement must be traceable to source code or `tms help` output.
- **Short description first**: the opening paragraph should be a one-liner. Expand in Description.
- **Examples must work**: use realistic product IDs and flags. Do not use placeholder values like `<product-id>`.
- **No double-dash options**: Smart Setup uses single-dash. Writing `--full` is wrong; write `-full`.
- **No "you" imperative unless necessary**: prefer "Installs the product" over "Installs your product".
- **Cross-reference, don't duplicate**: if behavior is explained in a guide topic, link to it.
- **Global options**: never document `-verbose`, `-config`, `-p`, etc. in per-command pages. Link to the global options page instead.

## UID Naming for Commands

| Command            | UID                              |
| ------------------ | -------------------------------- |
| `tms build`        | `SmartSetup.Command.Build`       |
| `tms install`      | `SmartSetup.Command.Install`     |
| `tms list-remote`  | `SmartSetup.Command.ListRemote`  |
| `tms self-update`  | `SmartSetup.Command.SelfUpdate`  |
| `tms config-read`  | `SmartSetup.Command.ConfigRead`  |
| `tms config-write` | `SmartSetup.Command.ConfigWrite` |
| `tms server-add`   | `SmartSetup.Command.ServerAdd`   |

Follow the PascalCase pattern: strip hyphens and capitalize each word.

## Command Index Page

The index page at `doc/source/reference/index.md` lists all commands grouped by section.
Sections and their order match the `tms` command output.
Each entry in the index links to its page using `[tms command-name](xref:SmartSetup.Command.CommandName)` and includes the short description.

## TOC Entry

When adding a new page, append an entry to `doc/source/reference/toc.yml` and `doc/source/reference/_toc.md` in the correct group order:

```yaml
- name: tms <command-name>
  href: tms-<command-name>.md
```

---
name: doc-command
description: Create and update command reference pages in Smart Setup documentation. Use when the user asks to document a command, create a missing command page, or update/fix/improve documentation of an existing command page. Triggers on requests like "document the build command", "create a page for tms install", "update docs for list-remote", "fix the documentation for tms fetch", "the doctor page needs to be updated".
---

# Smart Setup Command Reference Writer

Write and update command reference pages following the conventions in this skill.

## Before Writing

1. Use doc-guidelines skill to learn about documentation conventions.
2. Read [references/command-conventions.md](references/command-conventions.md) for command page structure, option formatting, and Smart Setup-specific rules.

## Gathering Information

Never invent command behavior, options, or descriptions. Gather information from these sources — use all three when available:

### 1. Source code (primary for option details)

Each command is declared in `tms/src/Commands/Commands.<CommandName>.pas`.  
Read that file to find:

- The exact command name (first arg to `RegisterCommand`)
- Short description (third arg) and long description (fourth arg)
- Usage line (fifth arg)
- Every option registered via `RegisterOption` or `RegisterUnNamedOption`, including name, short alias, description, and flags (`HasValue`, `Required`, `AllowMultiple`, `Hidden`)
- The `CommandGroups.*` constant passed to `AddCommand` — this determines which group the command belongs to

The command group constants are defined in `tms/src/Commands/Commands.CommonOptions.pas`:

```
CommandGroups.Install  → "Installing and updating:"
CommandGroups.Config   → "Configuration:"
CommandGroups.Status   → "Informative:"
CommandGroups.Diagnosis → "Check and Repair:"
CommandGroups.Self     → "Concerning tms tool itself:"
```

Global options (available to all commands) are also registered in `Commands.CommonOptions.pas`:

- `-config <file>` — specifies an alternate config file
- `-add-config <file>` — adds options from an additional config file
- `-verbose` — shows more detailed output
- `-display-options <options>` — controls console display
- `-p:<path>=<value>` — overrides a config parameter

### 2. Command-line output (verify option names and descriptions)

Run `tms help <command>` in the terminal to see the actual formatted help output. This confirms the exact option names and descriptions as users will see them.

### 3. Implementation source (for behavior details and examples)

Read the action/implementation files under `tms/src/` to understand what the command does in depth. This is especially useful for:

- Writing accurate descriptions of complex behavior
- Finding edge cases worth noting
- Constructing realistic examples

## Workflow

### Creating a New Command Page

1. Gather information using the three sources above.
2. Read 1–2 existing command pages in `doc/source/reference/` to match style and depth.
3. Create `doc/source/reference/tms-<command-name>.md` using the page template in [references/command-conventions.md](references/command-conventions.md).
4. Add the page to `doc/source/reference/toc.yml` at the appropriate position (grouped by command group, same order as `tms` output).
5. If the reference section is new, also update `doc/source/toc.yml` to include the reference section.

### Updating an Existing Command Page

1. Read the existing page.
2. Gather fresh information from source code and `tms help <command>` to find what changed.
3. Update the relevant sections. Follow the existing structure unless the conventions call for restructuring.

### Documenting the Command Index Page

The index page (`doc/source/reference/index.md`) lists all commands, organized in the same groups and order as the `tms` command output. The group order is defined by the order `PrintGroupedCommands` iterates `_Groups` in `common/src/UCommandLine.pas`, which matches the order commands are registered at startup. Use the `CommandGroups` constants as the section headings.

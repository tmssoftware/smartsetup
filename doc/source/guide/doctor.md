---
uid: SmartSetup.Doctor
---

# `doctor` command

## Overview

The `doctor` command is a diagnostic tool included in TMS Smart Setup. It analyzes your system and Delphi IDE configuration to detect and optionally fix common issues that could affect TMS Smart Setup's operation, particularly during the installation or setup of Delphi packages.

This command is available via the command line:

```shell
tms doctor
```

By default, it performs only diagnostics and does not apply changes. You can choose to fix the identified problems with additional options.

## Purpose

- Diagnose misconfigurations or invalid settings in your Delphi installation and operating system that could prevent Smart Setup from working properly.
- Detect invalid or outdated paths, missing environment variables, or system inconsistencies.
- Optionally, fix the detected issues either interactively or automatically.

## Usage

```shell
tms doctor [options]
```

### Options:

- `-fix` — Tries to fix the identified problems. For each fix, you will be asked for confirmation.
- `-do-not-confirm` — Applies all fixes automatically without asking for confirmation (only valid if `-fix` is also specified).

### Examples:

- Diagnostic only:
  ```shell
  tms doctor
  ```

- Fix problems interactively:
  ```shell
  tms doctor -fix
  ```

- Fix all problems without confirmation:
  ```shell
  tms doctor -fix -do-not-confirm
  ```

## Checks Performed

The `doctor` command runs multiple independent checks. Below is a list of checks and what they diagnose and fix.

### ✅ Windows Path Checks

#### 1. **Windows User Path**
- **Description:** Scans the PATH environment variable for the current user.
- **Issues Detected:**
  - Paths that no longer exist.
  - Empty or redundant path entries.
- **Fixes Suggested:**
  - Removal of invalid paths to reduce PATH size.

#### 2. **Windows Local Machine Path**
- **Description:** Same as above, but applies to the machine-wide PATH variable.
- **Permissions:** Requires administrative rights to apply fixes.

### ✅ Delphi Environment Checks

#### 3. **PATH Override in Delphi IDE**
- **Description:** Checks if Delphi’s environment variable override for PATH includes the `$(PATH)` placeholder.
- **Why It Matters:** Missing `$(PATH)` causes Delphi to ignore your system PATH, which may prevent loading of TMS packages.
- **Fix:** Adds `$(PATH)` to the override.

### ✅ Library Path Checks (For All Installed Delphi IDE Versions)

These checks apply to every installed Delphi IDE and platform.

#### 4. **Multiple Slashes in Library Path**
- **Description:** Scans library paths for entries with `\` (double backslashes).
- **Why It Matters:** May break builds due to incorrect path interpretation.
- **Fix:** Simplifies the paths by removing redundant slashes.

#### 5. **Non-Existent Paths in Library Path**
- **Description:** Looks for folder paths that don’t exist on disk.
- **Why It Matters:** Non-existent paths unnecessarily bloat the library path and can cause build slowdowns or failures.
- **Fix:** Suggests removing such paths.

These checks apply to every installed Delphi IDE and platform.

### ✅ Duplicated Files Checks
#### 6. **Duplicated BPLs**
- **Description:** Looks in the Windows Path for duplicated bpls.
- **Why It Matters:** If you have two different bpls in the Windows Path, Rad Studio is very likely to find the wrong bpl and show an error message when loading.
- **Fix:** Opens the folders where the duplicated bpls are, in file explorer, so you can delete the wrong ones. As you need to be very sure to remove the right bpls, this test won't automatically delete any file, you need to remove them yourself.


## What It Changes

When fixes are applied (using `-fix`), the following system areas **may be modified**:

- Windows environment variables (PATH) for the current user or machine.
- Delphi IDE configurations: IDE environment overrides and library paths.

All changes are logged and undo information is saved in a specific folder, so manual rollback is possible if needed.


## Troubleshooting Tips

- If you're unsure about applying changes, run `tms doctor` without `-fix` to preview potential problems.
- For issues requiring admin access (like modifying local machine PATH), run your terminal as administrator.
- After applying fixes, consider restarting the IDE and/or system to ensure all changes take effect.
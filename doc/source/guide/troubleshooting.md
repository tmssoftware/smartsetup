---
uid: SmartSetup.Troubleshooting
---

# Troubleshooting Guide for Installing Components Using Smart Setup

This article helps you diagnose and fix common issues when installing or building components using **Smart Setup**.  
It incorporates scenarios from both TMS and non-TMS users and provides clear steps applicable to any Smart Setup environment.

---

## Important Compatibility Notice

Smart Setup installs and manages packages in its own folder structure.  
If packages were installed previously **outside** Smart Setup, those installations may interfere with Smart Setup operations.

### Important clarification for TMS users  

TMS products installed using the **classic TMS installers** (setup.exe / Subscription Manager) are **not compatible** with the same TMS products installed using Smart Setup.

These classic installers:

- Place files in different locations  
- Use traditional Delphi package naming (for example: `TMSVCLUIPackPkgDXE15.dproj`)  
- Register design-time packages differently  

If you have TMS products installed both ways, Delphi will encounter package conflicts.

### For non-TMS users  

The legacy-installer conflict applies **only** to TMS products.  
If you are using Smart Setup for other libraries, you generally do not need to worry about TMS-specific legacy installers unless you also use TMS components.

---

## Common Symptoms

Applies to all Smart Setup users, with TMS-specific examples indicated.

Common issues include:

- “**Can't load package `<name>.bpl` – The specified module could not be found**”
- “**Unit XYZ is contained in two different packages**”
- “**This package was compiled with a different version…**”
- Successful builds but IDE cannot load packages
- Library Path or environment variables appear incorrect
- Conflicts between old and new versions of the same package

**TMS-specific example:**  
Seeing package files like:  

```
TMSVCLUIPackPkgDXE15.dproj
```

indicates a classic TMS installation is still present.

---

## 1. How to Run Smart Setup Commands

Many users rely only on **tmsgui.exe**, but troubleshooting often requires command-line commands such as:

- `tms doctor`
- `tms doctor -fix`
- `tms doctor -fix -do-not-confirm`
- `tms build -full`
- `tms install`
- `tms update`

These commands must be run from the Smart Setup installation folder.

---

### 1.1 Opening a Command Prompt

1. Press **Win + R**
2. Type: `cmd`
3. Press **Enter**

---

### 1.2 Smart Setup Installation Folder

Default for installer-based installation:

```
C:\Users\<user>\AppData\Local\TMS Setup
```

Replace `<user>` with your Windows user name.

For portable installations, use the folder where you extracted the ZIP.

---

### 1.3 Open a Command Prompt Directly in the Smart Setup Folder

1. Open **File Explorer**  
2. Navigate to your Smart Setup folder  
3. Click the **address bar**  
4. Type: `cmd`  
5. Press **Enter**

---

## 2. Standard Fix Sequence for Most Installation Problems

These steps resolve most Smart Setup installation issues for both TMS and non-TMS users.

---

### Step 1 – Analyze Your Environment with TMS Doctor

Run:

```
tms doctor
```

This scans for:

- Problems in PATH  
- Inconsistent environment variables  
- Broken or conflicting Delphi paths  
- Locations of outdated package files  
- Conditions that commonly prevent package loading

`tms doctor` does **not** modify anything.

---

### Step 2 – Apply Fixes with Confirmation

Run:

```
tms doctor -fix
```

This applies corrections for issues found by `tms doctor`.

You will be prompted to confirm **each** change before it is applied.

---

### Optional: Apply All Fixes Automatically

If many fixes are needed, use:

```
tms doctor -fix -do-not-confirm
```

This applies all corrections without prompting.  
It is slightly less safe because you cannot review each change individually but is useful for large batches of fixes.

---

### Step 3 – Restart Windows

Restart to ensure:

- PATH changes take effect  
- IDEs reload configuration correctly  
- Locked files are released  

---

### Step 4 – Rebuild All Packages

Run:

```
tms build -full
```

This:

- Cleans existing builds  
- Recompiles all installed products  
- Ensures your environment uses clean, consistent binaries  

This step solves many “can’t load package” issues.

---

### Step 5 – Test Delphi or Your Build System

Open Delphi normally and test.

If package loading succeeds, the issue is resolved.

---

### Step 6 – Remove Old or Duplicate BPL Files

Legacy files from old installations (especially classic installers for TMS products) or manual file copies may cause conflicts.

Use Everything Search:  

[https://www.voidtools.com/](https://www.voidtools.com/)

Search for:

```
*.bpl
*.dcp
tms*.bpl       (only if you use TMS products)
dctms*.bpl     (only if you use TMS products)
```

Check folders such as:

- `C:\Windows\System32`
- `C:\Windows\SysWOW64`
- Old Delphi installation directories
- Folders used by previous/unrelated installers  

If you find outdated or duplicate files:

- Delete them, or  
- Rename them (e.g., append `.old`)  

Then retest Delphi.

---

### Step 7 – If Problems Persist: Provide logs.zip

Smart Setup creates diagnostic logs in:

```
<Smart Setup Folder>\Logs\logs.zip
```

`logs.zip` includes:

- Build logs  
- Environment variable reports  
- Path analysis  
- Platform information  

Providing this file enables fast diagnosis.

---

## 3. Handling Classic TMS Installations (TMS Users Only)

This section applies **only to users of TMS products**.

Smart Setup installations of TMS components are **not compatible** with classic TMS installers.

### Symptoms of classic TMS remnants

- Package files with Delphi-version suffixes, e.g.:
  
```
  TMSVCLUIPackPkgDXE15.dproj
```

- Old TMS BPL files being loaded instead of Smart Setup versions  
- “Unit XYZ exists in both packages” errors  
- Smart Setup builds succeed, but Delphi loads the wrong package

### How to fix TMS-specific conflicts

1. Open **Windows Apps & Features** (Add/Remove Programs)  
2. Uninstall **any** TMS products found there  
   - Smart Setup packages do **not** appear in this list  
   - Anything appearing here is from the classic TMS installer  
3. After uninstalling, run:

```
tms doctor
tms doctor -fix
tms build -full
```

This gives you a clean Smart Setup-only environment.

---

## 4. Other Frequent Issues and Solutions

---

### 4.1 Missing Delphi Platform Support

Smart Setup may warn that certain platforms or compilers are unavailable.  
This happens when Delphi does not have required platforms installed (Win64, Linux, macOS, Android, iOS, etc.).

Fix:

- Open the **RAD Studio Installer**
- Add the missing platforms  

If you do not target that platform, you may ignore the warnings.

---

### 4.2 PATH Issues or Long PATH Problems

Delphi versions 12.x and 13 are sensitive to:

- Overly long PATH values  
- Custom environment variable overrides  
- Missing `$(PATH)` inside IDE configuration  

Fix inside the IDE:

1. Open **Delphi**  
2. Go to **Tools → Options → IDE → Environment Variables**  
3. Ensure PATH is defined as:

```
PATH = $(PATH)
```

`tms doctor` and `tms doctor -fix` commonly detect these issues.

---

### 4.3 Leftover Manual Copies of BPL/DCP Files

If you once manually copied BPL/DCP files into Delphi or Windows folders, they can interfere with Smart Setup.

Fix:

1. Locate and remove the manual files  
2. Rebuild everything running `tms build -full` command
3. Check that Delphi’s Library Path does not refer to old or unintended folders

---

## 5. After Upgrading Delphi (11 → 12 → 12.3 → 13)

Upgrading Delphi may leave behind:

- Outdated package folders  
- Old compiled binaries  
- Incorrect Library Path entries  
- Environment variables referencing old versions  

Recommended steps:

```
tms doctor
tms doctor -fix
tms build -full
```

If issues persist:

- Search for duplicate BPL files  
- Remove old/unused package folders  
- Reinstall missing Delphi platforms  
- Clean up Library Path entries  

---

## 6. When to Contact Support

If you need assistance, include:

- `logs.zip` from the Smart Setup Logs folder  
- Full screenshot of any error messages  
- Your Delphi version  
- Screenshot of **Tools → Options → Library → Library Path**  

This information enables accurate diagnosis.

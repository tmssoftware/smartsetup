---
uid: SmartSetup.Overview
---

# {{title}} introduction

TMS Smart Setup is a new generation to install libraries and components for Delphi, C++ Builder, Lazarus/FPC and .NET, brought by [TMS Software](https://www.tmssoftware.com).

**[Download TMS Smart Setup](download/index.md)**

## Overview

It provides command-line and GUI tools that work together to provide a modern install experience for TMS products. Some of key features are:

* Easy. A single command can download or install the full suite of TMS products at once.
* Fully scriptable. While there are planned GUI tools to allow the user easily choose options and products to install, all the work is done by command-line tools. You can call those tools from a bat file in a CI server and automate the installs. We can call those tools from others like TMS subscription manager or TMS Dashboard.
* Multi-platform. The tools will work in Windows, Mac and Linux, allowing people using Lazarus (under work) to install in other platforms than Windows without running an exe.
* No admin permissions needed. So it can install everything as a normal user.

### Smart download

TMS Smart Setup provides commands to download all the registered TMS products from a central repository. Some of key features are:

* Parallel download for faster retrieval
* Automatically detects missing and outdated products and downloads the latest versions of what's needed
* Dependency checker: automatically downloads dependencies of a specific project
* Self-update mechanism for the command-line tool

### Smart build

TMS Smart Setup builds the source code of products and install them in the supported IDEs. Each product has a list of packages to be built and installed. Some of key features are:

* Automatic build of packages to generate output binaries (`.bpl`, `.dcu`, etc.)
* Parallel package build for faster installation
* Smart and fast rebuild by only rebuilding packages that have been modified
* Supports since Delphi 7 up to latest version
* Registers design-time packages and in Delphi IDE and properly modify paths (library path, debug DCU path, browsing path, etc.)
* Smart uninstall, removing all IDE registration and generated binary files
* Dependency checker: you can define projects depending on others and build/registration process will work accordingly

## Usage

You can use TMS Smart Setup from command-line, or use a GUI tool. Command-line is the main way to use and recommended for fully control of your install process, for CI/CD environments. The GUI tool can be used as an alternative if you are not familiar or comfortable with using command-line tools.

## Command-line usage

TMS Smart Setup is folder-based. Once you have the folder initialized, you can run the command-line and Smart Setup will create files and subfolder inside it..

The command-line is self-explanatory, just run `tms` to list all available commands, or `tms help <command>` for more detailed information about a specific command.

### Setting up credentials

Once in the folder, run `tms credentials` to initialize the folder and input your credentials:

```shell
tms credentials
```

### Installing products

Then use `install` command to download new products from the remote repository to your local machine. 

```shell
tms install tms.biz.aurelius
```

It's worth noting that the above command will download and install TMS Aurelius and also all its dependencies (in this case, TMS BIZ Core Library ).

To find out what are the ids of the products you have available to install, you can run `list-remote` command:

```shell
tms list-remote
```

You can also specify multiple products to install, separated either by comma or spaces. Masks can be used to install all products that match the mask. For example, the following command installs all BIZ products and TMS Flexcel for VCL:

```yaml
tms install tms.biz.* tms.flexcel.vcl
```

### Updating products

From time to time, you can run `update` command to download newest versions of the products, if available:

```shell
tms update
```

The above command will check in the remote repository for new versions of all products you have installed. If there are new versions, it will download, update and rebuild them, all automatically.

### Rebuilding

When installing or updating products, TMS Smart Setup only rebuilds what has been modified. This makes the installation and update processes very fast. 

But there are times that the build process might fail, for any reason (Delphi misconfiguration, antivirus in action, bugs, etc.). In this case, you will have your products downloaded but not properly built and registered in the IDE. 

To force a new rebuild to fix these issues, just call the `build` command:

```shell
tms build
```

This command will just try to rebuild what has been modified or failed, and quickly fix that. In case things are really not working, you can ask for a full rebuild, which will rebuild and re-register all your products:

```
tms build -full
```

You can also use the `build` command if you updated the source code of TMS products yourself. After modifying the source code, call `build` command to update the installation properly.

### Uninstalling a product

Uninstalling a product is as simple and similar as installing. Just call `uninstall` passing the products to be uninstalled. It also accepts masks and comma-separated ids:

```shell
tms uninstall tms.biz.aurelius
tms uninstall tms.biz.*,tms.flexcel.vcl
```

Note that `uninstall` command **does not** uninstall dependencies. Thus, if you ask to uninstall `tms.biz.aurelius` only, then `tms.biz.bcl`, which is an TMS Aurelius dependency, will remain installed. To uninstall a product and all its dependencies, use the `-cascade` option:

```
tms uninstall tms.biz.aurelius -cascade
```

It might be possible that you try to uninstall a product that another installed product depends on. For example, if you try to uninstall TMS BCL but not TMS Aurelius, which depends on BCL In this case, `uninstall` will fail. You can bypass this check by adding the `-force` parameter. But we don't recommend it, because you will end up with bad-installed products. Only use it if you really know what you are doing.

### Custom configuration

TMS Smart Setup works smoothly out of the box. It automatically detects all your installed Delphi IDEs and platforms, in summary, your current system setup, and installs everything the best way possible in your environment.

But if you want more control over its behavior, you can fully customize its behavior by using a YAML config file.

You can ask the tool to create a preconfigured YAML configuration file by running the `config` command:

```shell
tms config
```

This will create a YAML file named `tms.config.yaml` in your root folder, and launch your default YAML editor to edit its content.

The default YAML config file contains all the available configuration options you can modify, and each of them is fully documented. Just read the file to learn about it and modify the options as you wish. 

### Check what is installed

You can use command `tms list` to check all products you have installed locally:

```shell
C:\tms>tms list
tms.biz.aurelius (5.16.0.1)
tms.biz.bcl (1.38.0.1)
tms.biz.sparkle (3.25.0.1)
tms.biz.xdata (5.13.0.1)
```

Using the `-detailed` parameter displays the exact IDE and platforms for which each product is installed:

```shell
C:\tms>tms list -detailed
tms.biz.aurelius (5.16.0.1)
- delphi11
  - win32intel
  - win64intel
  - linux64

tms.biz.bcl (1.38.0.1)
- delphi11
  - win32intel
  - win64intel
  - linux64

tms.biz.sparkle (3.25.0.1)
- delphi11
  - win32intel
  - win64intel
  - linux64

tms.biz.xdata (5.13.0.1)
- delphi11
  - win32intel
  - win64intel
  - linux64
```

### Self-updating

`tms.exe` can update itself. It will regularly check for new versions, and if available, it will warn you about it.

At any time, specially if you get such warning, you can update `tms.exe` to a newer version by executing `self-update`:

```shell
tms self-update
```

We recommend to not ignore the warnings about a new version, and always keep the tool updated to the latest version.

## GUI tool usage

Alternatively to command-line, you can use the GUI tool. Just launch `tmsgui.exe` executable. 

If the working folder is not initialized, it will first ask for credentials to download products from repository. If you cancel, no credentials will be saved and you can continue using the GUI to build project that have been manually downloaded.

The GUI tool lists the installed and available products in the main listing view. At the right, you have some buttons that perform on the selected products:

- **Install**: download and install the selected products. Equivalent to `tms install` command.
- **Uninstall**: uninstall the selected products. Equivalent to `tms uninstall` command.
- **Full build/Partial build**: rebuild the selected products. If no products are selected, rebuild everything. Equivalent to `tms build -full` and `tms build` commands
- **Configure**: create (if missing) and open the YAML configuration file. Equivalent to `tms config` command.
- **Credentials**: ask for new credentials. Equivalent to `tms credentials` command.

In the status bar, at the bottom of the main window, the GUI displays the current working folder, and the current Smart Setup version.

Under the hood, the GUI just executes the `tms` command-line. The `Output` tab shows the output of current command-line being executed. For long-running commands like installing and building, you can switch to it to follow the progress of the execution.

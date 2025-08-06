---
uid: SmartSetup.Overview
---

# {{title}} introduction

TMS Smart Setup is a new generation tool to install libraries and components for Delphi, C++ Builder, Lazarus/FPC and .NET, brought by [TMS Software](https://www.tmssoftware.com).

**[Download TMS Smart Setup](download/index.md)**

## Usage

You can use TMS Smart Setup from command-line, or use a GUI tool. Command-line is the main way to use and recommended for fully control of your install process, for CI/CD environments. The GUI tool can be used as an alternative if you are not familiar or comfortable with using command-line tools.

## Features

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


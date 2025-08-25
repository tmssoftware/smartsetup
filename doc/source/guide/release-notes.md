---
uid: SmartSetup.ReleaseNotes
---

# Release Notes

## Version 2.1 (August, 2025)
- **New** The config file now comes with a yaml schema that provides autocompletion and syntax checking when you edit it in an editor with yaml scheme support like VSCode.
- **Improved** Rewritten config-read command can now return the full configuration as a JSON object, and it can return from the full configuration to some sections, to a specific property.

## Version 2.0 (August, 2025)
- **New**: [Smart Setup registry](https://github.com/tmssoftware/smartsetup-registry). This allows everyone to put new projects in Smart Setup.

- **New**: Servers. You can now define multiple servers where SmartSetup will look for packages. A predefined "community" server comes built-in, and it is the registry mentioned above.

- **New**: Support for different naming conventions in the packages. Now you can specify your own naming, like for example have the packages inside a folder "Delphi 11" instead of "d11" which was the only name allowed before.

- **New**: Support for automatically creating packages for projects that don't have them.

- **New**: Support for compiling a single package (with {$LIBSUFFIX AUTO}) into multiple Delphi versions. Now you can have a folder "D11+" and it will be used for Delphi 11 and newer.

- **New**: [Dcu Megafolders](xref:SmartSetup.Configuration#using-dcu-megafolders) allow you to put the result of multiple products into a single folder. This way you can shorten the library path when you are installing tens of products.

- **New**: [Delphi 13 Florence](https://blogs.embarcadero.com/help-us-choose-the-name-of-the-next-rad-studio/) beta support. This is a work in progress, but will allow you to test your components if you [have been invited to the beta](https://blogs.embarcadero.com/update-subscription-customers-invited-to-join-rad-studio-ganymede-beta/)  

- **Improved**: Documentation. We added documentation about how to create your own SmartSetup bundles, and much more. 

- **Improved**: **Breaking Change** Two new options added to `skip register` section of `tms.config.yaml`. You can now skip `registry`(adding registry entries) and `filelinks` (linking files). If you were using a form of skip registering like `skip register: [Packages, Start Menu]` you might need to review it to add the new entries. The new syntax `skip register: [All, -option1, -option2]` helps prevent breaking in the future if we add more options. 

- **Improved**: Exe compilation. Now you can specify that you don't want to compile a "Debug exe", and also to use the latest or earliest Delphi version available to compile the exe, instead of compiling it with all available Delphis.

- **Fixed**: *Install* and *Uninstall* buttons in `tmsgui` are now enabled when the product is in status *available*.

- **Breaking change**: Removed `repo-register`, `repo-unregister` and `repo-list` commands. The functionality they offered is now replaced by [servers](xref:SmartSetup.ConsumingBundles)

## Version 1.10 (June, 2025)

- **Fixed**: Command-line not accepting multiple product names, like `tms install tms.biz.bcl tms.webcore` (Regression).
- **Fixed**: Smart Setup could report that a product had a skipped dependency when it hadn't it. (it should have been fixed in 1.9 but it wasn't)

## Version 1.9 (June, 2025)

- **Fixed**: Smart Setup could report that a product had a skipped dependency when it hadn't it. [Reference](https://support.tmssoftware.com/t/unsuccessful-installation/25460).

- **Fixed**: Configuration option `build cores` was not being enforced in some situations (even when `build cores` was, say, 1, it was using more than 1 core for the build).

## Version 1.8 (June, 2025)

- **New** [tms doctor](xref:SmartSetup.Doctor) command allows to fix common errors in Rad Studio installations.

- **Fixed** If a dependency of a product was compiled without debug dcus, and the product had debug dcus, the compilation would fail

## Version 1.7 (April, 2025)

- **New**: `tms config-write` command allows to modify the configuration file from the command line. For example, to alter the IDEs and platforms that smartsetup will install, you could write `tms config-write  -p:configuration-for-all-products:delphi-versions=[delphixe,delphi12] -p:configuration-for-all-products:platforms=[win32intel,win64intel]`

- **New**: `tms config-read` command allows to read the settings in the configuration file from the command line. For example, to read the IDEs that smartsetup will install, you could write `tms config-read  configuration-for-all-products:delphi-versions`

- **Improved**: Desktop icon "TMS Smart Setup Command Prompt" renamed to "TMS Smart Setup Console". The old name was causing Windows to suggest Smart Setup instead of regular built-in "Command Prompt" when users typed "command" in Windows search. The new name will hopefully prevent Windows from doing that.

## Version 1.6 (February, 2025)

- **Improved**: Now smartsetup will allow to preserve some registry keys like license information when uninstalling.

- **Improved**: `tmsgui` now shows products sorted by status by default, meaning installed products will be listed first. [Request #24608](https://support.tmssoftware.com/t/smart-setup-initial-product-filtering/24608).

- **Improved**: `tmsgui` icon looks better in dark backgrounds. [Request #24614](https://support.tmssoftware.com/t/smart-setup-gui-icon/24614).

## Version 1.5 (January, 2025)

- **New**: Now it's possible to define a "weak dependency" in the tmsbuild.yaml definition file. If a product A has a "strong" (regular) dependency on product B, it means product B needs to be installed for product A to be installed. Now you can define dependencies on specific packages of product A - for example, Package2 has a weak dependency on Product B. That means product A can be installed even without product B, in this case, Package2 will not be compiled/installed. If product B is installed, then Package2 will also be installed.

## Version 1.4 (January, 2025)

- **New**: `tmsgui` provides a "version history" popup menu option that displays the version history for the selected product. [Request #24504](https://support.tmssoftware.com/t/version-history-in-tms-smart-setup-gui/24504/10).

- **New**: `tmsgui` allows sorting the product list by clicking column heaers.

- **New**: The YAML build definition now allows for defining registry keys to be created at install time.

- **Improved**: `tmsgui` now displays error messages in a more intuitive way: the errors are displayed in a list box, and if a specific error is clicked, more details will be displayed in a text box below. 

- **Improved**: Better error message when uninstalling a product that was previously installed manually (without fetching from remote repository).

- **Improved**: Better handling of products installed manually (without fetching from remote repository), it will properly handle if user tries to install the same product over the existing one.

- **Improved**: The presence of `$(PATH)` macro in Delphi PATH environment variable override is now being checked. If not present, the BPL directory is added directly to the variable override. It avoids errors with "bad Delphi installations" like this: https://support.tmssoftware.com/t/error-installing-flexcel-for-macos64-and-macosarm64-on-macos-14-sonoma/22213/6.

- **Fixed**: Conditional defines used in the .dproj package are now being preserved and used when building them.

- **Fixed**: `uninstall` command was rebuilding unrelated products. Now it's smarter and rebuilds only products affected by the uninstalled products. 

## Version 1.3 (November, 2024)

- **Improved**: Building a specific product (`tms build <product_id>`) was not properly recompiling dependencies and dependant packages. This was particulary affecting TMS GUI tool.

- **Improved**: Internal file renaming mechanism now more resilient to antivirus and other monitoring software that might affect file operations.

- **Fixed**: Improves iOS64 platform detection in Delphi 12. In some cases Smart Setup was wrongly assuming iOS64 platform was installed.

- **Fixed**: Avoids duplicate "PATH" environment variable which caused issues to some users in XE5/XE6.

## Version 1.2 (October, 2024)

- **Fixed**: Fixed writing of dpk files with no pas files (it was generating an invalid file). [Reference](https://support.tmssoftware.com/t/cant-compile-a-webcore-project-containing-xdata-after-using-smart-setup/23959).

## Version 1.1.1 (August, 2024)

- **Fixed**: `-json` parameter output was not a valid JSON value (regression).

- **Fixed**: `tmsgui` not working due to error "could not parse info result as JSON object" (regression).

## Version 1.1 (July, 2024)

- **New**: support for installing trials and binary packages.

- **New**: support for `.tar.zstd` compressed bundles besides `.zip`, to get smaller file sizes.

- **New**: support for compiling C++ builder cbproj files.

- **New**: `uncompress` command allows to uncompress a tms bundle, zip or zstd.

- **Improved**: now when you uninstall all packages, smart setup removes itself from the Windows Path.

- **Improved**: the uninstaller could leave some C++ generated files when uninstalling.

- **Improved**: the build process now uses shorter paths, to avoid running into MAXPATH limitations.

## Version 1.0.31 (June, 2024)

- **New**: commands `repo-register`, `repo-unregister` and `repo-list` allows for installing libraries directly from Git repositories.

- **Improved**: visual progress indicators (progress bar, title, colors) in console when running from command-line.

- **Improved**: Win64x (modern C++) compilation improved.

- **Improved**: Installed packages are now removed from the list of IDE disabled packages.

- **Improved**: GUI interface now displays per-product progress.

## Version 1.0.29 (May, 2024)

- **New**: Config file supports extra file paths for a specific platform

- **Improved**: Extra file paths now being used during the build process.

- **Improved**: Significant performance improvement when building some products. As example TMS Flexcel build time dropped by almost 90%.

## Version 1.0.28 (May, 2024)

- **New**: Support for Modern C++ platform.

## Version 1.0.25 (February, 2024)

- **Improved**: `tmsgui` enables "Install" button for installed products that are outdated, allowing updating to latest versions.

- **Improved**: installing a specific product was rebuilding other unrelated products. Now it only builds the specific installed product and dependencies.

## Version 1.0.24 (February, 2024)

- **Improved**: `tmsgui` product list now shows uninstalled products in italic, and outdated products in bold.

- **Improved**: `tmsgui` product list now shows remote version number even when listing only installed products. [Request #22398](https://support.tmssoftware.com/t/tms-smart-setup-ui-upgrade-notification-button-missing/22398).

- **Fixed**: Packages flagged as design-time and runtime not being compiled for non-win32 platforms

## Version 1.0.22 (December, 2023)

- **Improved**: Several fixes and improvements since the previous version.

## Version 1.0.0 (October, 2023)

- **New**: First version released, beta stage.
---

uid: SmartSetup.GUIToolUsage

---
# GUI tool usage

## Main Screen

Alternatively to command-line, you can use the GUI tool. Just launch `tmsgui.exe` executable. 

If the working folder is not initialized, it will first ask you what servers you want to enable:

{{#image}}tmsgui-welcome-dialog.png{{/image}}

Here, you can choose if you want to manage **tms** products, if you want to manage products from the **community** server, or both.

If you enable the tms server, you will get another dialog asking you for the credentials:

{{#image}}tmsgui-login.png{{/image}}

And then you should arrive at the main view:

{{#image}}tmsgui-main-screen.png{{/image}}

- **Search box**: type to find the product you want to install. Press Ctrl+E to move the focus to it.
- **Filter pane**: select whether to see all available products, or only installed ones.
- **Product list**: a list of all products installed or available. Press Ctrl+A to select all the products in the list. The selection is kept when the list is refreshed or filtered.
- **Status column**: shows if a product is outdated and/or pinned.
- **Server selection box**: this box is only visible if there is more than one active server, and it allows filtering the results by server.
- **Details panel**: shows information about the selected products. See [Details panel](#details-panel).
- **Output tab**: shows the output of the current command-line being executed. For long-running commands like installing and building, you can switch to it to follow the progress of the execution.
- **Status bar**: at the bottom of the main window, the GUI displays the current working folder, and the current Smart Setup version. Click the working folder to switch to another folder.

The toolbar has these buttons:

- **Configure**: opens a menu with:
  - **Common settings**: opens the [Settings dialog](#settings).
  - **TMS credentials**: asks for new credentials. Equivalent to the [tms credentials](xref:SmartSetup.Command.Credentials) command.
  - **Open config file**: creates (if missing) and opens the YAML configuration file. Equivalent to the [tms config](xref:SmartSetup.Command.Config) command.
- **Install**: downloads and installs the selected products. Equivalent to the [tms install](xref:SmartSetup.Command.Install) command.
- **Uninstall**: uninstalls the selected products. Equivalent to the [tms uninstall](xref:SmartSetup.Command.Uninstall) command.
- **Build**: opens a menu with **Full build** and **Partial build**, to rebuild the selected products. If no products are selected, rebuilds everything. Equivalent to the [tms build](xref:SmartSetup.Command.Build) `-full` and [tms build](xref:SmartSetup.Command.Build) commands.
- **Refresh**: reloads the product list.
- **Backup**: creates a backup file. See [Backup and restore](#backup-and-restore).
- **Restore**: restores products from a backup or snapshot file. See [Backup and restore](#backup-and-restore).
- **SBOM**: generates SBOM files for the selected products. Equivalent to the [tms sbom-generate](xref:SmartSetup.Command.SbomGenerate) command. When it finishes, it shows the files that were created, and you can regenerate the ones that were skipped because they already existed.
- **Doctor**: runs [tms doctor](xref:SmartSetup.Doctor) in a console window, where you choose whether you want to only diagnose, confirm each fix, or apply all fixes automatically.

While a command is running, a spinning indicator shows in the toolbar, and the progress is also shown in the taskbar button. You can press **Cancel** to stop it.

## Details panel

When you select a single product, the details panel shows its description, and buttons to install it, install another version, or uninstall it. If the product defines a `homepage` or a `version history url` in its [tmsbuild.yaml](xref:SmartSetup.CreatingBundles#application-section), the panel shows links to them. For installed products, you can also see which Delphi versions the product is installed in.

When you select more than one product, the panel lets you install or uninstall all of them at once.

## Settings

The **Common settings** dialog lets you edit the most common settings in `tms.config.yaml` without editing the YAML file:

- **General**: the alternate registry key, to install the components in a different registry key of Rad Studio. Leave it empty to use the default key.
- **Servers**: add, remove, enable or disable servers, and set their name, URL and type.
- **Products**: settings for all products, or for specific products: whether to register them in the IDE, the Delphi versions and platforms to build for, debug dcus, and defines. When you add a define, the dialog shows the defines that the installed products accept, with their descriptions.

Settings that are not in this dialog can still be edited with **Edit Config File**.

## Backup and restore

The **Backup** button creates a backup file with [tms backup](xref:SmartSetup.Command.Backup). You can back up all the products from a server, or only the selected products, and choose to use the latest versions or to choose the version of each product. When the backup is created, a dialog shows the products in the file.

The **Restore** button restores a backup file (`.snapshot.zip`) or a snapshot file (`.snapshot.yaml`) with [tms restore](xref:SmartSetup.Command.Restore). You can choose whether to register the products in the IDE, and either restore all the products in the file or select the ones you want. The list shows, for each product, whether it will be restored from the backup file or downloaded from its server.

## Errors
When there are errors, an error pane will show:

{{#image}}tmsgui-errors.png{{/image}}

If you click on the messages in the error pane, it will show more detailed information about the particular error. You can also double-click a message to see its details, or open the HTML log of the command.

## Working offline

The welcome screen that shows when the working folder is not initialized has a **Cancel** button to close the app without initializing the folder. If you disable all servers, SmartSetup works offline with the products you already have.

## Versioning

While by default SmartSetup installs the latest versions of the selected products, you can install any available [version](xref:SmartSetup.Versioning) of any product.

### Installing a version
To install a specific version, right click in the product:

{{#image}}tmsgui-main-right-click.png{{/image}}

And select "Install version":

{{#image}}tmsgui-install-version.png{{/image}}

{{#Note}}
For products hosted in an API server like tms, this dialog will show the versions available in the server. For products hosted in a git repository, it will show all the tags in the repository. For example, for Spring4D it will show:

{{#image}}tmsgui-install-version-community.png{{/image}}
{{/Note}}

{{#Important}}
For products hosted in git, you can select any tag from the list, but also type any **branch name** or **commit id**. You can select any commit, not just tagged ones.
{{/Important}}

### Pinning a version

After installing any version (including the latest), you can right click again in the product and **Pin** the product to that version. Pinned products will show with a pin in the status column, and will stay in that version until you unpin them.


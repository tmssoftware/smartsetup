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

1. **Search box**: type to find the product you want to install.
2. **Filter pane**: select whether to see all available products, or only installed ones.
3. **Product list**: a list of all products installed or available.
4. **Status column**: shows if a product is outdated and/or pinned.
5. **Server selection box**: this box is only visible if there is more than one active server, and it allows filtering the results by server.
6. **Settings**: set general settings for tms/tmsgui.
7. **Install**: download and install the selected products. Equivalent to `tms install` command.
8. **Uninstall**: uninstall the selected products. Equivalent to `tms uninstall` command.
9. **Full build/partial build**: rebuild the selected products. If no products are selected, rebuild everything. Equivalent to `tms build -full` and `tms build` commands.
10. **Credentials**: ask for new credentials. Equivalent to `tms credentials` command.
11. **Configure**: create (if missing) and open the YAML configuration file. Equivalent to `tms config` command.
12. **Output tab**: shows the output of the current command-line being executed. For long-running commands like installing and building, you can switch to it to follow the progress of the execution.
13. **Status bar**: at the bottom of the main window, the GUI displays the current working folder, and the current Smart Setup version.

## Errors
When there are errors, an error pane will show:

{{#image}}tmsgui-errors.png{{/image}}

If you click on the messages in the error pane, it will show more detailed information about the particular error.

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

### Pinning a version

After installing any version (including the latest), you can right click again in the product and **Pin** the product to that version. Pinned products will show with a pin in the status column, and will stay in that version until you unpin them.


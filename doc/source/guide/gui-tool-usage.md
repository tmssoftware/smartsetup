---

uid: SmartSetup.GUIToolUsage

---
# GUI tool usage

Alternatively to command-line, you can use the GUI tool. Just launch `tmsgui.exe` executable. 

If the working folder is not initialized, it will first ask for credentials to download products from repository. If you skip this step, no credentials will be saved and you can continue using the GUI to build projects that have been manually downloaded:

{{#image}}tmsgui-login.png{{/image}}

The GUI tool lists the installed and available products in the main listing view. 

{{#image}}tmsgui-main-screen.png{{/image}}

At the right, you have some buttons that perform on the selected products:

- **Install**: download and install the selected products. Equivalent to `tms install` command.
- **Uninstall**: uninstall the selected products. Equivalent to `tms uninstall` command.
- **Full build/Partial build**: rebuild the selected products. If no products are selected, rebuild everything. Equivalent to `tms build -full` and `tms build` commands
- **Configure**: create (if missing) and open the YAML configuration file. Equivalent to `tms config` command.
- **Credentials**: ask for new credentials. Equivalent to `tms credentials` command.

In the status bar, at the bottom of the main window, the GUI displays the current working folder, and the current Smart Setup version.

Under the hood, the GUI just executes the `tms` command-line. The `Output` tab shows the output of the current command-line being executed. For long-running commands like installing and building, you can switch to it to follow the progress of the execution.


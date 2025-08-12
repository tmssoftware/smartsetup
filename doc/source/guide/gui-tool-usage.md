---

uid: SmartSetup.GUIToolUsage

---
# GUI tool usage

Alternatively to command-line, you can use the GUI tool. Just launch `tmsgui.exe` executable. 

If the working folder is not initialized, it will first ask you what servers you want to enable:

{{#image}}tmsgui-welcome-dialog.png{{/image}}

Here, you can choose if you want to manage **tms** products, if you want manage products from the **community** server, or both.

If you enable the tms server, you will get another dialog asking you for the credentials:

{{#image}}tmsgui-login.png{{/image}}

And then you should arrive to the main view:

{{#image}}tmsgui-main-screen.png{{/image}}

1. **Search box**: type to find the product you want to install
2. **Filter pane**: select if to see all available products, or only installed ones.
3. **Product list**: a list of all products installed or available
4. **Server selection box** this box is only visible if there is more than one active server, and it allows to filter the results by server
5. **Settings**: Set general settings for tms/tmsgui
6. **Install**: download and install the selected products. Equivalent to `tms install` command.
7. **Uninstall**: uninstall the selected products. Equivalent to `tms uninstall` command.
8. **Full build/Partial build**: rebuild the selected products. If no products are selected, rebuild everything. Equivalent to `tms build -full` and `tms build` commands
9. **Credentials**: ask for new credentials. Equivalent to `tms credentials` command.
10.**Configure**: create (if missing) and open the YAML configuration file. Equivalent to `tms config` command.
11.**Output tab**: shows the output of the current command-line being executed. For long-running commands like installing and building, you can switch to it to follow the progress of the execution.
12. **Status bar**: at the bottom of the main window, the GUI displays the current working folder, and the current Smart Setup version.

When there are errors, an error pane will show:

{{#image}}tmsgui-errors.png{{/image}}

If you click on the messages of the error pane, it will show more detailed information about the particular error.



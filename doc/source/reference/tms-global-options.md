---
uid: SmartSetup.Command.GlobalOptions
---

# Global Options

Options available to every tms command.

## Options

| Option                         | Description                                                                                                                                    |
| ------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| `-config:<file>`               | Specifies an alternate configuration file to use instead of the default `tms.config.yaml`.                                                     |
| `-add-config:<file>`           | Loads an additional configuration file and merges its options with the current configuration. Use it to override global settings (which you might store in a git repo) with machine-dependent settings (which are stored locally in every machine).|
| `-verbose`                     | Shows detailed output, including trace-level log entries.                                                                                 |
| `-display-options:<options>`   | This option is for other programs that call tms.exe. It controls what tms displays in the console, providing information that might be useful to them, but not to an interactive user. Accepts a comma-separated list of option names to toggle. Valid values: <ul><li>`no-ansi`: Suppresses ansi colored output. Smartsetup does this automatically when it detects it is not writing to a physical terminal, but in some cases it might be needed. It ensures you are not getting ANSI escape codes in the output. </li><li> `product-progress`: Shows the progress for each product, not just the total. This can be used by a GUI app to show the progress in parallel by product. </ul> |
| `-property:<path>=<value>`     | Overrides a single parameter from the configuration file. Can be specified multiple times. Short alias: `-p`. Example: `-p:configuration-for-all-products:platforms=[win32intel,win64intel]`. For more information, read []|

## See Also

- [tms config](xref:SmartSetup.Command.Config)

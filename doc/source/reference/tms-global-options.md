---
uid: SmartSetup.Command.GlobalOptions
---

# Global Options

Options available to every tms command.

## Options

| Option                         | Description                                                                                                                                    |
| ------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| `-config:<file>`               | Specifies an alternate configuration file to use instead of the default `tms.config.yaml`.                                                     |
| `-add-config:<file>`           | Loads an additional configuration file and merges its options with the current configuration.                                                  |
| `-verbose`                     | Shows more detailed output, including trace-level log entries.                                                                                 |
| `-display-options:<options>`   | Controls what tms displays in the console. Accepts a comma-separated list of option names to toggle. Valid values: `no-ansi`, `product-progress`. |
| `-property:<path>=<value>`     | Overrides a single parameter from the configuration file. Can be specified multiple times. Short alias: `-p`. Example: `-p:configuration-for-all-products:platforms=[win32intel,win64intel]`. |

## See Also

- [tms config](xref:SmartSetup.Command.Config)

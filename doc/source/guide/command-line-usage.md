---

uid: SmartSetup.CommandLineUsage

---

# Command-line usage

TMS Smart Setup is folder-based. Once you have the folder initialized, you can run the command-line and Smart Setup will create files and subfolders inside it.

The command-line is self-explanatory; just run `tms` to list all available commands, or `tms help <command>` for more detailed information about a specific command.

## Setting up credentials

Once in the folder, run `tms credentials` to initialize the folder and input your credentials:

```shell
tms credentials
```

## Installing products

Then, use the `install` command to download new products from the remote repository to your local machine. 

```shell
tms install tms.biz.aurelius
```

It's worth noting that the above command will download and install TMS Aurelius and also all its dependencies (in this case, TMS BIZ Core Library ).

To find out what the ids of the products you have available to install are, you can run `list-remote` command:

```shell
tms list-remote
```

You can also specify multiple products to install, separated either by commas or spaces. Masks can be used to install all products that match the mask. For example, the following command installs all BIZ products and TMS FlexCel for VCL:

```shell
tms install tms.biz.* tms.flexcel.vcl
```

## Updating products

From time to time, you can run `update` command to download newest versions of the products, if available:

```shell
tms update
```

The above command will check in the remote repository for new versions of all products you have installed. If there are new versions, it will download, update and rebuild them, all automatically.

## Rebuilding

When installing or updating products, TMS Smart Setup only rebuilds what has been modified. This makes the installation and update processes very fast. 

But there are times that the build process might fail, for any reason (Delphi misconfiguration, antivirus in action, bugs, etc.). Here, you will have your products downloaded but not properly built and registered in the IDE. 

To force a new rebuild to fix these issues, just call the `build` command:

```shell
tms build
```

This command will just try to rebuild what has been modified or failed, and quickly fix that. In case things are really not working, you can ask for a full rebuild, which will rebuild and re-register all your products:

```shell
tms build -full
```

You can also use the `build` command if you updated the source code of TMS products yourself. After modifying the source code, call `build` command to update the installation properly.

## Uninstalling a product

Uninstalling a product is as simple as installing. Just call `uninstall` passing the products to be uninstalled. It also accepts masks and comma-separated ids:

```shell
tms uninstall tms.biz.aurelius
tms uninstall tms.biz.*,tms.flexcel.vcl
```

Note that `uninstall` command **does not** uninstall dependencies. Thus, if you ask to uninstall `tms.biz.aurelius` only, then `tms.biz.bcl`, which is a TMS Aurelius dependency, will remain installed. To uninstall a product and all its dependencies, use the `-cascade` option:

```shell
tms uninstall tms.biz.aurelius -cascade
```

It might be possible that you try to uninstall a product that another installed product depends on. For example, if you try to uninstall TMS BCL but not TMS Aurelius, which depends on BCL. In this case, `uninstall` will fail. You can bypass this check by adding the `-force` parameter. But we don't recommend it, because you will end up with broken products. **Only use `-force` if you really know what you are doing**.

## Custom configuration

TMS Smart Setup works smoothly out of the box. It automatically detects all your installed Delphi IDEs and platforms, in summary, your current system setup, and installs everything the best way possible in your environment.

But if you want more control over its behavior, you can fully customize its behavior by using a YAML config file.

You can ask the tool to create a pre-configured YAML configuration file by running the `config` command:

```shell
tms config
```

This will create a YAML file named `tms.config.yaml` in your root folder, and launch your default YAML editor to edit its content.

The default YAML config file contains all the configuration options you can change, and each of them is fully documented. Just read the file to learn about it and modify the options as you wish. For more information, see the [configuration guide](xref:SmartSetup.Configuration).

## Check what is installed

You can use the command `tms list` to check all products you have installed locally:

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

## Self-updating

`tms.exe` can update itself. It will regularly check for new versions, and if available, it will warn you about it.

At any time, and especially if you get such warning, you can update `tms.exe` to a newer version by executing `self-update`:

```shell
tms self-update
```

We recommend not ignoring the warnings about a new version, and always keeping the tool updated to the latest version.

## Automation
A common use-case for the tms executable is to call it from your own scripts, or even from a GUI. That's how tmsgui works under the hood. In those cases, the following commands and options might come in handy:


### `-json` parameter, to get the results in a json object instead of plain text. 

For example:
```shell
tms list -json
tms list-remote -json
tms credentials -print -json
tms info -json
tms server-list -json
tms config-read -json
```

### `-p` command, to pass a configuration to tms 

Our configuration is normally done in `tms.config.yaml`. This allows all your settings to be in a single version-controlled file. 
But sometimes, you might want to call tms with some specific configuration, but not alter the existing `tms.config.yaml`.
In those cases, you can use the "-p" parameter to override any property in `tms.config.yaml`

The rules are: 
 1. Look at the path for the property in tms.config.yaml. Say we want to change the skip-register setting: it is under `configuration for all products`, then `options`, then `skip register`
 2. Replace the spaces with "-" signs. *Note: This step is optional. You can still write the names with spaces, but you will need to quote them so the command line accepts them*.
 3. Join the sections with ":"
 4. If the variable you want to set is an array (like, for example, the delphi versions), you set it by putting the elements between brackets and separating them with commas. For example: [delphi11,delphi12]. You can specify if you want to add those values to the existing array or replace the existing array by prepending `add-` or `replace-` to the name of the property. If the property is "delphi-versions", you can set "add-delphi-versions" instead to add to the existing values. ("replace" is the same as writing nothing, but we have the option so you can be more explicit in what you want to do)

 Some examples (the first and the second are similar, but the second omits step 2 above):

```shell
tms build -p:configuration-for-all-products:options:skip-register=true
tms build -p:"configuration for all products:options:skip register=true"
tms build -p:configuration-for-all-products:replace-platforms=[win32intel,win64intel] -p:configuration-for-all-products:replace-delphi-versions=[delphi12]
```

{{#Tip}}
Sometimes it might not be easy to figure out the exact syntax to change a setting. **But there is a simple way**.
`tms config-read` has a parameter: `-cmd`, which will list all the existing configuration options with the syntax `-p` uses.  So for example, you would do:

```
tms config-read -cmd
```
And get this result:
```
-p:"tms smart setup options:build cores = 0"
-p:"tms smart setup options:alternate registry key ="
-p:"tms smart setup options:working folder ="
-p:"tms smart setup options:prevent sleep = true"
-p:"tms smart setup options:versions to keep = -1"
-p:"tms smart setup options:error if skipped = false"
-p:"tms smart setup options:excluded products = []"
-p:"tms smart setup options:included products = []"
-p:"tms smart setup options:additional products folders = []"
-p:"tms smart setup options:auto snapshot filenames = [tms.snapshot.yaml]"
-p:"tms smart setup options:servers:tms:enabled = true"
-p:"tms smart setup options:servers:community:enabled = true"
...
```

So, if you wanted to change the autosnapshot filenames to save to two places, you can just copy from the results above and modify them:
```
tms config-write -p:"tms smart setup options:auto snapshot filenames = [tms.snapshot.yaml, ../../tms.snapshot.yaml]"
```
{{/Tip}}

### `tms config-read` and `tms config-write` to read and change tms.config.yaml
These two commands allow you to read or update a setting from `tms.config.yaml`. Different from the `-p` parameter, `tms config-write` will modify the actual file. This can be useful, for example, when doing a GUI: You can use `tms config-read` to read a value from the config file and show it to the user. When the user modifies it, you can use `tms config-write` to write it back.

The syntax for specifying the setting to read or write is the same as the one in the `-p` parameter above. In fact, `config-write`, when called alone, just reads your settings and writes them back, reformatting the config file. You need to use the `-p` parameter to alter that configuration, so what is written is different from the existing settings.

Examples:
```shell
tms config-read configuration-for-all-products:delphi-versions
tms config-write -p:configuration-for-tms.flexcel.vcl:replace-platforms=[] -p:tms-smart-setup-options:prevent-sleep=false -p:tms-smart-setup-options:git:git-location="" -p:configuration-for-all-products:replace-platforms=[]
```

{{#Important}}
`tms config-write` will reformat and remove all manually entered comments in tms.config.yaml. See [configuration](xref:SmartSetup.Configuration)
{{/Important}}

`tms config-read` can be called with a full path to a property, like `tms config-read "tms smart setup options:build cores"`, or it can be called with a partial path or even no path at all. If you call `tms config-read` alone, it will output the full configuration file to the screen. By default, this will be in YAML format, but you can call `tms config-read -json` to get a JSON object with all the configuration, or `tms config-read -cmd` to get the properties in a syntax that you can copy and paste to use in the `-p` parameter.

## Fixing problems
To open a browser showing a complete log of the last command, you can type:

```shell
tms log-view
```
If you are still having issues that you can't solve, you might try

```shell
tms doctor
```
You can find more information about [tms doctor here](xref:SmartSetup.Doctor)

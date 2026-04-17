---
uid: SmartSetup.ContinuousIntegration
---

# Using SmartSetup in Continuous Integration (CI) environments

SmartSetup is two tools in one: It is a tool that downloads components from a server or a public repository (GitHub, Bitbucket, etc), but it is also a tool that builds Delphi components and full applications. 

In this document we will study how you can use SmartSetup to build your own projects and components in an automated way.

## Advantages of using SmartSetup to build your projects

While you can also use the classic msbuild (and even dcc32.exe) to build your applications, building them with SmartSetup has some advantages.

  * No need to install any components. This is particularly useful in a server or CI environment, because installing a particular version of a component might break other builds running in the same server. I you are building only with SmartSetup, you don't have to worry about modifying Delphi in any way in the server.
  * SmartSetup can build in parallel, taking dependencies in account. So if one of your products depends in Component A, it won't start building until Component A is compiled. But it might start before Component B finishes, if this product doesn't require Component B.
  * It is simpler to configure all build parameters from a single `tms.config.yaml`. If you now want to build in Delphi 13 instead of 12, you only need to modify that file, and all components and projects will be built with Delphi 13.

## Making your projects SmartSetup-aware

To make your project build with smartsetup, you need to generate a tmsbuild.yaml file on its root folder. 
The simplest way to do this is to cd to the root folder, and type:
```
tms spec
```
[tms spec](xref:SmartSetup.Command.Spec) will ask you a few questions, and try to create a tmsbuild.yaml file for you. You can then edit that file and change it to your needs.

{{#Tip}}
If you edit the file with Visual Studio Code and the yaml extensions installed, you will get error highlight and autocomplete. We provide a [schema for the file](https://github.com/tmssoftware/smartsetup/blob/main/tms/example-config/tmsbuild.schema.json) so any editor that can understand that schema should be able to help.
{{/Tip}}

Or, if you prefer, you can [download a full empty tmsbuild.yaml](https://github.com/tmssoftware/smartsetup/blob/main/tms/example-config/tmsbuild.yaml) and modify it, without using `tms spec`. 

You can find more information about editing this file [in the docs](xref:SmartSetup.CreatingBundles)
And you can see an example of an app adapted to build with SmartSetup here: https://github.com/agallero/multide  
This particular example doesn't depend in any components, but it can give you an idea of how to organize it.

## Configuring the build

Once you have your projects structured, they should look like this:

```
                    ┌───────────────────────┐
                    │  root/tms.config.yaml │
                    └────────────┬──────────┘
                                 │
          ┌──────────────────────┼──────────────────────────┐
          │                      │                          │
┌─────────▼────────────┐ ┌───────▼──────────────┐ ┌─────────▼────────────┐
│Project1/tmsbuild.yaml│ │Project2/tmsbuild.yaml│ │Project3/tmsbuild.yaml│
└──────────────────────┘ └──────────────────────┘ └──────────────────────┘
```

In the root folder you have a `tms.config.yaml` file that sets global settings about how everything will be compiled, and then you have folders with your projects, each one with its own `tmsbuild.yaml`. We recommend that you put `tms.config.yaml` in version control, so everyone cloning the structure will get the same compilation settings.

{{#Tip}}
If you want different settings for CI than for normal building, you can store the CI settings in a different file, like `tms.config.ci.yaml`, and then call `tms build -config:tms.config.ci.yaml`. See [Global Options](xref:SmartSetup.Command.GlobalOptions)
{{/Tip}}

{{#Important}}
As mentioned at the start, for a CI build, you **don't want to register the components in the IDE**. **So make sure you set `skip register` to true in the tms.config.yaml file for CI**
{{/Important}}

### Global options and machine-dependent options

Sometimes, you might want to have some configuration options (like paths) that vary machine from machine. You can do this by adding an extra `tms.config.local.yaml` to the root folder and adding it to `.gitignore`, or specifying one with [-add-config](xref:SmartSetup.Command.GlobalOptions)

See [below](#adding-extra-settings-to-an-existing-configuration-file) for more information.

## Snapshots and restoring

We also recommend you store a [snapshot](xref:SmartSetup.Versioning#snapshots) of all the components and versions you are developing with at the root. You don't want to do a release build with some component versions you haven't tested in development.

## Building

Once you have the structure, configuration files and snapshots, the steps to build should be:

1. Clone the repo (or pull the changes) in the build server
2. Restore the snapshot with `tms restore -skip-register snapshot.yaml`. This will ensure the correct versions of everything is used.
3. call `tms build -full` to create a new build.


## Advanced: Automating command calls.

When creating scripts to automate a build, the following commands might come handy:


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
[tms config-read](xref:SmartSetup.Command.ConfigRead) has a parameter: `-cmd`, which will list all the existing configuration options with the syntax `-p` uses.  So for example, you would do:

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

### Adding extra settings to an existing configuration file.

As mentioned before, if you have am existing configuration, but you want to temporarily change a value, you can do it with `-p`. But if there are many `-p` parameters, the command line might become too complex. An alternative, which is 100% equivalent, is to use the `-add-config` command.

Let's say you have an existing configuration, but you now want to build in a CI server, so you don't want to register any component.

You could do it with:
```
tms build -p:"configuration for all products:options:skip register = true"
```
But you can also create a CI config file, let's call it `tms.ci-config.yaml`. Inside that file you can have just the lines:
```yaml
configuration for all products:
   options:
      skip register: true
```
And then call 
```
tms build -add-config:tms.ci-config.yaml
```
This will have the same effect as the `-p` line above, but it might be simpler to maintain. You can also add multiple configs:
```
tms build -add-config:tms.ci-config.yaml -add-config:tms.delphi12-config.yaml
```
The line above will load the configuration in `tms.config.yaml`, then apply the changes in `tms.ci-config.yaml` and finally apply the changes in `tms.delphi12-config.yaml`.

{{#Note}}
The same rule 4 in [the section about -p](#-p-command-to-pass-a-configuration-to-tms) applies here. If you are replacing an array, you can use `add ` or `replace ` prefixes to control the behavior:

If the original tms.config.yaml had delphi11 as the delphi version, then adding this config:

```yaml
configuration for all products:
   add delphi versions:
    - delphi12
    - delphi13
```
will add delphi12 and delphi13 to the existing ones, resulting in `delphi versions=[delphi11, delphi12, delphi13]`

On the other hand, if you add this configuration:
```yaml
configuration for all products:
   replace delphi versions:
    - delphi12
    - delphi13
```
it will replace [delphi11] array with the new one, and the result will be `delphi versions=[delphi12, delphi13]`

{{/Note}}

{{#Tip}}
Since SmartSetup 3.2, you can also create a file named `tms.config.local.yaml` and this file will be automatically added to your config, similar as if you had specified `-add-config:tms.config.local.yaml`. But if you use that specific name, you won't need to add the `-add-config` parameter to every call, it will be loaded automatically.
{{/Tip}}

### `tms config-read` and `tms config-write` to read and change tms.config.yaml
These two commands allow you to read or update a setting from `tms.config.yaml`. Different from the `-p` parameter, [tms config-write](xref:SmartSetup.Command.ConfigWrite) will modify the actual file. This can be useful, for example, when doing a GUI: You can use [tms config-read](xref:SmartSetup.Command.ConfigRead) to read a value from the config file and show it to the user. When the user modifies it, you can use [tms config-write](xref:SmartSetup.Command.ConfigWrite) to write it back.

The syntax for specifying the setting to read or write is the same as the one in the `-p` parameter above. In fact, [tms config-write](xref:SmartSetup.Command.ConfigWrite), when called alone, just reads your settings and writes them back, reformatting the config file. You need to use the `-p` parameter to alter that configuration, so what is written is different from the existing settings.

Examples:
```shell
tms config-read configuration-for-all-products:delphi-versions
tms config-write -p:configuration-for-tms.flexcel.vcl:replace-platforms=[] -p:tms-smart-setup-options:prevent-sleep=false -p:tms-smart-setup-options:git:git-location="" -p:configuration-for-all-products:replace-platforms=[]
```

{{#Important}}
[tms config-write](xref:SmartSetup.Command.ConfigWrite) will reformat and remove all manually entered comments in tms.config.yaml. See [configuration](xref:SmartSetup.Configuration)
{{/Important}}

[tms config-read](xref:SmartSetup.Command.ConfigRead) can be called with a full path to a property, like `tms config-read "tms smart setup options:build cores"`, or it can be called with a partial path or even no path at all. If you call [tms config-read](xref:SmartSetup.Command.ConfigRead) alone, it will output the full configuration file to the screen. By default, this will be in YAML format, but you can call `tms config-read -json` to get a JSON object with all the configuration, or `tms config-read -cmd` to get the properties in a syntax that you can copy and paste to use in the `-p` parameter.


---

uid: SmartSetup.Configuration

---
# Introduction

Configuration in SmartSetup is done by editing a `tms.config.yaml` file. From the command line, you can just call `tms config` to open it in the default editor. From tmsgui, there is a "Configuration" button that you can press. 

{{#Note}}
SmartSetup comes with no configuration file by default. To create it, you have to call `tms config` or equivalent. Once it is created, you can just edit it with your editor of choice.
{{/Note}}

Here is how it looks in Visual Studio Code. 
{{#image}}tms-config-yaml.png{{/image}}

The file is self-commented, so you can find help about all the options just by reading inside. In this guide, we are just going to look over the most important settings you might want to change.

{{#Important}}
**Do not store your own comments inside this configuration file**
Some commands like `tms server enable` or `tms config-write` will override the config file with a clean file (keeping your configuration). They will remove all your comments and reformat the file. GUI apps will also call those commands internally, so it is very likely that they will regenerate the config file.
{{/Important}}

## Updating your configuration file

From time to time, we might add new properties to the config file, change comments, or add documentation. If you have an older config file with an existing configuration, you won't see those new properties when you update. 

There is a simple solution for this. Just type:
```shell
tms config-write
```
And the config file will be updated to the newest version, while keeping all your settings. This is the reason we can't preserve your own comments in the file: If you changed a comment and we changed it too, we would need to merge both versions and there might be conflicts. The simplest solution is just to regenerate the config file and discard your comments and whitespace changes. Every time you do a `config-write` the file will be reformatted clean, and all metadata updated. New properties that we added will also be added to the file. 

## Shared configuration and specific configuration for a single product

Sometimes, you might want to apply different settings for different products. For example, you might want to build product **example.a** only for Win32 (even if product **example.a** supports other platforms), and all the other products for all the platforms they support. To do that, you can create a specific "Configuration for product example.a" section inside your tms.config.yaml. It will look like this:

```yaml
configuration for all products:
  ...
configuration for example.a:
  platforms:
    - win32intel

```
You only need to specify the settings that differ from "all products" in the product-specific configurations. All the others will be inherited.

{{#Note}}
Try to keep it simple. The more product-specific settings you add, the more complex the config file will be to maintain. If possible, stick to just a single global configuration.
{{/Note}}

## Basic configuration
The most likely things that you might want to review are:
  * **build cores**: By default, SmartSetup will try to use them all, and that's normally what you will want. But if this stresses your machine too much, you can reduce the number of cores used.
  * **skip register**: The default is that we register all components in the IDE. If you just want to build, but not change any settings, set this property to false.
  * **debug dcus**: If you don't change it, SmartSetup will compile the libraries in both debug and release modes. While this will allow you to step into the code in those libraries while debugging, it will also more than double the size used in disk, and increase the build time (since each library has to be compiled twice)
  * **delphi versions**: The default if nothing is selected is that SmartSetup will try to compile the library for all the Delphi versions installed in your machine. If you just want to install for a single delphi version (or multiple), you can configure that here.
  * **platforms**: Similar to **delphi versions** above, if you don't specify any platform, SmartSetup will try to compile for all the platforms that the library supports and that you have installed. If you only care about some specific platforms, you can change it with this setting. 

## Defines

## Using Dcu Megafolders
---
uid: SmartSetup.CreatingBundles
---

# Creating SmartSetup bundles

SmartSetup is designed to compile and install not only tms-created packages but any package or application. Adding SmartSetup support to your own components and applications will let you use them as dependencies in the build system and make them available to others in a simple way. You will also be able to compile full applications without registering any of the needed dependencies into Rad Studio. 

## Naming conventions for packages

Currently,  SmartSetup requires a specific naming convention for packages. This is something that we are looking to improve, but today, to make your component compatible with SmartSetup, you need to have the packages in the following folders inside a single folder:

 * lazarus 
 * d6 
 * d7
 * d2005
 * d2006 
 * d2007
 * d2009
 * d2010
 * dxe
 * dxe2
 * dxe3
 * dxe4
 * dxe5
 * dxe6
 * dxe7
 * dxe8
 * dseattle
 * dberlin
 * dtokyo
 * drio
 * dsydney
 * d11
 * d12

{{#Note}}
In SmartSetup we normally use the convention of putting all the packages inside a **packages** folder, but that's not necessary.
SmartSetup will find the packages wherever you put them. The only requirement is that they all have the same root folder.
{{/Note}}

So, for example, the packages for Delphi 12 support might be in the folder 
```
<root>\packages\d12
```
{{#Note}}
Applications don't require any special folders, the dprojs for exes can be anywhere.
{{/Note}}

{{#Important}}
Packages to be used in smartsetup must all **have the same name** use **LibSufix** to differentiate between Delphi versions.
{{/Important}}

## Creating a tmsbuild.yaml file

Once you have all the files you want to deploy into a folder, with the packages in the correct places, you will need to create a `tmsbuild.yaml` file at the root.
You can get an up-to-date example here:
https://github.com/tmssoftware/smartsetup/blob/main/tms/example-config/tmsbuild.yaml

The sections of the file are discussed below.

### **application** section

Here you just need to provide basic information about the package. It is recommended that you have a file "version.txt" at the root of your bundle, where SmartSetup can read the version number. This version number will be used in many places, so you should have tools to update it automatically.

### **supported frameworks** section

In this section you will define "Frameworks" that will later be used by the packages. A framework definition in smartsetup is just a group of supported **IDEs** and **Platforms**. You might define multiple frameworks for your packages, and each package can refer to different frameworks.

So, for example, you can have:

```
Framework1:
    ide since: delphixe2
    ide until: delphi12
    platforms:
      - all
      - linux64: false

```
and

```
Framework2:
    ide since: delphi11
    platforms:
      - win32intel

```
And then a package might say it supports Framework1 or Framework2, or both. 

We normally use the frameworks to specify stuff like "VCL", "FMX", "LCL", etc. So your package might suport FMX but not VCL or vice-versa, or it might support both (if it is a core package). But there is no built-is knowledge in SmartSetup about that stuff. You can define any framework name you want, with any combination of IDEs and Platforms you want.

{{#Note}}
It is not necessary to include the line: "ide until". If you don't, SmartSetup will assume the Framework supports all IDEs since "inde since". This way, you don't need to modify the file every time a new Delphi version comes out.
{{/Note}}

{{#Note}}
A framework can include *weak* dependencies. A *weak* dependency is a dependency that is only used if installed. With a normal dependency, if package A depends on B, and B isn't available, you will get an error. With a weak dependency, if pakage B isn't available, then Package A won't be installed either. You can use weak dependencies to add extra functionality to your packages **only** if another package is already installed. For example, your product might get the ability to encrypt files, only if tms crypto pack is installed. If it isn't, then the package in your product that provides the functionality will be skipped, as it has a weak dependency with crypto pack.
{{/Note}}

### **packages** section

Here you need to specify the packages that smartsetup has to build and/or install. If your package is named `MyPackage.dproj` then you have to add an entry with the text `MyPackage`. Then specify the frameworks that this package supports. For example, it might be a VCL-only package, and then only support Win32/64 and not other platforms like Linux. The names of frameowrks you can use were defined in the previous section, but there are two additional always-available ones:
  * design: This is a design package.
  * runtime: This is runtime package. 

### **paths** section

By default SmartSetup will automatically add all needed paths to Delphi when installing. But you might need to specify additional paths to be added to the library/browsing path that SmartSetup can't figure out by itself. You can use this section to add those additional paths.


## Using your bundle

## Making the bundle accessible for more users

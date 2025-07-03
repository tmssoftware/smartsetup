---

uid: SmartSetup.CreatingBundles

---

# Creating SmartSetup bundles

SmartSetup is designed to compile and install any package or application, not only tms-created packages. Adding SmartSetup support to your own components and applications will let you use them as dependencies in the build system, and make them available to others. You will also be able to compile full applications without registering its dependencies in Rad Studio.

## SmartSetup bundles composition

A SmartSetup bundle is just a folder with the files you need inside, together with a `tmsbuild.yaml` file at the root. The folder might be compressed (we currently support **zip** and **tar.zstd** compression), or it might be just a folder with files. The `tmsbuild.yaml` file is used to configure how SmartSetup will compile the bundle. 

* Zip files are preferred for distributing source code to many users. Zip is the most common compression method, and everyone can easily unzip the bundles to see the files inside.

* Tar.zstd files are preferred for distributing bundles that have big binary files inside (like a trial or a pre-compiled bundle). [Zstd](https://facebook.github.io/zstd/) is a newer compression method that can compress much more than zip, and is much faster at similar compression levels. Windows 11 also has direct support for looking at `.tar.zstd` files directly in Windows Explorer, so there is no need for users to install any additional tool if they want to uncompress the bundle manually. And virtually any modern third-party tool supports them too.

* Source folders are preferred for development, and also if you are sharing a component directly from a GIT/SVN repository. 

## Structure for library packages

There are some few requirements for the packages SmartSetup can work with:

1. SmartSetup requires packages, it isn’t designed to work with “source only” distributions.  But if you don’t have them, SmartSetup can create them for you. See the [package definitions section.](#package-definitions-section) 

2. All packages must be in the same root folder, inside another folder with the Rad Studio version (`package root\delphi version`). So, for example, you might have your Delphi 12 packages in `\packages\d12` and your Delphi 13 packages in `\packages\d13`. You might also have them in `\my packages\delphi\Rad Studio 12 Athens` and `\my packages\delphi\Rad Studio 13 Florence`. What you can’t have is for example `\packages\delphi\delphi12` and `\packages\lazarus`. You need a single package root, and then all Delphi/Lazarus packages should be inside a single folder.

3. All packages for different Delphi versions must have the same name, and must have a [{$LIBSUFFIX}](https://docwiki.embarcadero.com/RADStudio/en/Compiler_directives_for_libraries_or_shared_objects_%28Delphi%29) directive inside to differentiate the version. You can use any LIBSUFFIX you want, and also  `{$LIBSUFFIX AUTO}` for Delphi 11 and newer.

### Standard SmartSetup naming

While you can use any naming convention you want, if you are starting a library from scratch and want to follow SmartSetup conventions, the packages should be all inside folders named `<root>\packages\<version>`. Where `<version>` is any of:

 * lazarus, d6, d7, d2005, d2006, d2007, d2009, d2010,
 * dxe, dxe2, dxe3, dxe4, dxe5, dxe6, dxe7, dxe8
 * dseattle, dberlin, dtokyo, drio, dsydney
 * d11, d12, d13

{{#Note}}

If your package has {$LIBSUFFIX AUTO} and it supports multiple Delphi versions, you can append a “+” after any of the names above to indicate this is the folder for that version and all newer versions, until the next “+” folder. For example, you might have:

 * `packages\drio`

 * `packages\dsydney`

 * `packages\d11+`

And the “d11+” folder will be used for all Delphi versions after and including Delphi11. If in the future, say Delphi 2048 introduces a breaking change in the packages and you need a new package for them, you can add a line:

 * `packages\d2048+`

With this extra line, all packages \>= D11 but \<d2048 will use the d11+ folder. All packages \>= Delphi 2048 will use the d2048 folder.

{{/Note}}

If your packages have a different naming convention, it is likely we can still compile them, as long as they match the requirements above. But you will have to tell SmartSetup manually where they are, using the file `tmsbuild.yaml`

## Structure for applications

SmartSetup can also compile applications, not just packages. For applications, the only restriction is:

1. You can have a single application per product. 

If you need to compile multiple applications, each one must go in their own product, with their own `tmsbuild.yaml`. Other than that, they might be anywhere below the root folder, and there is nothing you need to do for SmartSetup to find them. 

## Creating a tmsbuild.yaml file

Once you have all the files you want to deploy into a folder, with the packages in the correct places, you will need to create a `tmsbuild.yaml` file at the root.

You can get a commented, up-to-date example with all the properties here: https://github.com/tmssoftware/smartsetup/blob/main/tms/example-config/tmsbuild.yaml

And you can also look at the [SmartSetup registry](https://github.com/tmssoftware/smartsetup-registry/tree/main) to see actual files for actual projects that SmartSetup supports.

The sections of the file are discussed below.

{{#Note}}

If you follow standard naming for your folders and libsuffixes, the tmsbuild.yaml file can be very simple. You can see an example here: [Landgraf AWS SDK for Delphi](https://github.com/tmssoftware/smartsetup-registry/blob/main/landgraf.aws.sdk/tmsbuild.yaml)

{{/Note}}

### **application** section

Here you just need to provide basic information about the package. It is recommended that you have a file “version.txt” at the root of your bundle, where SmartSetup can read the version number. This version number will be used in many places, so you should have tools to update it automatically.

### **supported frameworks** section

In this section, you will define “Frameworks” that will later be used by the packages. A framework definition in SmartSetup is just a group of supported **IDEs** and **Platforms**. You might define multiple frameworks for your packages, and each package can support multiple frameworks.

So, for example, you can have:

```yaml
Framework1:
  ide since: delphixe2
  ide until: delphi11

  platforms:  
    - win32intel

```

and

```yaml

Framework2:
  ide since: delphi12

  platforms:
    - win32intel
    - win64intel
    - win64xintel

```

And then a package might say it supports Framework1 or Framework2, or both. If it supports both, then it will support only win32 from Delphi XE2 to Delphi 11, and it will support win32/win64/win64x for Delphi 12 and newer.

We normally use the frameworks to specify stuff like “VCL”, “FMX”, “LCL”, etc. So your package might support FMX but not VCL or vice-versa, or it might support both (if it is a core package). But there is no built-in knowledge in SmartSetup about that stuff. You can define any framework name you want, with any combination of IDEs and platforms you want.

{{#Note}}

It is unnecessary to include the line: “ide until”. If you don’t, SmartSetup will assume the Framework supports all IDEs since “ide since”. This way, you don’t need to change the file every time a new Delphi version comes out.

{{/Note}}

{{#Note}}

A framework can include *weak* dependencies. A *weak* dependency is a dependency that is only used if installed. With a normal dependency, if package A depends on B, and B isn’t available, you will get an error. With a weak dependency, if package B isn’t available, then Package A won’t be installed either. You can use weak dependencies to add extra functionality to your packages **only** if another package is already installed. 

For example, your product might get the ability to encrypt files, only if tms crypto pack is installed. If it isn’t, then the package in your product that provides the functionality will be skipped, as it has a weak dependency with crypto pack.

SmartSetup is smart enough that if you later install crypto pack, it will recompile your product automatically and install the package that had been skipped because crypto pack wasn’t there.

{{/Note}}

### **packages** section

Here you need to specify the packages that SmartSetup has to build and/or install. If your package is named `MyPackage.dproj` then you have to add an entry with the text `MyPackage`. Then specify the frameworks that this package supports. For example, it might be a VCL-only package, and then only support Win32/64 and not other platforms like Linux. The names of frameworks you can use were defined in the previous section, but there are three additional always-available ones:

  * design: This is a design package.

  * runtime: This is a runtime package.

  * exe: This is an application, not a component.

### **package options** section.

You only need this section if you have packages that are not using the [Standard SmartSetup naming](#standard-smartsetup-naming).

Here, you can specify the naming for your packages if they are not “d11, d12, etc”, and also the LIBSUFFIX you are using if it is not the standard Delphi uses (280, 290, etc) There are also some options like automatically adding a LIBSUFFIX if it isn’t there, etc. Each property is documented in the [example tmsbuild.yaml](https://github.com/tmssoftware/smartsetup/blob/git-registry/tms/example-config/tmsbuild.yaml) but you can remove this section completely if you don’t need to adapt the packages.

You can find some examples of real-world usage for this section here:

   * [Different package naming and different LIBSUFFIX](https://github.com/tmssoftware/smartsetup-registry/blob/main/sglienke.spring4d/tmsbuild.yaml)

   * [A single package supporting all Delphi versions with LIBSUFFIX AUTO](https://github.com/tmssoftware/smartsetup-registry/blob/main/pyscripter.python4delphi/tmsbuild.yaml)

### **package definitions** section

This section is only if the library you are adapting for SmartSetup doesn’t have packages at all. We recommend you ship packages with your bundle, because that way, users can also manually install the components by unzipping them, opening the packages and registering them. But if you prefer, you can auto-generate them, and that’s what this section is for.

Again, you can find the documentation for each property in the [example tmsbuild.yaml file](https://github.com/tmssoftware/smartsetup/blob/git-registry/tms/example-config/tmsbuild.yaml), but here you define some basic properties (name of the package, description, etc) and the files it will contain. All packages defined here must have been defined previously in the [packages section](#packages-section)

In the files section, you can define multiple “sources” for the files. In each source, you can specify a mask to include files, another to exclude files, and if you want recursively include all files in children folders too. If recursively including files, you can also specify masks for including and excluding folders below the root folder.

You can find some real-world examples of libraries that don’t have packages here:

[Delphi TOML](https://github.com/tmssoftware/smartsetup-registry/blob/main/pyscripter.toml.delphi/tmsbuild.yaml)

[Delphi AST](https://github.com/tmssoftware/smartsetup-registry/blob/main/yankovsky.delphi.ast/tmsbuild.yaml)

{{#Note}}

Currently, SmartSetup supports generating packages for Delphi XE or newer. It will also generate one package by version before Delphi 11, and a “d11+” folder for Delphi11 and newer.

{{/Note}}

### **paths** section

By default, SmartSetup will automatically add all needed paths to Delphi when installing. But you might need to specify additional paths to be added to the library/browsing path that SmartSetup can’t figure out by itself. You can use this section to add those additional paths.

{{#Note}}

Try to add as little as possible here. The more stuff you add, the more crowded the library path for Delphi will be. Also, try not to add source files to the library path unless really necessary. SmartSetup can work with a lot of libraries, and if they all add their sources to the library path, compilation of projects from the IDE will become really slow.

If you need a path specifically for compiling, but there is no need to add it to the library path (for example, a path to find an include file), you can use the `build-only library paths` for that. Here is a real-world example of that case: [SynEdit](https://github.com/tmssoftware/smartsetup-registry/blob/main/turbopack.synedit/tmsbuild.yaml)

{{/Note}}

### **defines** section

This section works in tandem with the section “defines” in your [tms.config.yaml configuration file](https://github.com/tmssoftware/smartsetup/blob/git-registry/tms/example-config/tms.config.byproduct.yaml).

In the configuration file, the user can write a list of defines for your product to use when compiling. For example, he could write:

```yaml
   defines:
       - RTTI
       - UNICODE
```

To have your product compiled with both RTTI enabled and UNICODE support. In your own code, you would have to have something like this:

```delphi
{$IFDEF UNICODE}
 //Consume unicode library
{$ENDIF}

```

When the user does a `tms build` or `tms install`, SmartSetup will automatically pass the defines RTTI and UNICODE to your library, so it will be compiled as the user expects. But, if you declare a `defines filename` in this section, the defines will also be saved to the file you specify, with the following contents:

```delphi
// This file was generated automatically by TMS Smart Setup.
// Do not modify it, as it will be overwritten every time you rebuild.
// To change the values below, modify tms.config.yaml and re-run tms build.

{$DEFINE RTTI}
{$DEFINE UNICODE}

```

You can then include this file in your sources, with `{$I <DefinesFileName>}. This is not strictly necessary for SmartSetup to work (it passes the defines in the command line anyway), but it will help your users if they want to compile your library manually from the IDE.

{{#Important}}

By default, SmartSetup has disabled the “modify sources” parameter in `tms.config.yaml`. This means that with a default configuration, SmartSetup won’t generate this file. It will only be generated if the user edits its configuration and allows SmartSetup to modify the sources.

{{/Important}}

Besides the `defines filename`, here you can specify which defines your user might specify when compiling your application. Currently, this isn’t used anywhere, but a GUI could read those values and present a list to the users so they can click in a checkbox “RTTI” for your library. It also works as a documentation: users who know might read your `tmsbuild.yaml` file and get a list of available defines they can use from here.

### **registry keys** section

### **help** section

### **links** section

### **file links** section

### **other versions** section

This section is used by tms products to know if a product to install is also installed by an old, not SmartSetup, installer. If the registry key in this section exists, SmartSetup will refuse to run and tell you to uninstall the old product first. You most likely can delete this section for your own products.

### **dependencies** section

Here you define which product your product depends on. While SmartSetup build in parallel, it guarantees that those products will be fully compiled before yours starts compiling, and their library paths will be passed to compile your product.
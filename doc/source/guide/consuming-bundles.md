---

uid: SmartSetup.ConsumingBundles

---

# Consuming SmartSetup bundles

## Understanding servers

SmartSetup 2.0 introduced the concept of **Servers**. A Server is just a place that can provide multiple SmartSetup products for you to install.

It supports two types of servers:

  * Api Servers
  * ZipFile Servers

Each one of those provides two things:
  * A list of products available to install
  * A way to download any of those products into your machine so you can install them.


### ZipFile Servers

ZipFile servers work by having a list of products inside a zip file.

The zip file must contain folders, each one with a tmsbuild.yaml file inside, describing a particular product. 

You can see an example of this kind of server in our "Community" server, which is hosted at https://github.com/tmssoftware/smartsetup-registry

{{#Note}}
While we host the community server at GitHub for convenience so everyone can collaborate, we don't actually use Git to retrieve the list of products. We use this zip file instead: https://github.com/tmssoftware/smartsetup-registry/archive/refs/heads/main.zip which contains the contents of the repository inside.
{{/Note}}

To avoid constantly re downloading the zip file with the products for checking updates, we use [ETags](https://developer.mozilla.org/docs/Web/HTTP/Reference/Headers/ETag), same as we do when downloading a file using the [ZipFile](#zipfile) protocol. GitHub supports ETags by default, so if you host a ZipFile server in GitHub, there is nothing to be done. When hosting it in your own servers, you might have to configure them to support ETags. If they don't, SmartSetup will still work, but print a message telling you to enable ETag support.

Currently, the ZipFile server only uses the following information of the `tmsbuild.yaml` file:
```yaml
application:
  id: tms.example # use a short descriptive name in the form company.product. This id will be used everywhere by tms smart setup to refer to this product.
  name: TMS Example for VCL
  description: An example of a component that doesn't exist, but it would be quite nice if it did. The world needs more examples!.
  url: https://www.tmssoftware.com/
  vcs protocol: git  #might be git, svn or zipfile. If omitted, it will be git.
```

The most important ones are the url to the repository, and the vcs protocol (which defaults to git, but you can change it to be a different one)

The rest of the file is ignored, except when the repository at `url` doesn't contain a `tmsbuild.yaml` file. If the target repository contains a `tmsbuild.yaml` file, that file will be used for building. If it doesn't, then SmartSetup will copy and use the `tmsbuild.yaml` file in the server-zip file. **This allows you to add SmartSetup support for existing repositories without having to add a `tmsbuild.yaml` file to them**. Just adding the file in the ZipFile Server is enough. 


### ZipFile Server Protocols

As mentioned above, a server consists of two parts: A list of products, and a way to download those products to the machine. In the previous section, we saw how a ZipFile server handled the list of products. It is always a compressed file with `tmsbuild.yaml` files inside. Now, to actually download the products in the list, the following protocols are available:
 
#### GIT

When using GIT, the url must be a valid URL for a git clone. SmartSetup will just offload all the work to a Git client in your machine. You need to have Git configured locally, and as we call the same git client that you normally use, it will be configured the same as your does.

There is normally nothing you need to do to use GIT, as SmartSetup will find the Git client in your Windows Path. But if you want to use a different Git client, or configure for example the clone to be for a certain tag, you can edit `tms.config.yaml` and uncomment the lines in the git section that you want:

```yaml
git:
    # Location where to find git.exe. If not specified, smart setup will search for the first git.exe available in the OS PATH.
    git location: c:\git\git.exe

    # Here you can specify extra parameters for the git clone call. If not specified, smart setup will do a plain clone.
    clone command: clone

    # Command for git pull
    pull command: pull

    # Command for a shallow clone that will be used to get a single file.
    shallow clone command: clone --depth 1 --filter=blob:none --sparse

```

#### SVN

This is similar to GIT, but using Subversion instead. Similar to GIT, there is a section in the config file where you can fine-tune it, but it should work out of the box, as long as the svn client is in your Windows Path:

```yaml
svn:
    # Location where to find svn.exe. If not specified, smart setup will search for the first git.exe available in the OS PATH.
    svn location: C:\fpc\fpcbootstrap\svn\bin\svn.exe

    # Here you can specify extra parameters for the svn checkout call. If not specified, smart setup will do a plain checkout.
    checkout command: checkout

    # Command for svn update
    update command: update

    # Command for svn export
    export command: export
```

#### ZIPFILE

ZipFiles are just what the name says, a zip file that contains all of the files. Just point the url to a place where you can download a zip file, and SmartSetup will download it, unzip it, and build it.

Notes:
 * Despite the name, "ZipFiles" can be `.zip` or `.tar.zst` files. Zip files are the most widespread format, so we recommend it whenever files aren't too big. [ZStd](http://www.zstd.net) is a newer format that compresses more, and is faster for the same level of compression than zip. And while not as common as .zip, Windows 11 Explorer fully supports `.tar.zst` files, and also most modern file compression utilities will.
 
 * A drawback of using ZipFiles as the backend is that there isn't an obvious way to know if there is a new version of the component without downloading the file again and again every time we do some type of `tms update`. To fix this, we use [ETags](https://developer.mozilla.org/docs/Web/HTTP/Reference/Headers/ETag). So if you want to use the ZipFile protocol to serve files, make sure that the webserver that has them supports ETags. **GitHub supports ETags by default**, so you can link to any file in the GitHub releases or other parts without issues. For other webservers, you might need to configure them. SmartSetup won't refuse to serve zip files over a server that doesn't support ETags, but it will keep **constantly downloading the zip file** to check for newer versions, and will issue a warning in the logs. 

 * ZipFile files might be hosted in a webserver, or just by the file system. To use the filesystem, just set the url to be in the form `file://path-to-zip-file.zip` instead of `https://url-to-zip-file.zip`

 * The root folder of a ZipFile might have a `tmsbuild.yaml` file, or the file might be stored inside a folder, as long as the path to the file is all empty folders. This is to support hosts like GitHub that provide you a zip where, for example, the root will have just one folder `product` and the files (including tmsbuild.yaml) will be all inside it.
 


### Api Servers

apiservers are also the only ones that download to downloads so they are backed up.

{{#Note}}
Currently Api servers are the only ones that support authentication. For that reason, and to keep the interface simpler, at the moment we only allow a single active Api server at a time. Otherwise you would have to login to each Api server separately. 
{{/Note}}

## Predefined servers

Currently, SmartSetup comes with three predefined servers, with the reserved names:
  1. **local**: A local server that you can use to store your own projects.
  2. **tms**: A api server that is used to install tms products.
  3. **community**: A zipfile server containing open source projects and hosted at https://github.com/tmssoftware/smartsetup-registry. It is disabled by default.

## Adding and removing servers

Besides the predefined servers, you can add your own. This can be useful if you need a private server for your products, or just for testing if you plan to submit a new product to the community server.

To add a server, you can issue the command:
```shell
 tms server-add <name> <protocol> <url> [enable]
```
For example, the command:
```shell
 tms server-add  myserver zipfile file://c:\myzip.zip
```
will add a server named "myserver" as a zipfile in your machine. This zip file should contain a list of `tmsbuild.yaml`files, similar to the ones at https://github.com/tmssoftware/smartsetup-registry/archive/refs/heads/main.zip
Look at [ZipFile Servers](#zipfile-servers) for more information in that file. You could also host the zip file in a web server instead of your file system: The command would be similar but the url would start with `https://` instead of `file://`

You can also list the servers you have with `tms server-remove` and `tms server-list`, but it might be simpler to just edit the file with `tms config` and manually editing the file. So for example, after you added the `myserver` above, `tms.config.yaml` will be:

```yaml
  # Specify which servers SmartSetup will connect to.
  # When creating your own server, you also need to specify "protocol" (can be local, api, github)
  # And the "url" where the server is.
  servers:
    local:
      enabled: true

    tms:
      enabled: true

    community:
      enabled: true

    myserver:
      protocol: zipfile
      url: file://c:\myzip.zip
      enabled: true
```

All the commands above just modify `tms.config.yaml`, so you might edit it instead.

## Enabling and disabling servers

Besides adding and removing servers, you can enable and disable them. So, for example, the community server comes disabled by default. To enable it, you should write:

```shell
tms server-enable community
```
and to disable it again:

```shell
tms server-enable community false
```

{{#Note}}
To avoid adding another command, there is not a `server-disable` command. To disable a server, you call `server-enable` with the `enabled` parameter set to false.
{{/Note}}

As with [Adding and removing servers](#adding-and-removing-servers), you can just edit `tmsconfig.yaml` and change the server status there.
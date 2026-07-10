---
uid: SmartSetup.Command.Credentials
---

# tms credentials

Sets, displays, or removes the credentials used to access a remote repository.

## Synopsis

```shell
tms credentials [<options>] [<global-options>]
```

## Description

Authenticates with a TMS API server and stores the result securely. On Windows, the credentials are kept in the Windows Credential Manager. On other platforms, they are kept in a file in the Smart Setup metadata folder.

How you authenticate depends on how the server is configured, and the command adapts automatically:

- **Registration e-mail and code.** The command prompts for the e-mail address and registration code and saves them. If `-email` or `-code` are provided on the command line, it updates only those values without prompting.
- **Browser sign in.** The command opens your default browser so you can sign in there, and waits for you to finish. If the browser doesn't open automatically, it prints a URL to open manually in a browser **on the same machine**; the sign in can't be completed from a different device. Press `Ctrl-C` to cancel while waiting. Because the sign in is interactive, `-email` and `-code` do not apply and the command reports an error if they are passed.

After a successful browser sign in, commands that access the server work without further interaction: Smart Setup renews the access automatically and only asks you to sign in again if that is no longer possible — for example, after a long time without using it.

Use `-print` to display the current authentication state without changing it, and `-delete` to remove the stored credentials — for a browser sign-in server, this signs you out. After removing, commands that need the server fail until you authenticate again.

By default, the command targets the server named `tms`. Use `-server` to target a different API server.

## Options

| Option               | Description                                                                                                    |
| -------------------- | -------------------------------------------------------------------------------------------------------------- |
| `-email:<email>`     | Sets the registration e-mail without prompting. Only for servers that use e-mail and code.                     |
| `-code:<code>`       | Sets the registration code without prompting. Only for servers that use e-mail and code.                       |
| `-check`             | Validates the credentials against the server before saving them. Only for servers that use e-mail and code.    |
| `-timeout:<seconds>` | For browser sign in, seconds to wait for it to complete. Defaults to 180.                                       |
| `-print`             | Displays the current authentication state instead of changing it.                                              |
| `-delete`            | Removes the stored credentials. For a browser sign-in server, this signs you out.                              |
| `-server:<name>`     | Targets a specific server by name. Defaults to `tms` if omitted.                                               |
| `-json`              | Outputs the result in JSON format.                                                                             |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Authenticates with the default TMS server, prompting for an e-mail and code or opening the browser depending on how the server is configured:

```shell
tms credentials
```

Sets e-mail and code non-interactively (useful in CI environments, for servers that use e-mail and code):

```shell
tms credentials -email:user@example.com -code:XXXX-XXXX-XXXX-XXXX
```

Validates and saves the e-mail and code in one step:

```shell
tms credentials -email:user@example.com -code:XXXX-XXXX-XXXX-XXXX -check
```

Allows more time to complete a browser sign in, for example when it involves multi-factor authentication:

```shell
tms credentials -timeout:600
```

Displays the current authentication state:

```shell
tms credentials -print
```

Removes the stored credentials for the default server (or signs out of a browser sign-in server):

```shell
tms credentials -delete
```

Targets a custom server:

```shell
tms credentials -server:myserver
```

## See Also

- [tms server-list](xref:SmartSetup.Command.ServerList)
- [tms install](xref:SmartSetup.Command.Install)

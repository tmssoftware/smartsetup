---
uid: SmartSetup.Command.Credentials
---

# tms credentials

Sets or displays the credentials used to access a remote repository.

## Synopsis

```shell
tms credentials [<options>] [<global-options>]
```

## Description

Prompts for the e-mail address and registration code used to authenticate with a TMS API server, and saves them securely. On Windows, credentials are stored in the Windows Credential Manager. On other platforms, they are stored in a file in the Smart Setup metadata folder.

If `-email` or `-code` are provided on the command line, the command updates only those values without prompting interactively.

By default, credentials are set for the server named `tms`. Use `-server` to target a different API server.

Use `-print` to display the currently stored credentials without modifying them.

## Options

| Option              | Description                                                                                          |
| ------------------- | ---------------------------------------------------------------------------------------------------- |
| `-email:<email>`    | Sets the registration e-mail without prompting interactively.                                        |
| `-code:<code>`      | Sets the registration code without prompting interactively.                                          |
| `-server:<name>`    | Targets a specific server by name. Defaults to `tms` if omitted.                                    |
| `-check`            | Validates the credentials against the server before saving them.                                     |
| `-print`            | Displays the currently stored credentials instead of updating them.                                  |
| `-json`             | Outputs credentials in JSON format when used together with `-print`.                                 |

## Global Options

See [Global Options](xref:SmartSetup.Command.GlobalOptions) for options available to all commands.

## Examples

Prompts interactively to set credentials for the default TMS server:

```shell
tms credentials
```

Sets credentials non-interactively (useful in CI environments):

```shell
tms credentials -email:user@example.com -code:XXXX-XXXX-XXXX-XXXX
```

Validates and saves credentials in one step:

```shell
tms credentials -email:user@example.com -code:XXXX-XXXX-XXXX-XXXX -check
```

Displays the currently stored credentials:

```shell
tms credentials -print
```

Sets credentials for a custom server:

```shell
tms credentials -server:myserver -email:user@example.com -code:XXXX-XXXX-XXXX-XXXX
```

## See Also

- [tms server-list](xref:SmartSetup.Command.ServerList)
- [tms install](xref:SmartSetup.Command.Install)

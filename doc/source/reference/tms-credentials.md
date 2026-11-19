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

## Environment Variables

Credentials for the `tms` server can also be provided with environment variables instead of being stored. This is useful in automated environments, and required where the Windows Credential Manager is not available.

| Variable           | Description                       |
| ------------------ | --------------------------------- |
| `SMARTSETUP_EMAIL` | The registration e-mail.          |
| `SMARTSETUP_CODE`  | The registration code.            |

When either variable is set:

- Smart Setup reads the credentials for the `tms` server from the environment variables and ignores any stored credentials. Both variables must be set; if one is missing, commands that access the server fail with an error.
- The variables apply only to the `tms` server. Other API servers keep using their stored credentials.
- Credential profiles are ignored.
- Nothing is written to the Credential Manager or to disk. The access token obtained from the server is kept in memory and reused only during the current `tms` process, so each run authenticates again.
- `tms credentials` refuses to run, including with `-print`. Unset the variables to view or modify the stored credentials.

{{#Note}}
On Windows, the Credential Manager is not available in some sessions, such as SSH, remote PowerShell sessions, Windows services, CI agents, or scheduled tasks that run without a stored password. In those sessions, saving credentials fails with the error "A specified logon session does not exist" (code 1312). Use the environment variables instead.
{{/Note}}

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

Sets credentials non-interactively (for CI environments, see also [Environment Variables](#environment-variables)):

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

Provides credentials through environment variables for the current PowerShell session, without storing them:

```shell
$env:SMARTSETUP_EMAIL = "user@example.com"
$env:SMARTSETUP_CODE = "XXXX-XXXX-XXXX-XXXX"
tms install tms.biz.aurelius
```

## See Also

- [Continuous Integration](xref:SmartSetup.ContinuousIntegration)
- [tms server-list](xref:SmartSetup.Command.ServerList)
- [tms install](xref:SmartSetup.Command.Install)

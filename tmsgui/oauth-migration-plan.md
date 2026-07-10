# tmsgui ↔ tms.exe authentication contract

Everything below is implemented (branch `oauth`). This document survives as the list
of contracts between tms.exe and tmsgui: changing any of them on one side silently
breaks the other. tmsgui and tms.exe must always ship together.

## Commands tmsgui runs

| Action | Command | Success output (stdout, pure JSON) |
|---|---|---|
| Browser sign in | `tms credentials -server:tms -timeout:600 -json` | `{"server","status":"signed-in","email"}` |
| Sign out | `tms credentials -server:tms -delete -json` | `{"server","status":"signed-out"}` |
| E-mail/code get/set | `tms credentials -print -json` / `-email:x -code:y -check` | email/code pairs; `-print` on an OIDC server adds `"legacyCredentials": true` when stored e-mail/code exist |
| Sign-in state | `tms info -json` | see `auth status` below |
| Server auth mode | `tms server-list -json` | see `auth_mode` below |

## Contracts

- **`error: oauth2:` substring.** tmsgui greps command output (lowercase) for
  `error: oauth2:` to tell auth failures from other errors. Every CLI auth-failure
  message must start with `oauth2: ` (UMain prefixes `Error: `).

- **Pure JSON on stdout for `-json` commands.** Any extra human-readable line the
  CLI prints during a `-json` command must be registered in tmsgui's strip-list
  (`TTmsRunner.ProcessAlerts`, `src/UTmsRunner.pas`), or JSON parsing breaks.
  Currently stripped: new-version alert, disk-space warning, and the legacy-
  credentials deprecation notice, whose first line must keep starting with
  `You are using stored e-mail/code credentials` (emitted by
  `TCredentialsManager.EffectiveAuthMode` in the `TLegacyCredentialsPolicy.Warn`
  phase; tmsgui shows one GUI-phrased log notice per session instead).

- **`tms info -json` → `"auth status"`** (tms server only; field absent = old CLI):
  - `signed-in` — browser (OIDC) session (refresh token, or non-legacy valid token)
  - `legacy-credentials` — grandfathered e-mail/code, still used to authenticate
  - `credentials` — e-mail/code on a credentials-mode server
  - `none` — nothing usable; commands needing auth will fail
  `"has credentials"` (deprecated) = status is not `none`; it drives the startup
  sign-in prompt, so grandfathered users are not prompted until the policy is Deny.

- **`tms server-list -json` → `"auth_mode"`.** Value `oidc`; field absent =
  credentials mode (or old CLI). The CLI applies the `TMSSETUP_AUTH_MODE`
  env-var override here, so tmsgui automatically follows the support escape
  hatch (e.g. shows the e-mail/code dialog when the override forces credentials).

## Button and flow rules (implemented)

- `TGUIEnvironment.ExecuteRequestCredentials` branches on `UsesOidc('tms')`:
  browser sign-in dialog (`src/Forms.SignIn.pas`, runners `TTmsLoginRunner`/
  `TTmsLogoutRunner` in `src/UTmsRunner.pas`) vs. classic credentials dialog.
- The single Credentials button shows **Sign out** only when
  `TTmsInfo.SignedInViaBrowser` (`auth status = signed-in`). Grandfathered
  users see **Sign in...** — the migration action: a successful browser sign-in
  makes the CLI delete the stored e-mail/code. Sign out (`-delete`) also removes
  grandfathered e-mail/code.

## Migration phases (CLI constants drive everything; no tmsgui changes needed)

`TMSDefaultAuthMode` is already `Oidc`. `TMSLegacyCredentialsPolicy`
(UConfigDefinition.pas) escalates: **Allow** — grandfathered users work silently,
button reads "Sign in..."; **Warn** — same, plus deprecation notice (CLI console /
one tmsgui log entry per session); **Deny** — `auth status` becomes `none`, so
tmsgui prompts browser sign-in at startup. Server-side kill switch afterwards:
`TMSClient.CredentialsAuthEnabled: false` (server repo), rejecting with OAuth error
`credentials_auth_disabled`.

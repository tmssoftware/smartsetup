# Tests that `tms self-update` refuses to overwrite the running binary when
# the new tms.exe in the bundle isn't Authenticode-signed by the same
# publisher as the currently running tms.exe. Without this gate, an attacker
# who controlled a server (or a tampered bundle on its way down) could ship
# a malicious tms.exe — the bundle's `file_hash` is fetched from the same
# server as the bundle, so it is not a trust anchor.
#
# This test only meaningfully exercises the negative path on Windows where
# Authenticode is implemented. The test fixtures are unsigned, so as long
# as we don't pass -test-skip-self-update-signature-verification the gate
# should fire (either because the new tms.exe in the bundle isn't signed,
# or because the current tms.exe isn't signed and we have no publisher to
# pin to).
#
# See Commands.SelfUpdate.Verify.pas and TMSSystem.Signatures.pas.

. test.setup

if ($IsLinux -or $IsMacOS) {
    Write-Host "Skipping self-update signature test on non-Windows: signature verification is Windows-only for now."
    return
}

$tmsexe = Get-Alias tms

# Copy tms.exe to the work folder so a successful self-update wouldn't
# clobber the binary the test runner is using.
Copy-Item $tmsexe.Definition "./tms.exe" -Force

# Tamper with our copy so it still loads and runs but its Authenticode
# signature no longer validates. Appending bytes to the end of the PE file
# puts them in the overlay, which the Windows loader ignores when running the
# image -- but those bytes fall outside the range covered by the signed
# digest, so WinVerifyTrust now fails with a bad-digest error. This makes the
# test exercise the real "signature present but tampered" path instead of only
# "no signature at all", so the gate is still verified even when the fixtures
# happen to be signed.
$tmsexePath = (Resolve-Path "./tms.exe").ProviderPath
$tampered = [System.IO.File]::ReadAllBytes($tmsexePath)
$tampered += [byte]0
[System.IO.File]::WriteAllBytes($tmsexePath, $tampered)

# Sanity check: the tampered binary must still run.
$null = ./tms.exe version
if ($LASTEXITCODE -ne 0) {
    throw "Tampered tms.exe failed to run (exit $LASTEXITCODE); the test fixture is invalid."
}

# Verify the binary on disk wasn't modified.
$beforeUpdateTime = (Get-Item "./tms.exe").LastWriteTime

# --- Without the skip flag, self-update must abort. ---
Test-CommandFails {
    ./tms.exe self-update -test-force-self-update
} "*Self-update aborted*" "self-update without signature skip flag must abort on unsigned bundle"

$afterUpdateTime = (Get-Item "./tms.exe").LastWriteTime
if ($afterUpdateTime -ne $beforeUpdateTime) {
    throw "tms.exe was modified despite the signature gate firing. Before: $beforeUpdateTime, After: $afterUpdateTime"
}

# --- With the skip flag (test-only), self-update proceeds. This mirrors
# the existing tests/commands/self-update path and confirms the gate is
# the only thing blocking us. ---
$log = ./tms.exe self-update -test-force-self-update -test-skip-self-update-signature-verification
Test-Result -CommandResult $log -Message "*TMS Smart Setup has been updated from version*"

$afterUpdateTime = (Get-Item "./tms.exe").LastWriteTime
if ($afterUpdateTime -eq $beforeUpdateTime) {
    throw "tms.exe was not modified despite the signature gate being skipped. Before: $beforeUpdateTime, After: $afterUpdateTime"
}

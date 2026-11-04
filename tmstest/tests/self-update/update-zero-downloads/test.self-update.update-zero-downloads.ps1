# Self-update must work even if versions to keep is set to 0.

. test.setup

tms config-write -p:"tms smart setup options:versions to keep=0"

$tmsexe = Get-Alias tms
Copy-Item $tmsexe.Definition "./tms.exe" -Force
$hashBefore = (Get-FileHash "./tms.exe").Hash

$result = ./tms.exe self-update -test-force-self-update -test-skip-self-update-signature-verification

$hashAfter = (Get-FileHash "./tms.exe").Hash
if ($hashBefore -eq $hashAfter) {
    throw "tms.exe should have been updated, but its hash didn't change."
}

if (-not (Test-Path "./tmsgui.exe")) {
    throw "tmsgui.exe should exist after self-update, but it doesn't."
}
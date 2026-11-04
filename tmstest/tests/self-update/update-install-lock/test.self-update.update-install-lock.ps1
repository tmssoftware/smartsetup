# Check how INSTALLING.LOCK is handled: a broken install is detected and repaired by self-update,
# a corrupt hashes file is reported, and a stale lock on a correct install doesn't cause a downgrade.
. test.setup

# Bundle with the same version as the running tms, so it is only installed when forced or when the lock exists.
$v = [version]((tms info -json | ConvertFrom-Json -AsHashtable)["tms version"])
$CurrentVersion = '{0}.{1}.{2}.{3}' -f $v.Major, ($v.Minor -lt 0 ? 0 : $v.Minor), ($v.Build -lt 0 ? 0 : $v.Build), ($v.Revision -lt 0 ? 0 : $v.Revision)

New-AutoUpdateBundle $CurrentVersion -AddHashes $false
$hashTmsNew = (Get-FileHash "./fake-auto-update-bundle/tms.exe").Hash
$hashGuiNew = (Get-FileHash "./fake-auto-update-bundle/tmsgui.exe").Hash

./tms.exe self-update -test-force-self-update -test-skip-self-update-signature-verification -nofetch

if ($hashTmsNew -ne (Get-FileHash "./tms.exe").Hash) {
    throw "tms.exe should have been updated, but its hash didn't change."
}
if (-not (Test-Path "./tms.smartsetup.hashes.txt")) {
    throw "tms.smartsetup.hashes.txt should exist after self-update, but it doesn't."
}
if (Test-Path "./INSTALLING.LOCK") {
    throw "INSTALLING.LOCK should have been removed after a successful self-update."
}

# 1. Broken install: other commands fail, self-update reinstalls even if the version is not newer.
[System.IO.File]::AppendAllText((Join-Path $PWD "tmsgui.exe"), "corrupt")
Set-Content -Path "./INSTALLING.LOCK" -Value "lock"

$result = Invoke-WithExitCodeIgnored{ ./tms.exe fetch tms.biz.aurelius }
if (-not (($result -join "`n") -like "*Error: Fatal: This smartsetup update didn't complete correctly. Please run tms self-update to fix it*")) {
    throw "Expected the fetch to fail because the install is broken, but it didn't. Output: $result"
}

$result = ./tms.exe self-update -test-skip-self-update-signature-verification -nofetch
if (-not (($result -join "`n") -like "*has been repaired*")) {
    throw "Expected self-update to reinstall the broken install, but it didn't. Output: $result"
}
if ($hashGuiNew -ne (Get-FileHash "./tmsgui.exe").Hash) {
    throw "tmsgui.exe should have been repaired, but its hash is wrong."
}
if (Test-Path "./INSTALLING.LOCK") {
    throw "INSTALLING.LOCK should have been removed after repairing the install."
}

# 2. Corrupt hashes file.
Copy-Item "./tms.smartsetup.hashes.txt" "./hashes.backup.txt" -Force
Add-Content -Path "./tms.smartsetup.hashes.txt" -Value "this line is invalid"
Set-Content -Path "./INSTALLING.LOCK" -Value "lock"

$result = Invoke-WithExitCodeIgnored{ ./tms.exe fetch tms.biz.aurelius }
if (-not (($result -join "`n") -like "*is corrupt. Invalid line: ""this line is invalid""*")) {
    throw "Expected the fetch to report a corrupt hashes file, but it didn't. Output: $result"
}

Copy-Item "./hashes.backup.txt" "./tms.smartsetup.hashes.txt" -Force
Remove-Item "./hashes.backup.txt"

# 3. Stale lock on a correct install (e.g. the user reinstalled manually): self-update must not downgrade.
# Only an older bundle is available. New-AutoUpdateBundle overwrites tms.exe and tmsgui.exe, so keep the installed ones.
New-Item -ItemType Directory -Force "./installed-backup" | Out-Null
Copy-Item "./tms.exe", "./tmsgui.exe" "./installed-backup" -Force
Remove-Item "./Downloads/CurrentVersions/*.zip"
New-AutoUpdateBundle "1.0.0.0"
Copy-Item "./installed-backup/tms.exe", "./installed-backup/tmsgui.exe" "./" -Force
Set-Content -Path "./INSTALLING.LOCK" -Value "lock"

$result = ./tms.exe self-update -test-skip-self-update-signature-verification -nofetch
if (-not (($result -join "`n") -like "*You are using the latest version*")) {
    throw "Expected self-update to do nothing with a stale lock, but it didn't. Output: $result"
}
if ($hashTmsNew -ne (Get-FileHash "./tms.exe").Hash) {
    throw "tms.exe was downgraded by a stale lock."
}
if ($hashGuiNew -ne (Get-FileHash "./tmsgui.exe").Hash) {
    throw "tmsgui.exe was downgraded by a stale lock."
}
if (Test-Path "./INSTALLING.LOCK") {
    throw "A stale INSTALLING.LOCK should have been removed when the install is correct."
}

./tms.exe fetch tms.biz.aurelius

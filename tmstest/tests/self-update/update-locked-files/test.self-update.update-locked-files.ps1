#check we are as atomic as possible. Either we fully update, or we don't at all.
. test.setup


New-AutoUpdateBundle

$hashTmsOld = (Get-FileHash "./tms.exe").Hash
$hashGuiOld = (Get-FileHash "./tmsgui.exe").Hash
$hashTmsNew = (Get-FileHash "./fake-auto-update-bundle/tms.exe").Hash
$hashGuiNew = (Get-FileHash "./fake-auto-update-bundle/tmsgui.exe").Hash

#fully lock tmsgui. as we will first update tms.exe, this could end up with a new tms.exe and an old tmsgui.exe
$tmsguiPath = Join-Path -Path $PWD -ChildPath "tmsgui.exe"
$fileStream = [System.IO.File]::Open($tmsguiPath, [System.IO.FileMode]::Open, [System.IO.FileAccess]::ReadWrite, [System.IO.FileShare]::None)

$result = Invoke-WithExitCodeIgnored{ ./tms.exe self-update -test-force-self-update -test-skip-self-update-signature-verification -nofetch}

$fileStream.Close();
$fileStream.Dispose();

if (-not ($result -like "*The process cannot access the file because it is being used by another process*"))
{
    throw "Expected the self-update to fail with a file lock error, but it didn't. Output: $result"
}

$hashtmsAfter = (Get-FileHash "./tms.exe").Hash
if ($hashTmsNew -ne $hashtmsAfter) {
    throw "tms.exe should have been updated, but its hash didn't change."
}

$hashGuiAfter = (Get-FileHash "./tmsgui.exe").Hash
if ($hashGuiOld -ne $hashGuiAfter) {
    throw "tmsgui.exe should not have been updated, but its hash changed."
}


# check that in the next run, we realize the update is not complete.
$result = Invoke-WithExitCodeIgnored{ ./tms.exe fetch tms.biz.aurelius }

if (-not (($result -join "`n") -like "*Error: Fatal: This smartsetup update didn't complete correctly. Please run tms self-update to fix it*")) {
    throw "Expected the fetch to complete the incomplete self-update, but it didn't. Output: $result"
}

#should update without force, we are in a non stable state.
$result = ./tms.exe self-update -test-skip-self-update-signature-verification -nofetch
$hashtmsAfter = (Get-FileHash "./tms.exe").Hash
if ($hashTmsNew -ne $hashtmsAfter) {
    throw "tms.exe should have been updated, but its hash didn't change."
}

$hashGuiAfter = (Get-FileHash "./tmsgui.exe").Hash
if ($hashGuiNew -ne $hashGuiAfter) {
    throw "tmsgui.exe should have been updated, but its hash didn't change."
}


Restore-AutoupdateBundle
./tms.exe self-update -test-force-self-update -test-skip-self-update-signature-verification -nofetch

$hashtmsAfter = (Get-FileHash "./tms.exe").Hash
if ($hashTmsNew -ne $hashtmsAfter) {
    throw "tms.exe should have been updated (it already was), but it's not."
}
$hashGuiAfter = (Get-FileHash "./tmsgui.exe").Hash
if ($hashGuiNew -ne $hashGuiAfter) {
    throw "tmsgui.exe should have been updated, but its hash didn't change."
}

# read-only files not block the self-update.
Restore-AutoupdateBundle

Set-ItemProperty -Path "./tms.exe" -Name IsReadOnly -Value $true
Set-ItemProperty -Path "./tmsgui.exe" -Name IsReadOnly -Value $true

Invoke-WithExitCodeIgnored{ ./tms.exe self-update -test-force-self-update -test-skip-self-update-signature-verification -nofetch }

$hashtmsAfter = (Get-FileHash "./tms.exe").Hash
if ($hashTmsOld -ne $hashtmsAfter) {
    throw "Read-only tms.exe should have not been updated, but its hash changed."
}
$hashGuiAfter = (Get-FileHash "./tmsgui.exe").Hash
if ($hashGuiOld -ne $hashGuiAfter) {
    throw "Read-only tmsgui.exe should have not been updated, but its hash changed."
}

Set-ItemProperty -Path "./tms.exe" -Name IsReadOnly -Value $false
Set-ItemProperty -Path "./tmsgui.exe" -Name IsReadOnly -Value $false




# Test if we can self-update, even if API servers are disabled.


. test.setup

#tms server-enable tms false
tms server-enable community false

$tmsexe = Get-Alias tms

#Copy tms.exe from its location to the work folder so we don't overwrite the one we are testing.
Copy-Item $tmsexe.Definition "./tms.exe" -Force

#get the time of the copied file, so we can check if it was updated.
$beforeUpdateTime = (Get-Item "./tms.exe").LastWriteTime
$log = ./tms.exe self-update -test-force-self-update
if ($log -like "*TMS Smart Setup has been updated from version*") {
    Write-Host "Self-update reported a successful update."
}
else {
    throw "Unexpected output from self-update: $log"
}
$afterUpdateTime = (Get-Item "./tms.exe").LastWriteTime
if ($afterUpdateTime -le $beforeUpdateTime) {
    throw "tms.exe was not updated. Before: $beforeUpdateTime, After: $afterUpdateTime"
}
else {
    Write-Host "tms.exe was successfully updated. Before: $beforeUpdateTime, After: $afterUpdateTime"   
}
#check that we have a tmsgui.exe in the same folder, and that it was also updated.
if (Test-Path "./tmsgui.exe") {
}
else {
    throw "tmsgui.exe was not found after self-update."
}

$log = ./tms.exe self-update
if ($log -like "*You are using the latest version of TMS Smart Setup*") {
    Write-Host "No updates available, as expected."
}
else {
    throw "Unexpected output from self-update: $log"
}

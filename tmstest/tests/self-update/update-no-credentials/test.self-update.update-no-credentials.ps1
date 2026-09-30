#Check that we can self-update even if we never entered credentials, and even if the API servers are disabled. 

. test.setup

tms server-enable tms false
$tmsexe = Get-Alias tms
Copy-Item $tmsexe.Definition "./tms.exe" -Force

$log = ./tms.exe self-update -test-force-self-update -test-no-credentials -test-skip-self-update-signature-verification
if ($log -like "*TMS Smart Setup has been updated from version*") {
    Write-Host "Self-update reported a successful update."
}
else {
    throw "Unexpected output from self-update: $log"
}

$LogFile = tms log-view -print -text
$LogFileContent = Get-Content -Path $LogFile -Raw
if ($LogFileContent -like "*Can't get update from API server: Credentials not provided. *") {
    Write-Host "Self-update log contains the expected message about not finding the Api server."
}
else {
    throw "Unexpected content in self-update log: $LogFile"
}

if ($LogFileContent -like "*' smartsetup.zip is up to date.'*") {
    throw "File should not be up to date. Unexpected content in self-update log: $LogFile"
}
else {
    Write-Host "Self-update correct."
}


#Now the tms.exe in the disk must be the one from the server, which isn't compiled in Debug mode.
Test-CommandFails {./tms.exe self-update -test-force-self-update} "Unknown command line option : -test-force-self-update"

Copy-Item $tmsexe.Definition "./tms.exe" -Force

$log = ./tms.exe self-update -test-force-self-update -test-no-credentials -test-skip-self-update-signature-verification
if ($log -like "*TMS Smart Setup has been updated from version*") {
    Write-Host "Self-update reported a successful update."
}
else {
    throw "Unexpected output from self-update: $log"
}

$LogFile = tms log-view -print -text
$LogFileContent = Get-Content -Path $LogFile -Raw
if ($LogFileContent -like "*smartsetup.zip is up to date.*") {
    Write-Host "Self-update log contains the expected message about not finding the Api server."
}
else {
    throw "Unexpected content in self-update log: $LogFile"
}

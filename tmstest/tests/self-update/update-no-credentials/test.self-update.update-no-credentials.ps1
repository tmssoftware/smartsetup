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
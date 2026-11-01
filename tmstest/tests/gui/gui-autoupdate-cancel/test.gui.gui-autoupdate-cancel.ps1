#Check what happens if there are no credentials.
. test.setup

$tmsguiexe = Get-Alias tmsgui
Copy-Item $tmsguiexe.Definition "./tmsgui.exe" -Force
$tmsexe = Get-Alias tms
Copy-Item $tmsexe.Definition "./tms.exe" -Force

Test-tmsgui -tmsguiexe "./tmsgui.exe" -suffixParam "basic" -ExtraArgs "-test-tms:`"-test-force-self-update -test-alert-new-versions -test-skip-self-update-signature-verification`""


#Check what happens if there are no credentials.
. test.setup

$tmsguiexe = Get-Alias tmsgui
Copy-Item $tmsguiexe.Definition "./tmsgui.exe" -Force
$tmsexe = Get-Alias tms
Copy-Item $tmsexe.Definition "./tms.exe" -Force

Test-tmsgui -tmsguiexe "./tmsgui.exe" -suffixParam "basic" -ExtraArgs "-test-tms:`"-test-force-self-update -test-alert-new-versions -test-skip-self-update-signature-verification`""

$tmsguiFullPath = (Resolve-Path "./tmsgui.exe").Path
$tmsFullPath = (Resolve-Path "./tms.exe").Path

# The update relaunches tmsgui.exe. Wait for the new instance, then kill it.
$newGui = $null
$deadline = (Get-Date).AddSeconds(60)
while ((Get-Date) -lt $deadline) {
    $newGui = Get-Process -Name "tmsgui" -ErrorAction SilentlyContinue | Where-Object { $_.Path -eq $tmsguiFullPath }
    if ($newGui) { break }
    Start-Sleep -Milliseconds 500
}
if (-not $newGui) {
    throw "The updated tmsgui.exe was not relaunched after the self-update."
}
$newGui | Stop-Process -Force
$newGui | Wait-Process -Timeout 30 -ErrorAction SilentlyContinue

# Kill any tms.exe the new instance might have started, so it doesn't keep the folder locked.
Get-Process -Name "tms" -ErrorAction SilentlyContinue | Where-Object { $_.Path -eq $tmsFullPath } | Stop-Process -Force

if ((Get-FileHash $tmsguiFullPath).Hash -eq (Get-FileHash $tmsguiexe.Definition).Hash) {
    throw "tmsgui.exe was not updated."
}
if ((Get-FileHash $tmsFullPath).Hash -eq (Get-FileHash $tmsexe.Definition).Hash) {
    throw "tms.exe was not updated."
}

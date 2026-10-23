#Check a simple install of community and api products
. test.setup

$tmsguiexe = Get-Alias tmsgui
Copy-Item $tmsguiexe.Definition "./tmsgui.exe" -Force
Test-tmsgui -tmsguiexe "./tmsgui.exe"

<# The code here will end up showing the message, but it will popup dialog boxes that we can't test.
New-Item  -Name "tms.exe" -ItemType File -Value "not really an exe"
Test-tmsgui -tmsguiexe "./tmsgui.exe"
#>
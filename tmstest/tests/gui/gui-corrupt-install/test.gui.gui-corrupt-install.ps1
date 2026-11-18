# check tmsgui can handle a corrupt install

. test.setup

New-AutoUpdateBundle

Set-ItemProperty -Path "./tmsgui.exe" -Name IsReadOnly -Value $true

Invoke-WithExitCodeIgnored{ ./tms.exe self-update -test-force-self-update -test-skip-self-update-signature-verification -nofetch }

Test-tmsgui -tmsguiexe "./tmsgui.exe"

Set-ItemProperty -Path "./tmsgui.exe" -Name IsReadOnly -Value $false



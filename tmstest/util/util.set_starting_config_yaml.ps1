$BaseDelphiVersion = "37.0" #Delphi 13. Update someday for Delphi 14.

$BDS_ROOT_DIR = Get-ItemProperty -Path "HKCU:\Software\Embarcadero\BDS\$($BaseDelphiVersion)"

$Env:TMS_RSVARS = "$($BDS_ROOT_DIR.RootDir)\bin\rsvars.bat"

set-alias msbuild "$tmsTestRootDir\util\build.bat"


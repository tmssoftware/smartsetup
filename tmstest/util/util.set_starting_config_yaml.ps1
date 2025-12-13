$BaseDelphiVersion = "37.0" #Delphi 13. Update someday for Delphi 14.
$PreviousDelphiVersionId = "delphi12"


$BDS_ROOT_DIR = Get-ItemProperty -Path "HKCU:\Software\Embarcadero\BDS\$($BaseDelphiVersion)"

$Env:TMS_RSVARS = "$($BDS_ROOT_DIR.RootDir)\bin\rsvars.bat"
$Env:TMS_BDS = "$($BDS_ROOT_DIR.RootDir)\bin\bds.exe"

set-alias msbuild "$tmsTestRootDir\util\build.bat"


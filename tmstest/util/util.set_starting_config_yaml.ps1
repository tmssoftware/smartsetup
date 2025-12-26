$Global:BaseDelphiVersion = "37.0" #Delphi 13. Update someday for Delphi 14.
$Global:PreviousDelphiVersionId = "delphi12"

$Global:AllDelphiVersionIDs = @(
    "delphi12",
    "delphi13"
)
$Global:AllDelphiVersions = @(
    "23.0",
    "37.0"
)


$Global:BDS_ROOT_DIR = Get-ItemProperty -Path "HKCU:\Software\Embarcadero\BDS\$($BaseDelphiVersion)"

$Env:TMS_RSVARS = "$($BDS_ROOT_DIR.RootDir)\bin\rsvars.bat"
$Env:TMS_BDS = "$($BDS_ROOT_DIR.RootDir)\bin\bds.exe"

set-alias msbuild "$tmsTestRootDir\util\build.bat"

$Global:ALL_BDS_ROOT_DIRS = @{}
foreach ($ver in $Global:AllDelphiVersions) {
    $bdsDir = Get-ItemProperty -Path "HKCU:\Software\Embarcadero\BDS\$ver"
    $Global:ALL_BDS_ROOT_DIRS[$ver] = $bdsDir.RootDir
}

$BaseDelphiVersion = "23.0" #Delphi 12. Update someday for Delphi 13.

$Env:BDS_ROOT_DIR = Get-ItemProperty -Path "HKCU:\Software\Embarcadero\BDS\$($BaseDelphiVersion)"


$Env:TMS_STARTING_CONFIGURATION ="$tmsTestRootDir\util\tms.starting.config.yaml"

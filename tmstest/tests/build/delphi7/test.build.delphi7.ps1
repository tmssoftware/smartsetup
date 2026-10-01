# Check that we can build and install in delphi7

. test.setup
#$registry = "tmstest\delphi7"
#Set-AlternateRegistryKey -RegKey $registry -RegisterAllDelphiVersions $true

tms config-write -p:configuration-for-all-products:replace-delphi-versions=[delphi7] -p:configuration-for-all-products:platforms=[win32intel,win64intel] -p:configuration-for-all-products:options:skip-register=false

tms install tms.vcl.uipack:13.6.9.2  #keep it frozen in time, because newer uipack versions might not support delphi7.

#check the file d7app\Win32\Release\d7.exe was generated
if (-not (Test-Path "d7app\Win32\Release\d7.exe")) {
    throw "d7.exe was not generated"
}

tms uninstall *

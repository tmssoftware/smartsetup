# Check the dpr/res/dfm copy. Similar to packages-from-dpk, but using simple folders, not "+" ones
# Simple folders are copied differently, to Platform\Config, not CompilerVersion\Platform\Config
# We need to copy the dpr/res/dfm files to the correct folder in both cases, when using + and when not.

. test.setup

$registry = "tmstest\packages-from-dpk"
Set-AlternateRegistryKey -RegKey $registry -RegisterAllDelphiVersions $true

tms config-write -p:configuration-for-all-products:replace-delphi-versions=[delphi12,delphi13] -p:configuration-for-all-products:platforms=[win32intel,win64intel] -p:configuration-for-all-products:options:skip-register=false

tms build

$checkFiles = @(
    "dcr\rectangles.dcr",
    "FRes.dfm",
    "triangles.dcr"
)

foreach ($delphi in @("d12", "d13")) {
     Write-Host "Checking files for platform '$delphi'." -ForegroundColor Green
     foreach ($file in $checkFiles) {
        if (-Not (Test-Path "testpkg\resourceful\pkg-source\$delphi\Win32\Release\$file")) {
            throw "The file '$file' does not exist."
        }
    }
}

$originalLocation = Get-Location
Set-Location testapp

bds -ProjectFile "testapp.dproj"  -RegistryKey $registry

.\Win32\Debug\testapp.exe | Assert-ValueIs "616/616"


Set-Location $originalLocation
tms build -unregister
# Checks that we install in WinArm64EC. pyscripter.toml.delphi generates the
# packages on the fly, so we test that too

. test.setup
    
tms config-write -p:configuration-for-all-products:replace-delphi-versions=[delphi12,delphi13] -p:configuration-for-all-products:platforms=[win64intel,winarm64ec] 

tms server-enable community
tms install pyscripter.toml.delphi tms.flexcel.vcl sglienke.spring4d

# check the generated packages at "Products\pyscripter.toml.delphi\src\packages\d11+\TomlDelphi.dproj"
#contain the string "WinArm64EC" in the platform list. If it doesn't, then the package won't be generated and the test will fail when it tries to build it.
$packagePath =  ".\Products\pyscripter.toml.delphi\src\packages\d11+\TomlDelphi.dproj"
$packageContent = Get-Content $packagePath -Raw
if ($packageContent -notmatch "WinArm64EC") {
    Write-Error "Test failed: The generated package does not contain WinArm64EC in the platform list."
}

#check that the file ".\Products\pyscripter.toml.delphi\src\packages\d11+\37.0\WinARM64EC\Release\TomlDelphi.dcp" exists.
$dcpPath = ".\Products\pyscripter.toml.delphi\src\packages\d11+\37.0\WinARM64EC\Release\TomlDelphi.dcp"
if (-Not (Test-Path $dcpPath)) {
    Write-Error "Test failed: The file TomlDelphi.dcp for WinArm64EC was not generated."
}

$dcpPath = ".\Products\pyscripter.toml.delphi\src\packages\d11+\23.0\WinARM64EC\Release\TomlDelphi.dcp"
if (Test-Path $dcpPath) {
    Write-Error "Error: The file TomlDelphi.dcp for WinArm64EC in d12 shouldn't exist."
}

$dcpPath = ".\Products\tms.flexcel.vcl\src\packages\d12+\23.0\WinARM64EC\Release\FlexCel_Core.dcp"
if (-Not (Test-Path $dcpPath)) {
    Write-Error "Test failed: The file FlexCel_Core.dcp for WinArm64EC was not generated."
}

$dcpPath = ".\Products\tms.flexcel.vcl\src\packages\d12+\23.0\WinARM64EC\Release\FlexCel_Core.dcp"
if (Test-Path $dcpPath) {
    Write-Error "Error: The file FlexCel_Core.dcp for WinArm64EC in d12 shouldn't exist."
}

$dcpPath = ".\Products\sglienke.spring4d\src\Packages\Delphi13\WinArm64EC\Release\Spring.Base.dcp"
if (-Not (Test-Path $dcpPath)) {
    Write-Error "Test failed: The file Spring.Base.dcp for WinArm64EC was not generated."
}

$dcpPath = ".\Products\sglienke.spring4d\src\Packages\Delphi12\WinArm64EC\Release\Spring.Base.dcp"
if (Test-Path $dcpPath) {
    Write-Error "Error: The file Spring.Base.dcp for WinArm64EC in d12 shouldn't exist."
}

uninstall_and_check(0)

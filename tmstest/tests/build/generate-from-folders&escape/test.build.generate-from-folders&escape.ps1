# Check the generate from gets the right files when they are in different folders.

. test.setup

tms build

$result = & ".\app\$($Global:BaseDelphiVersion)\Win32\Release\generate_fromtest.exe"
if (($result -join ' ') -ne 'init1 init2 init3 init4 init5 Work!, Rest! / Work2!, Rest2!') {
    throw "Error The app returned '$result' instead of the expected value."
}

$dpkFile = ".\pkg\packages\d11+\Escaped_Core.dpk"
$dpkContent = Get-Content -Path ($dpkFile)
    if (-not (Test-Result -CommandResult $dpkContent -Message "*vcl,*")){
        throw "Expected dpk to contain 'vcl', but it did not. dpk content: $dpkContent"
    }
    if (-not (Test-Result -CommandResult $dpkContent -Message "*rtl,*")){
        throw "Expected dpk to contain 'rtl', but it did not. dpk content: $dpkContent"
    }
    if (-not (Test-Result -CommandResult $dpkContent -Message "*dbrtl;*")){
        throw "Expected dpk to contain 'dbrtl', but it did not. dpk content: $dpkContent"
    }

$dprojFile = ".\pkg\packages\d11+\Escaped_Core.dproj"
$dprojContent = Get-Content -Path ($dprojFile)
    if (-not (Test-Result -CommandResult $dprojContent -Message '*<DCCReference Include="vcl.dcp"/>*')){
        throw "Expected dproj to contain 'vcl', but it did not. dproj content: $dprojContent"
    }
    if (-not (Test-Result -CommandResult $dprojContent -Message '*<DCCReference Include="rtl.dcp"/>*')){
        throw "Expected dproj to contain 'rtl', but it did not. dproj content: $dprojContent"
    }
    if (-not (Test-Result -CommandResult $dprojContent -Message '*<DCCReference Include="dbrtl.dcp"/>*')){
        throw "Expected dproj to contain 'dbrtl', but it did not. dproj content: $dprojContent"
    }

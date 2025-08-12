function Invoke-TmsTest {
    if (Test-Path "$tmsTestRootDir") {
        Set-Location "$tmsTestRootDir"
    } else {
        Write-Error "TMS test directory not found: $tmsTestRootDir"
    }
    & "$tmsTestRootDir\tmstest.ps1"
}

set-alias test.setup "$tmsTestRootDir\util\util.setup.ps1"
set-alias tmstest Invoke-TmsTest


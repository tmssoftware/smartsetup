# When you install product A that depends on product B,
# and you specify a version for product B, it could try to install both the version you specified for B
# and the latest version of B as a dependency of A, which would cause a conflict.
# This test ensures that it does not fail in such case.

function Test-ProductVersions {
    param(
        [hashtable]$Products,
        [string]$ExpectedAVersion,
        [string]$ExpectedBVersion,
        [string]$ExpectedCVersion
    )
    
    if ($Products["tmstest.a"].version -ne $ExpectedAVersion) {
        throw "tmstest.a should be version $ExpectedAVersion, but it is $($Products["tmstest.a"].version)."
    }
    if ($Products["tmstest.b"].version -ne $ExpectedBVersion) {
        throw "tmstest.b should be version $ExpectedBVersion, but it is $($Products["tmstest.b"].version)."
    }
    if ($Products["tmstest.c"].version -ne $ExpectedCVersion) {
        throw "tmstest.c should be version $ExpectedCVersion, but it is $($Products["tmstest.c"].version)."
    }
}

. test.setup
tms server-enable tms false
tms server-add testserver zipfile "file:///$($tmsTestRootDir.Replace('\', '/'))/tmp-run/test-repos/tmsbuild_test_repos.zip"

$installResult = tms install tmstest.a
Test-BuildResultCounts -BuildResult $installResult -expectedNotModifiedCount 0 -expectedIgnoreCount 0 -expectedOkCount 6

tms uninstall *

$installResult = tms install tmstest.a:v1.0.0 tmstest.b:v1.0.0
Test-BuildResultCounts -BuildResult $installResult -expectedNotModifiedCount 0 -expectedIgnoreCount 0 -expectedOkCount 6

$products = tms list -detailed -json | ConvertFrom-Json -AsHashtable
Test-ProductVersions -Products $products -ExpectedAVersion "v1.0.0" -ExpectedBVersion "v1.0.0" -ExpectedCVersion "0.0.0.0"

$installResult = tms update tmstest.a:v1.1.0
$products = tms list -detailed -json | ConvertFrom-Json -AsHashtable
Test-ProductVersions -Products $products -ExpectedAVersion "v1.1.0" -ExpectedBVersion "0.0.0.0" -ExpectedCVersion "0.0.0.0"

$installResult = Invoke-WithExitCodeIgnored{tms update tmstest.x:v1.0.0}
$testOk = Test-Result -CommandResult $installResult -Message "*Error: Could not find any products matching tmstest.x*"
if (-not ($testOk)) {
    Write-Error "The error message should mention 'Error: Could not find any products matching tmstest.x'. Actual message: $($installResult)"
}


#Check cross-reference with community server
$installResult = Invoke-WithExitCodeIgnored{tms install tmstest.x:v1.0.0}
$testOk = Test-Result -CommandResult $installResult -Message "*requires the product ""Spring4D"" to build, and it isn't present*"
if (-not ($testOk)) {
    Write-Error "The error message should mention 'requires the product ""Spring4D"" to build, and it isn't present'. Actual message: $($installResult)"
}


tms server-enable community
$installResult = tms install tmstest.x:v1.0.0
$products = tms list -detailed -json | ConvertFrom-Json -AsHashtable
Test-ProductVersions -Products $products -ExpectedAVersion "v1.1.0" -ExpectedBVersion "0.0.0.0" -ExpectedCVersion "0.0.0.0"
if ($Products["tmstest.x"].version -ne "v1.0.0") {
        throw "tmstest.x should be version v1.0.0, but it is $($Products["tmstest.x"].version)."
    }


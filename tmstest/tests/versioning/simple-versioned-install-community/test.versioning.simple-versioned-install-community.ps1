# Test versioning support in the community server.

function Test-ProductVersions {
    param (
        [Parameter(Mandatory)]
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

tms install tmstest.a:v1.0.0 tmstest.b:v1.1.0
Test-ProductVersions -Products (tms list -detailed -json | ConvertFrom-Json -AsHashtable) -ExpectedAVersion "v1.0.0" -ExpectedBVersion "v1.1.0" -ExpectedCVersion "v1.1.0"

tms update tmstest.a:v1.1.0
Test-ProductVersions -Products (tms list -detailed -json | ConvertFrom-Json -AsHashtable) -ExpectedAVersion "v1.1.0" -ExpectedBVersion "v1.1.0" -ExpectedCVersion "v1.1.0"

tms update tmstest.b:v1.0.0
Test-ProductVersions -Products (tms list -detailed -json | ConvertFrom-Json -AsHashtable) -ExpectedAVersion "v1.1.0" -ExpectedBVersion "v1.0.0" -ExpectedCVersion "v1.1.0"

tms install tmstest.b
Test-ProductVersions -Products (tms list -detailed -json | ConvertFrom-Json -AsHashtable) -ExpectedAVersion "v1.1.0" -ExpectedBVersion "v1.1.0" -ExpectedCVersion "v1.1.0"

tms update tmstest.b:v1.0.0
Test-ProductVersions -Products (tms list -detailed -json | ConvertFrom-Json -AsHashtable) -ExpectedAVersion "v1.1.0" -ExpectedBVersion "v1.0.0" -ExpectedCVersion "v1.1.0"

tms install tmstest.b
Test-ProductVersions -Products (tms list -detailed -json | ConvertFrom-Json -AsHashtable) -ExpectedAVersion "v1.1.0" -ExpectedBVersion "v1.1.0" -ExpectedCVersion "v1.1.0"
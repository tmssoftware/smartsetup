# Check that we can unregister and re-register products without rebuilding them.

$regkey = "tmstest\unregistering"

enum ResultStatus {
    Ok
    Registered
    Unregistered
    NotModifiedRegistered
    NotModifiedUnregistered
}

$ResultStatusNameMap = @{
    [ResultStatus]::Ok = "OK."
    [ResultStatus]::Registered = "REGISTERED."
    [ResultStatus]::Unregistered = "UNREGISTERED."
    [ResultStatus]::NotModifiedRegistered = "NOT MODIFIED."
    [ResultStatus]::NotModifiedUnregistered = "NOT MODIFIED."
}
function Test-ProductRegistration {
    param(
        [Parameter(Mandatory)]
        [array]$buildResult,
        [Parameter(Mandatory)]
        [ResultStatus] $expectedRegisteredStatus
    )

    $expectedRegistered = switch ($expectedRegisteredStatus) {
        Ok { $true }
        Registered { $true }
        Unregistered { $false }
        NotModifiedRegistered { return $true }
        NotModifiedUnregistered { return $false } 
        default { throw "Invalid expectedRegistered value: $expectedRegisteredStatus" }
    }
    #check tms list
    $products = tms list -json -detailed | ConvertFrom-Json -AsHashtable    
    foreach ($product in $products.values) {
        foreach ($ide in $product.ides.keys) {
            foreach ($plat in $product.ides[$ide].platforms.keys) {
                if ($product.ides[$ide].platforms[$plat].registered -ne $expectedRegistered) {
                    throw "Component $($plat) of product $($product.name) for IDE $($ide) should be $(if ($expectedRegistered) { 'registered' } else { 'unregistered' }), but it is not."
                }
                if ([string]::IsNullOrEmpty($product.ides[$ide].platforms[$plat]."registered_items") -and $expectedRegistered) {
                    throw "Component $($plat) of product $($product.name) for IDE $($ide) should have registered items, but it does not."
                } elseif (-not [string]::IsNullOrEmpty($product.ides[$ide].platforms[$plat]."registered_items") -and -not $expectedRegistered) {
                    throw "Component $($plat) of product $($product.name) for IDE $($ide) should not have registered items, but it does: `n$($product.ides[$ide].platforms[$plat]."registered_items")"
                }
            }
        }
    }

    #check build result
    $summary = $buildResult | tmsutil summary-to-json | ConvertFrom-Json -AsHashtable
    foreach ($productName in $summary.Keys) {
        $productSummary = $summary[$productName]
        $expectedStatusString = $ResultStatusNameMap[$expectedRegisteredStatus]
        if ($productSummary -ne $expectedStatusString) {
            throw "Product $productName should have status '$expectedStatusString' in the build result, but it has status '$productSummary'."
        }        
    }

    #check registry
    $WindowsPath = $env:Path
    if ($expectedRegistered) {
        $WindowsPath | Assert-ValueContains -expected "tmp-run\unregister\.tmssetup\build\bpl\Win32"
    } else {
        $WindowsPath | Assert-ValueNotContains -expected "tmp-run\unregister\.tmssetup\build\bpl\Win32"
    }
}


. test.setup
tmscredentials
tms server-enable community true
tms server-add testserver zipfile "file:///$($tmsTestRootDir.Replace('\', '/'))/tmp-run/test-repos/tmsbuild_test_repos.zip"

Set-AlternateRegistryKey -RegKey $regkey -RegisterAllDelphiVersions $true

tms config-write -p:"configuration for all products:options:skip register=false"

$buildResult = tms install tmstest.x:"v1.0.0" tms.webcore tms.biz.echo tmstest.a
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::Ok)

$buildResult = tms build
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::NotModifiedRegistered)

$buildResult = tms build -unregister
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::Unregistered)
$buildResult = tms build -unregister
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::Unregistered) # it should be ModifiedUnregistered, but we fully unregister every time.  

$buildResult = tms build
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::Registered)
$buildResult = tms build
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::NotModifiedRegistered)

$buildResult = tms build -unregister tmstest.x
$buildResult = tms build -unregister tmstest.x

$buildResult = tms build tmstest.x
$buildResult = tms build

Remove-Item "Products/tmstest.x" -Recurse -Force

$buildResult = tms build -unregister
$buildResult = tms build

tms config-write -p:"configuration for all products:options:skip register=true"
$buildResult = tms build

#pending: megafolders, uninstall a product, modify a source file and rebuild, compile the apps with bds.exe
# check to not unregister twice
# see the effect of skip register option

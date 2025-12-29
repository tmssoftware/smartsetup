# Check that we can unregister and re-register products without rebuilding them.

$regkey = "tmstest\unregistering"

enum ResultStatus {
    Ok
    OkUnregistered
    Registered
    Unregistered
    NotModifiedRegistered
    NotModifiedUnregistered
    IgnoredUnregistered
    IgnoredRegistered
    Uninstalled
}

$ResultStatusNameMap = @{
    [ResultStatus]::Ok = "OK."
    [ResultStatus]::OkUnregistered = "OK."
    [ResultStatus]::Registered = "REGISTERED."
    [ResultStatus]::Unregistered = "UNREGISTERED."
    [ResultStatus]::NotModifiedRegistered = "NOT MODIFIED."
    [ResultStatus]::NotModifiedUnregistered = "NOT MODIFIED."
    [ResultStatus]::IgnoredUnregistered = "IGNORED."
    [ResultStatus]::IgnoredRegistered = "IGNORED."
    [ResultStatus]::Uninstalled = "UNINSTALLED."
}
function Test-ProductRegistration {
    param(
        [Parameter(Mandatory)]
        [array]$buildResult,
        [Parameter(Mandatory)]
        [ResultStatus] $expectedRegisteredStatus,
        [Parameter(Mandatory=$false)]
        [array]$excludeProducts = @(),
        [Parameter(Mandatory=$false)]
        [array]$includeProducts = @(),
        [Parameter(Mandatory=$false)]
        [bool]$checkPath = $true
    )

    $expectedRegistered = switch ($expectedRegisteredStatus) {
        Ok { $true }
        OkUnregistered { $false }
        Registered { $true }
        Unregistered { $false }
        NotModifiedRegistered { $true }
        NotModifiedUnregistered { $false }
        IgnoredRegistered { $true }
        IgnoredUnregistered { $false }
        Uninstalled { $false }
        default { throw "Invalid expectedRegistered value: $expectedRegisteredStatus" }
    }
    #check tms list
    $products = tms list -json -detailed | ConvertFrom-Json -AsHashtable    
    foreach ($product in $products.values) {
        if ($excludeProducts -contains $product.name) {
            continue
        }
        if ($includeProducts.Count -gt 0 -and -not ($includeProducts -contains $product.name)) {
            continue
        }
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
        if ($excludeProducts -contains $productName) {
            continue
        }
        if ($includeProducts.Count -gt 0 -and -not ($includeProducts -contains $productName)) {
            continue
        }
        $productSummary = $summary[$productName]
        $expectedStatusString = $ResultStatusNameMap[$expectedRegisteredStatus]
        if ($productSummary -ne $expectedStatusString) {
            throw "Product $productName should have status '$expectedStatusString' in the build result, but it has status '$productSummary'."
        }        
    }

    #check registry// env:PATH won't be refreshed.
    if (-not $checkPath) {
        return
    }
    $WindowsPath = [System.Environment]::GetEnvironmentVariable("Path","User")
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

Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::IgnoredRegistered) -excludeProducts @("X")
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::Unregistered) -includeProducts @("X") -checkPath $false

$buildResult = tms build tmstest.x
#VCS products always try to update, so they come as not modified. Api products will be ignored.
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::NotModifiedRegistered) -includeProducts @("A","B","C","D","E","F","SPRING4D")
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::Registered) -includeProducts @("X")
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::IgnoredRegistered) -excludeProducts @("A","B","C","D","E","F","X","SPRING4D") -checkPath $false

Remove-Item "Products/tmstest.x" -Recurse -Force

$buildResult = tms build -unregister
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::Unregistered)

$buildResult = tms build
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::Registered) -excludeProducts @("X") 
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::Uninstalled) -includeProducts @("X") -checkPath $false

tms config-write -p:"configuration for all products:options:skip register=true"
$buildResult = tms build
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::Unregistered)

#  modify .\Products\tmstest.b\src\source\tmstest.b.pas to be all proper case
Set-Content -Path "Products/tmstest.b/src/source/tmstest.b.pas" -Value (Get-Content -Path "Products/tmstest.b/src/source/tmstest.b.pas" | ForEach-Object { $_.ToLower() } )

$buildResult = tms build
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::NotModifiedUnregistered) -excludeProducts @("A", "B")
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::OkUnregistered) -includeProducts @("A", "B")

Set-Content -Path "Products/tmstest.b/src/source/tmstest.b.pas" -Value (Get-Content -Path "Products/tmstest.b/src/source/tmstest.b.pas" | ForEach-Object { $_.ToUpper() } )
tms config-write -p:"configuration for all products:options:skip register=[windowspath]"
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::NotModifiedUnregistered) -excludeProducts @("A", "B")
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::OkUnregistered) -includeProducts @("A", "B")

Set-Content -Path "Products/tmstest.b/src/source/tmstest.b.pas" -Value (Get-Content -Path "Products/tmstest.b/src/source/tmstest.b.pas" | ForEach-Object { $_.ToLower() } )
tms config-write -p:"configuration for all products:options:skip register=[startmenu, help]"
$buildResult = tms build
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::Registered)


# finally unregister all again
$buildResult = tms build -unregister
Test-ProductRegistration -buildResult $buildResult -expectedRegisteredStatus ([ResultStatus]::Unregistered)


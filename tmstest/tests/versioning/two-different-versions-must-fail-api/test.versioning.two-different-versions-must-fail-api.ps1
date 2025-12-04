# If we install v1 and v2 of the same product, it must fail.
# both in community and tms servers.

. test.setup

tmscredentials

$versions = tms versions-remote tms.flexcel.vcl -json | ConvertFrom-Json
 
$sortedVersions = $versions.psobject.Properties.Name | Sort-Object {[version]$_}

$installResult = Invoke-WithExitCodeIgnored{tms install tms.flexcel.vcl:$($sortedVersions[-1]) tms.flexcel.vcl:$($sortedVersions[-2])}

$testOk = $false
foreach ($line in $installResult) {
    if ($line -like "*was requested to be installed in versions*") {
        $testOk = $true
        break
    }
}

if (-not ($testOk)) {
    Write-Error "The error message should mention 'was requested to be installed in versions'. Actual message: $($installResult)"
}

Write-Output "ok." 

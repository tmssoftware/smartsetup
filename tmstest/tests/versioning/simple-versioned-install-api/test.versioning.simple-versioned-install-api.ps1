# Test versioning in API servers.

. test.setup
tmscredentials

$versions = tms versions-remote tms.fnc.wxpack -json | ConvertFrom-Json

$sortedVersions = $versions.psobject.Properties.Name | Sort-Object {[version]$_}
$installResult = tms install tms.fnc.wxpack:$($sortedVersions[-1])
Test-BuildResultCounts -BuildResult $installResult -expectedNotModifiedCount 0 -expectedIgnoreCount 0 -expectedOkCount 2

#check the version installed is the correct one
$products = tms list -detailed -json | ConvertFrom-Json -AsHashtable
if ($Products["tms.fnc.wxpack"].version -ne $sortedVersions[-1]) {
    throw "tms.fnc.wxpack should be version $($sortedVersions[-1]), but it is $($Products["tms.fnc.wxpack"].version)."
}

$sortedcoreversions = (tms versions-remote tms.fnc.core -json | ConvertFrom-Json).psobject.Properties.Name | Sort-Object {[version]$_}
$installResult = tms install tms.fnc.core:$($sortedcoreversions[-2])
Test-BuildResultCounts -BuildResult $installResult -expectedNotModifiedCount 0 -expectedIgnoreCount 0 -expectedOkCount 2
Test-FetchResultCounts -FetchResult $installResult -expectedLines @("*- tms.fnc.core -> UPDATED*")

#check the version installed is the correct one
$products = tms list -detailed -json | ConvertFrom-Json -AsHashtable
if ($Products["tms.fnc.core"].version -ne $sortedcoreversions[-2]) {
    throw "tms.fnc.core should be version $($sortedcoreversions[-2]), but it is $($Products["tms.fnc.core"].version)."
}
if ($Products["tms.fnc.wxpack"].version -ne $sortedVersions[-1]) {
    throw "tms.fnc.wxpack should be version $($sortedVersions[-1]), but it is $($Products["tms.fnc.wxpack"].version)."
}

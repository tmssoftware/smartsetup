# If we install v1 and v2 of the same product, it must fail.
# both in community and tms servers.

. test.setup

tmscredentials

$versions = tms versions-remote tms.flexcel.vcl -json | ConvertFrom-Json
 
$sortedVersions = $versions.psobject.Properties.Name | Sort-Object {[version]$_}

Test-CommandFails { tms install tms.flexcel.vcl:$($sortedVersions[-1]) tms.flexcel.vcl:$($sortedVersions[-2]) } "*was requested to be installed in versions*"

Write-Output "ok." 

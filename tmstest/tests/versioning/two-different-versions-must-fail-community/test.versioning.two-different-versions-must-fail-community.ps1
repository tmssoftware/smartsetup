# If we install v1 and v2 of the same product, it must fail.
# both in community and tms servers.

. test.setup
tms server-enable community
tms server-enable tms false

$versions = tms versions-remote vsoft.cancellationtoken -json | ConvertFrom-Json
 
$sortedVersions = $versions.psobject.Properties.Name | Sort-Object {[version]$_.substring(1)}

Test-CommandFails { tms install vsoft.cancellationtoken:$($sortedVersions[-1]) vsoft.cancellationtoken:$($sortedVersions[-2]) } "*was requested to be installed in versions*"

Write-Output "ok." 

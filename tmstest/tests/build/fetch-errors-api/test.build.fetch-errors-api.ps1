# When a fetch fails but others don't we should still build. both community and tms servers.

. test.setup
tmscredentials
$buildResult = Invoke-WithExitCodeIgnored{ tms install tms.vcl.crypto tms.this.product.doesnotexist }
$testOk = Test-Result -CommandResult $buildResult -Message "*Error: Fetch failed for tms.this.product.doesnotexist*"

if (-not ($testOk)) {
    Write-Error "The error message should mention 'Fetch failed for tms.this.product.doesnotexist'. Actual message: $($buildResult)"
}
$installed = tms list -json | ConvertFrom-Json -AsHashtable
if (-not $installed.ContainsKey("tms.vcl.crypto")) {
    Write-Error "tms.vcl.crypto should have been installed despite the fetch error of the other product."
}
# When uninstalling with -force, it should delete the product and leave the dependencies installed.

. test.setup
tmscredentials
tms install tms.biz.aurelius

tms uninstall tms.biz.bcl -force

$buildResult = Invoke-WithExitCodeIgnored{tms build tms.biz.aurelius}
$testOk = Test-Result -CommandResult $buildResult -Message "*Error: The product ""TMS Aurelius"" requires the product ""TMS BIZ Core Library"" to build*"

if (-not ($testOk)) {
    Write-Error "The build should have failed due to missing dependency TMS BIZ Core Library. Actual message: $($buildResult)"
}

tms list -json | ConvertFrom-Json -AsHashtable | ForEach-Object {
    if (-not $_.ContainsKey("tms.biz.aurelius")) {
        Write-Error "tms.biz.aurelius should still be installed."
    }
    if ($_.ContainsKey("tms.biz.bcl")) {
        Write-Error "tms.biz.bcl should have been uninstalled."
    }
}
# Check that when there are fetch errors, no products are installed
 
. test.setup
tms server-enable community
tms server-enable tms false

tms config-write -p:"tms smart setup options:git:git location=wrongpath"

$buildResult = Invoke-WithExitCodeIgnored{ tms install sglienke.spring4d }
$testOk = Test-Result -CommandResult $buildResult -Message "* Error: Errors downloading products from version control.*"  
if (-not ($testOk)) {
    Write-Error "The error message should mention ' Error: Errors downloading products from version control.'. Actual message: $($buildResult)"
}
$installed = tms list -json | ConvertFrom-Json -AsHashtable
if ($installed.Count -ne 0) {   
    Write-Error "No products should have been installed, but $($installed.Count) were."
}

tms config-write -p:"tms smart setup options:git:git location="

tms install sglienke.spring4d

tms config-write -p:"tms smart setup options:git:pull command=mypullcommandthatdoesnotexist"
$buildResult2 = Invoke-WithExitCodeIgnored{ tms install sglienke.spring4d }
$testOk2 = Test-Result -CommandResult $buildResult2 -Message "* Error: Errors downloading products from version control.*"  
if (-not ($testOk2)) {
    Write-Error "The error message should mention ' Error: Errors downloading products from version control.'. Actual message: $($buildResult2)"
}
$installed2 = tms list -json | ConvertFrom-Json -AsHashtable
if ($installed2.Count -ne 1) {   
    Write-Error "One product should have been installed, but $($installed2.Count) were."
}


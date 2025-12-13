# See that included/excluded products work as expected
# in fetch, build, install, uninstall, update
 
. test.setup
tmscredentials
tms server-enable community

# first install all to see what is there.
tms install vsoft.* tms.biz.*

$installed = tms list -json | ConvertFrom-Json -AsHashtable
$allCount = $installed.Count

tms config-write -p:"tms smart setup options:excluded products=[vsoft.messaging,tms.biz.aurelius,tms.biz.bcl]" 

tms uninstall *
$installedAfterUninstall = tms list -json | ConvertFrom-Json -AsHashtable
if ($installedAfterUninstall.Count -ne 3) {
    Write-Error "After uninstall, there should be 3 products remaining, but there are $($installedAfterUninstall.Count)."
}

tms config-write -p:"tms smart setup options:excluded products=[]" 
tms uninstall *
$installedAfterUninstall2 = tms list -json | ConvertFrom-Json -AsHashtable
if ($installedAfterUninstall2.Count -ne 0) {
    Write-Error "After uninstall with no excluded products, there should be 0 products remaining, but there are $($installedAfterUninstall2.Count)."
}

# we exclude bcl, so it will fail to install aurelius, which depends on it.
tms config-write -p:"tms smart setup options:excluded products=[vsoft.messaging,tms.biz.echo,tms.biz.bcl]" 

Test-CommandFails { tms install vsoft.* tms.biz.* } "*Error: The product tms.biz.bcl is excluded in the section ""excluded products"" from tms.config.yaml.*"

# Actually the products will show as installed, even if it crashed during fetch.
# list -detailed shows if it is really installed and where (d11, d12), but we just won't check that here.

#$installed = tms list -json -detailed | ConvertFrom-Json -AsHashtable
#if ($installed.Count -ne 0) {
#    Write-Error "0 products should have been installed, but $($installed.Count) were."
#}

tms config-write -p:"tms smart setup options:excluded products=[]" 
tms uninstall *

tms config-write -p:"tms smart setup options:excluded products=[vsoft.messaging,tms.biz.echo]" 
tms install vsoft.* tms.biz.*

$installed = tms list -json | ConvertFrom-Json -AsHashtable
if ($installed.Count -ne $allCount - 2) {
    Write-Error "$($allCount - 2) products should have been installed, but $($installed.Count) were."
}

Test-CommandFails { tms install vsoft.messaging } "*Error: The product vsoft.messaging is excluded in the section ""excluded products"" from tms.config.yaml.*"

# check tms build. tms build is the only command that can be called alone without product names (you can do tms build, but not tms install)
# so tms build is the only one that should use included products. All the others replace included products with their command line arguments. 
tms config-write -p:"tms smart setup options:excluded products=[vsoft.commandline]" 
tms config-write -p:"tms smart setup options:included products=[vsoft.cancellationtoken]" 

$installedCountNow = (tms list -json | ConvertFrom-Json -AsHashtable).Count 
$BuildResult = tms build


Test-BuildResultCounts -BuildResult $BuildResult -expectedNotModifiedCount 1 -expectedIgnoreCount ($installedCountNow - 1) -expectedOkCount 0

$testOk = Test-Result -CommandResult $BuildResult -Message "*VSoft CancellationToken -> NOT MODIFIED*"  
if (-not ($testOk)) {   
    Write-Error "The error message should mention 'VSoft CancellationToken -> NOT MODIFIED'. Actual message: $($BuildResult)"
}

# finally, if we call tms build with a product name, included products should be ignored.

$BuildResult = tms build tms.biz.bcl
Test-BuildResultCounts -BuildResult $BuildResult -expectedNotModifiedCount ($installedCountNow - 4) -expectedIgnoreCount 4 -expectedOkCount 0

# finally let's test update
tms config-write -p:"tms smart setup options:excluded products=[vsoft.commandline]" 
tms config-write -p:"tms smart setup options:included products=[vsoft.cancellationtoken]" 

$BuildResult = tms update
Test-BuildResultCounts -BuildResult $BuildResult -expectedNotModifiedCount 1 -expectedIgnoreCount ($installedCountNow - 1) -expectedOkCount 0
$testWrong = Test-Result -CommandResult $BuildResult -Message "*Updating vsoft.commandline*"  
if($testWrong) {   
    Write-Error "The error message shouldn't mention 'Updating vsoft.commandline'. Actual message: $($BuildResult)"
}

Test-RepoFetchResultCounts -FetchResult $BuildResult -expectedLines @("*- vsoft.cancellationtoken -> OK*")
Test-FetchResultCounts -FetchResult $BuildResult -expectedLines @()


tms config-write -p:"tms smart setup options:excluded products=[tms.biz.aurelius]" 
tms config-write -p:"tms smart setup options:included products=[vsoft.cancellationtoken]" 

$BuildResult = tms update
Test-BuildResultCounts -BuildResult $BuildResult -expectedNotModifiedCount 1 -expectedIgnoreCount ($installedCountNow - 1) -expectedOkCount 0
$testWrong = Test-Result -CommandResult $BuildResult -Message "*Updating vsoft.commandline*"  
if ($testWrong) {   
    Write-Error "The error message shouldn't mention 'Updating vsoft.commandline'. Actual message: $($BuildResult)"
}

Test-RepoFetchResultCounts -FetchResult $BuildResult -expectedLines "*- vsoft.cancellationtoken -> OK*"
Test-FetchResultCounts -FetchResult $BuildResult -expectedLines @()

tms config-write -p:"tms smart setup options:excluded products=[]" 
tms config-write -p:"tms smart setup options:included products=[tms.biz.aurelius]" 

$BuildResult = tms update
Test-BuildResultCounts -BuildResult $BuildResult -expectedNotModifiedCount 6 -expectedIgnoreCount ($installedCountNow - 6) -expectedOkCount 0

Test-RepoFetchResultCounts -FetchResult $BuildResult -expectedLines @()
Test-FetchResultCounts -FetchResult $BuildResult -expectedLines @("*- tms.biz.aurelius -> SKIPPED (Up to date)*","*- tms.biz.bcl -> SKIPPED (Up to date)*")

tms config-write -p:"tms smart setup options:excluded products=[]" 
tms config-write -p:"tms smart setup options:included products=[vsoft.cancellationtoken]"
$BuildResult = tms update tms.biz.sphinx
Test-BuildResultCounts -BuildResult $BuildResult -expectedNotModifiedCount 6 -expectedIgnoreCount ($installedCountNow - 6) -expectedOkCount 0
$testOk = Test-Result -CommandResult $BuildResult -Message "*- tms.biz.sphinx -> SKIPPED (Up to date)*"  
if (-not ($testOk)) {   
    Write-Error "The error message should mention '- tms.biz.sphinx -> SKIPPED (Up to date)'. Actual message: $($BuildResult)"
}

Test-RepoFetchResultCounts -FetchResult $BuildResult -expectedLines @()
Test-FetchResultCounts -FetchResult $BuildResult -expectedLines @(
    "*- tms.biz.sphinx -> SKIPPED (Up to date)*",
    "*- tms.biz.bcl -> SKIPPED (Up to date)*",
    "*- tms.biz.aurelius -> SKIPPED (Up to date)*",
    "*- tms.biz.sparkle -> SKIPPED (Up to date)*",
    "*- tms.biz.xdata -> SKIPPED (Up to date)*")

# note: "tms update" is different from "tms update *". The second one doesn't use included products, only the first one does.
# both use excluded products.
$BuildResult = tms update *
Test-BuildResultCounts -BuildResult $BuildResult -expectedNotModifiedCount ($installedCountNow) -expectedIgnoreCount 0 -expectedOkCount 0

Test-RepoFetchResultCounts -FetchResult $BuildResult -expectedLines @(
    "*- vsoft.commandline -> OK*",
    "*- vsoft.cancellationtoken -> OK*")

Test-FetchResultCounts -FetchResult $BuildResult -expectedLines @(
    "*- tms.biz.sphinx -> SKIPPED (Up to date)*",
    "*- tms.biz.bcl -> SKIPPED (Up to date)*",
    "*- tms.biz.remotedb -> SKIPPED (Up to date)*",
    "*- tms.biz.scripter -> SKIPPED (Up to date)*",
    "*- tms.biz.xdata -> SKIPPED (Up to date)*",
    "*- tms.biz.aurelius -> SKIPPED (Up to date)*",
    "*- tms.biz.logging -> SKIPPED (Up to date)*",
    "*- tms.biz.sparkle -> SKIPPED (Up to date)*")

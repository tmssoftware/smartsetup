# check that when a build fails, and we try again, we try to rebuild the failed project, not just skip it because it was already built

. test.setup

tms config-write -p:configuration-for-all-products:replace_platforms=[win32intel,win64intel,win64xintel]
# 4.2.3.0 was a version that wouldn't build.

Invoke-WithExitCodeIgnored { tms install tms.fnc.core:4.2.3.0 }

# pin it so we don't try to update it.
tms pin tms.fnc.core

$buildResult = Invoke-WithExitCodeIgnored { tms install tms.fnc.uipack }

#if $result contains "Can't find the package" then there was an error. It should try to build all packages
$testOk = Test-Result -CommandResult $buildResult -Message "*Building package TMSFNCCorePkg*"
if ($testOk) {
    Write-Host "Test passed: The build process tried to build the missing package." -ForegroundColor Green
}
else {
    throw "Test failed: The build process did not try to build the missing package."
}
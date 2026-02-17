# Install all the products and see that there are no errors.
# This is a slow test, so it should be named test.slow.build.install-everything.ps1
# It should be run with `tmstest -skip-slow` to skip it.

. test.setup
Write-Output "bds.exe: $($Env:TMS_BDS)"
$regkey = "tmstest\everything"

# Duplicate the testapp. We will test it with smartsetup and with msbuild (to see if the paths are ok)

Copy-Item -Path testapp -Destination testapp-no-tmsbuild -Exclude tmsbuild.yaml -Recurse -Force

Set-AlternateRegistryKey -RegKey $regkey -RegisterAllDelphiVersions $false

tmscredentials
tms server-enable community
tms config-write -p:"configuration for all products:options:skip register=false" #we want to check also if components are registered correctly. We will run msbuild directly later.
tms config-write -p:"tms smart setup options:error if skipped=false"
tms config-write -p:"tms smart setup options:excluded products=[zeos.zeoslib]" #Zeos doesn't build currently. A pity, since it is the only way to test SVN at the moment. See https://sourceforge.net/p/zeoslib/tickets/633/

tms fetch *   #do not install, so we can build with the testapp at the same time and see deps work fine.
tms fetch  tms.flexcel.vcl tms.biz.aurelius tms.vcl.crypto sglienke.spring4d tms.biz.scripter tms.fnc.core

tms build #this will also build testapp.

tms update

Write-Output "Now compiling the app and checking the result."
$expectedResult = "1DC22C912262B3FDA386381714E4674E240860FB66DD5A70307F800CFA4A6B6F"
$Result = & .\testapp\Win32\Release\testapp.exe
if ($Result -ne $expectedResult) {
    Write-Error "Install everything failed: Result was '$Result' and should have been '$expectedResult'."
} else {
    Write-Output "testapp.exe worked."
}

Write-Output "Now testing with bds.exe"

Set-Location testapp-no-tmsbuild

# msbuild # msbuild uses EnvOptions.dproj, which hasn't been updated yet as we didn't launch the ide.
bds "testapp.dproj" $regkey

$Result = & .\Win32\Debug\testapp.exe
if ($Result -ne $expectedResult) {
    Write-Error "Install everything failed at msbuild: Result was '$Result' and should have been '$expectedResult'."
}

Set-Location -
uninstall_and_check(1) #only if no errors. If there are errors, we leave it there for investigation.

#check registry is the same as the one we used to install.


# Verify that we can install in Delphi CE
# As we probably don't have delphi CE installed, we simulate that the command line compilers aren't available.
# Delphi CE compilation is single threaded, so we test multiple products at once.
# Note also that big projects will show a nag screen, but this is not a real CE installation.

. test.setup
tmscredentials
tms server-enable community

$result = tms install vsoft.yaml tms.biz.bcl -test-delphi-ce
Test-CommandOk { $result } "*Rad Studio CE detected. Disabling multithreaded compilation.*"

Test-BuildResultCounts $result 0 0 2

$buildOutputFolder = "./Products/tms.biz.bcl/Packages"
$bplfiles = Get-ChildItem -Path $buildOutputFolder -Filter *.bpl -Recurse
if ($bplfiles.Count -eq 0) {
    throw "No BPL files were created in the build output folder '$buildOutputFolder'."
}
$dcufiles = Get-ChildItem -Path $buildOutputFolder -Filter *.dcu -Recurse
if ($dcufiles.Count -eq 0) {
    throw "No DCU files were created in the build output folder '$buildOutputFolder'."
}

$buildOutputFolder = "./Products/vsoft.yaml/src/Packages"
$bplfiles = Get-ChildItem -Path $buildOutputFolder -Filter *.bpl -Recurse
if ($bplfiles.Count -eq 0) {
    throw "No BPL files were created in the build output folder '$buildOutputFolder'."
}
$dcufiles = Get-ChildItem -Path $buildOutputFolder -Filter *.dcu -Recurse
if ($dcufiles.Count -eq 0) {
    throw "No DCU files were created in the build output folder '$buildOutputFolder'."
}


tms uninstall * -test-delphi-ce
    
$expectedRemaining = 0
$remaining = tms list -json | ConvertFrom-Json -AsHashtable
if ($remaining.Count -ne $expectedRemaining) {
    throw "There should be $expectedRemaining results remaining, but there are $($remaining.Count)."
}

# we already had tests for compiling in CE, but they weren't working for "+" delphi versions.
# we will check FlexCel here, which uses a D12+ folder.

. test.setup
tmscredentials
tms server-enable community

$result = tms install tms.flexcel.vcl -test-delphi-ce
Test-CommandOk { $result } "*Rad Studio CE detected. Disabling multithreaded compilation.*"

tms config-write -p:"configuration for all products:advanced options:keep parallel folders = false"
Test-BuildResultCounts $result 0 0 1

#check that the bpls were created
$buildOutputFolder = "./Products/tms.flexcel.vcl/Packages"
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

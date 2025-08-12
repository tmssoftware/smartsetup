# Makes sure we support the FRAMEWORK_VCL and FRAMEWORK_FMX defines introduced in Delphi 11.1
# See https://github.com/tmssoftware/tms-smartsetup/issues/261

. test.setup

tms config-write -p:"configuration for test.framework.defines.ok:compilation options:add-defines=[MYDEF]"

$results = Invoke-WithExitCodeIgnored{tms build -full} | tmsutil summary-to-json | ConvertFrom-Json -AsHashtable 

if ($results.Count -ne 3) {
    Write-Error "There should be 3 results, but there are $($results.Count)."
}

foreach ($result in $results.keys) {
    if ($result.Contains($results[$result]) -eq $false) {
        Write-Error "The line '$result' doesn't contain '$($results[$result])'."
    }
}   


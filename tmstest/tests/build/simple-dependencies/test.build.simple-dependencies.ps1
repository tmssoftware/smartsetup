. test.setup

tms credentials -code:$env:TMSTEST_CODE -email:$env:TMSTEST_EMAIL

tms install tms.biz.aurelius
$results = tms list -json | ConvertFrom-Json -AsHashtable

if ($results.Count -ne 2) {
    Write-Error "There should be 2 results, but there are $($results.Count)."
}

if (!$results['tms.biz.aurelius']) {
    Write-Error "tms.biz.aurelius should be installed."
}
if (!$results['tms.biz.bcl']) {
    Write-Error "tms.biz.bcl should be installed."
}
if ($results['tms.fnc.core']) {
    Write-Error "tms.fnc.core should not be installed."
}

tms install tms.biz.echo tms.fnc.uipack

$results = tms list -json | ConvertFrom-Json -AsHashtable

if ($results.Count -ne 7) {
    Write-Error "There should be 7 results, but there are $($results.Count)."
}

if (!$results['tms.biz.echo']) {
    Write-Error "tms.biz.echo should be installed."
}
if (!$results['tms.fnc.uipack']) {
    Write-Error "tms.fnc.uipack should be installed."
}

try {
  tms uninstall tms.biz.bcl
  Write-Output "Error: tms.biz.bcl should not be uninstalled because it is required by tms.biz.aurelius."
  exit 1
}
catch {
}

tms uninstall tms.biz.bcl -force
$results = tms list -json | ConvertFrom-Json -AsHashtable
if ($results.Count -ne 6) {
    Write-Error "There should be 6 results, but there are $($results.Count)."
}

$afterReinstalling = tms install tms.biz.bcl |  tmsutil summary-to-json | ConvertFrom-Json -AsHashtable
tms uninstall tms.biz.echo -cascade
$results = tms list -json | ConvertFrom-Json -AsHashtable
if ($results.Count -ne 2) {
    Write-Error "There should be 2 results, but there are $($results.Count)."
}

uninstall-and-check(0)
exit 0

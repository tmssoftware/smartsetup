. test.setup

tmscredentials

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

$uninstallResult = tms uninstall tms.biz.bcl -force  |  tmsutil summary-to-json | ConvertFrom-Json -AsHashtable

if ($uninstallResult.Count -ne 7) {
    Write-Error "There should be 7 products listed, but there are $($uninstallResult.Count)."
}

foreach ($product in $uninstallResult.Keys) {
    if ($product.Contains('TMS BIZ Core')) {
        if ($uninstallResult[$product] -ne 'UNINSTALLED.') {
            Write-Error "$product should be uninstalled."
        } 
    } else {
        if ($uninstallResult[$product] -ne 'IGNORED.') {
            Write-Error "$product should be ignored."
        }
    }
}

$results = tms list -json | ConvertFrom-Json -AsHashtable
if ($results.Count -ne 6) {
    Write-Error "There should be 6 results, but there are $($results.Count)."
}

$afterReinstalling = tms install tms.biz.bcl |  tmsutil summary-to-json | ConvertFrom-Json -AsHashtable

if ($afterReinstalling.Count -ne 7) {
    Write-Error "There should be 7 products rebuild, but there are $($afterReinstalling.Count)."
}

foreach ($product in $afterReinstalling.Keys) {
    if ($product.Contains('TMS FNC')) {
        if ($afterReinstalling[$product] -ne 'IGNORED.') {
            Write-Error "$product should be ignored."
        } 
    } else {
        if ($afterReinstalling[$product] -ne 'OK.') {
            Write-Error "$product should be rebuilt."
        }
    }
}

tms uninstall tms.biz.echo -cascade
$results = tms list -json | ConvertFrom-Json -AsHashtable
if ($results.Count -ne 2) {
    Write-Error "There should be 2 results, but there are $($results.Count)."
}

uninstall_and_check(0)
exit 0

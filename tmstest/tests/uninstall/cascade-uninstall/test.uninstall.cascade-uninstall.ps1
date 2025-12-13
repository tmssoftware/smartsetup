# Cascade uninstall should uninstall all dependencies as well.

. test.setup
tmscredentials
tms install tms.biz.echo
tms uninstall tms.biz.echo -cascade

$remaining = tms list -json | ConvertFrom-Json -AsHashtable
if ($remaining.Count -ne 0) {  
    Write-Error "All products should have been uninstalled, but $($remaining.Count) remain."
}

tms install tms.biz.sparkle
tms install tms.biz.aurelius
Test-CommandFails { tms uninstall tms.biz.aurelius -cascade } "*Error: Uninstall failed: tms.biz.bcl*"


$remaining = tms list -json | ConvertFrom-Json -AsHashtable
if ($remaining.Count -ne 2) {  
    Write-Error "only tms.biz.bcl and sparkle should remain, but $($remaining.Count) remain."
}
if (-not $remaining.ContainsKey("tms.biz.bcl")) {
    Write-Error "tms.biz.bcl should remain installed."
}
if (-not $remaining.ContainsKey("tms.biz.sparkle")) {
    Write-Error "tms.biz.sparkle should remain installed."
}

tms uninstall tms.biz.sparkle -cascade
$remaining = tms list -json | ConvertFrom-Json -AsHashtable
if ($remaining.Count -ne 0) {  
    Write-Error "All products should have been uninstalled, but $($remaining.Count) remain."
}
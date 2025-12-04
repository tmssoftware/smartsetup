# If we uninstall tms.fnc.core, tms.fnc.chart and tms.fnc.uipack, it should fail to uninstall core (it has more dependencies), but not uipack

. test.setup

tmscredentials

tms install tms.fnc.chart tms.fnc.uipack


$uninstallResult = Invoke-WithExitCodeIgnored{tms uninstall tms.fnc.core tms.fnc.uipack}
$testOk = Test-Result -CommandResult $uninstallResult -Message "*Error: Uninstall failed: tms.fnc.core"

if (-not ($testOk)) {
    Write-Error "The uninstall of tms.fnc.core should have failed due to it depending on tms.fnc.chart. Actual message: $($uninstallResult)"
}

$remaining = tms list -json | ConvertFrom-Json -AsHashtable
if (-not $remaining.ContainsKey("tms.fnc.core")) {
    Write-Error "tms.fnc.core should still be installed."
}
if ($remaining.ContainsKey("tms.fnc.uipack")) {
    Write-Error "tms.fnc.uipack should have been uninstalled."
}

tms uninstall tms.fnc.core tms.fnc.chart

$remaining = tms list -json | ConvertFrom-Json -AsHashtable
if ($remaining.Count -ne 0) {  
    Write-Error "All products should have been uninstalled, but $($remaining.Count) remain."
}

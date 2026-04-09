# Check that installing one product doesn't interfere with another product.

. test.setup

#this one will fail, that fnc core version version was wrong.
#we will ignore the failure and continue with the test, as we want to verify that the second product can be installed even if the first one fails.
tms install tms.*.excel.bridge
Invoke-WithExitCodeIgnored{ tms install tms.fnc.core:4.2.2.10 }
Invoke-WithExitCodeIgnored{ tms install tms.vcl.uipack }

$installed = tms list -detailed -json | ConvertFrom-Json -AsHashtable
foreach ($ide in $installed["tms.vcl.uipack"].ides.GetEnumerator()) {
    if (!$ide.Value.platforms.win32intel.built) {
        throw "tms.vcl.uipack should be built for win32intel, but it is not."
    }
}

if ($installed["tms.fnc.uipack"].ContainsKey('ides')) {
    throw "tms.fnc.core should not be installed (its install was expected to fail), but it is found in the list."
}


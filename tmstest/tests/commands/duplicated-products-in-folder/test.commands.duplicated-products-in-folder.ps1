#Check that duplicated products don't give cryptic errors

. test.setup

tms fetch tms.fnc.uipack

Copy-Item ./Products/tms.fnc.uipack ./Products/tms.fnc.uipack.dup -Recurse -Force

$result = Invoke-WithExitCodeIgnored {tms build}

CheckTextHasString -Text "$result" -Message "*Make sure only one is available*"

tms pin *
tms unpin *
$result = Invoke-WithExitCodeIgnored {tms self-update -nofetch}
CheckTextHasString -Text "$result" -Message "*\Products\tms.fnc.uipack.dup* and *\Products\tms.fnc.uipack*Make sure only one is available*"

uninstall_and_check 0

# See that we fail when a dependency is missing

. test.setup

tmscredentials
tms install tms.biz.echo

Remove-Item -Recurse -Force ".\Products\tms.biz.aurelius"

$results = Invoke-WithExitCodeIgnored{tms build tms.biz.echo} 

if (-not ($results | Out-String).Contains('The product "TMS Echo" requires the product "TMS Aurelius"')) {
    Write-Error "Unexpected error message: $results."
}

$results = Invoke-WithExitCodeIgnored{tms build} 

if (-not ($results | Out-String) -like 'The product "*" requires the product "TMS Aurelius"') {
    Write-Error "Unexpected error message when installing all: $results."
}

uninstall_and_check(0)
exit(0)

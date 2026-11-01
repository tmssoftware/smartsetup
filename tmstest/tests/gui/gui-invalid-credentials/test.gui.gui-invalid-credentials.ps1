#Check what happens if there are no credentials.
. test.setup
tms server-enable community true

$creds = "tmsguitest.emptycreds"

# delete existing credentials
tms credentials -code:" " -email:" " -test-credentials-profile:$creds
tms credentials -code:" " -email:" " -test-credentials-profile:$creds 

$existing = tms credentials -print -json -test-credentials-profile:$creds | ConvertFrom-Json -AsHashtable
$existing.Count | Assert-ValueIs 0
Test-tmsgui -suffixParam "ok" -ExtraArgs "-test-tms:-test-credentials-profile:$($creds)"

tms credentials -code:" " -email:" " -test-credentials-profile:$creds
tms credentials -code:" " -email:" " -test-credentials-profile:$creds  
Test-tmsgui -suffixParam "cancel" -ExtraArgs "-test-tms:-test-credentials-profile:$($creds)"

Test-tmsgui -suffixParam "no-tms-credentials" -ExtraArgs "-test-tms:-test-credentials-profile:$($creds)"

$servers =  tms server-list -json | ConvertFrom-Json -AsHashtable
if ($servers.Count -ne 2) {
    throw "There should be 2 servers, but there are $($servers.Count)."
}
if ($servers["community"].enabled -ne $true) {
    throw "The community server should be enabled."
}

if ($servers["tms"].enabled -ne $false) {
    throw "The tms server should be disabled."
}

Test-tmsgui -suffixParam "no-tms-server" -ExtraArgs "-test-tms:-test-credentials-profile:$($creds)"

tms server-enable tms
Test-tmsgui -suffixParam "invalid-credentials" -ExtraArgs "-test-tms:-test-credentials-profile:$($creds)"

tms credentials -code:" " -email:" " -test-credentials-profile:$creds
tms credentials -code:" " -email:" " -test-credentials-profile:$creds 

Test-tmsgui -suffixParam "offline" -ExtraArgs "-test-tms:`"-test-credentials-profile:$($creds) -test-offline`""

# check that we show a welcome screen in an empty folder

. test.setup

# no welcome screen as we have tms.config.yaml
Test-tmsgui

# welcome screen
Remove-Item -Path .\tms.config.yaml -Force
Test-tmsgui 2

$servers = tms server-list -json | ConvertFrom-Json -AsHashtable
if ($servers.Count -ne 2) {
    throw "There should be 2 servers in the list, but there are $($servers.Count)."
}
if ($servers.tms.enabled -ne $true) {
    throw "The tms server should be enabled, but it is not."
}

if ($servers.community.enabled -ne $false) {
    throw "The community server should not be enabled, but it is."
}

Test-tmsgui
Remove-Item -Path .\tms.config.yaml -Force
Test-tmsgui 3

$servers = tms server-list -json | ConvertFrom-Json -AsHashtable
if ($servers.Count -ne 2) {
    throw "There should be 2 servers in the list, but there are $($servers.Count)."
}
if ($servers.tms.enabled -ne $false) {
    throw "The tms server should not be enabled, but it is."
}

if ($servers.community.enabled -ne $true) {
    throw "The community server should be enabled, but it is not."
}
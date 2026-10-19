# check that we show a welcome screen in an empty folder

. test.setup

# no welcome screen as we have tms.config.yaml
Test-tmsgui no-welcome-screen

# welcome screen
mkdir empty-folder
Push-Location empty-folder
Test-tmsgui welcome-screen-no-changes

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

Test-tmsgui no-welcome-screen

Pop-Location
mkdir empty-folder-3
Push-Location empty-folder-3
Test-tmsgui welcome-screen-enable-community

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

Pop-Location
mkdir empty-folder-4
Push-Location empty-folder-4
#create an empty file
New-Item -ItemType File -Name dont-use-this-folder.txt
Test-tmsgui wrong-folder

Test-tmsgui wrong-folder-but-keep-going

Test-tmsgui wrong-folder-but-really-keep-going

Pop-Location
mkdir empty-folder-5
Push-Location empty-folder-5
#create an empty file to fool the test
New-Item -ItemType File -Name tms.exe
New-Item -ItemType File -Name also-some-file.txt
New-Item -ItemType Directory -Name dont-use-this-folder
Test-tmsgui welcome-screen-no-changes

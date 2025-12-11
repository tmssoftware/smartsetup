# Test for every command how they behave with no internet connection.

. test.setup
tmscredentials -test-offline

Test-CommandFails { tms install tms.biz.aurelius -test-offline } "*Offline mode, no connection to internet.*" "tms install"

# Install something in Online mode first, so others commands have something to work with.
tms install tms.biz.aurelius

Test-CommandOk { tms help -test-offline } "*Usage: tms help <command>*" "tms help"
Test-CommandOk { tms help install -test-offline } "*Installs the specified product(s)*" "tms help install"

Test-CommandFails { tms update -test-offline } "*Offline mode, no connection to internet.*" "tms update"
Test-CommandFails { tms fetch tms.biz.aurelius -test-offline } "*Offline mode, no connection to internet.*" "tms fetch"

tms build -test-offline


Test-CommandFails { tms versions-remote tms.biz.aurelius -test-offline } "*Offline mode, no connection to internet.*" "tms versions-remote"

Test-CommandOk { tms uninstall tms.biz.aurelius -cascade -test-offline } "* TMS BIZ Core Library * -> UNINSTALLED.*" "tms uninstall"
Test-CommandOk { tms uncompress ./Downloads/CurrentVersions/tms.biz.aurelius_production_*.zip ./Products -test-offline } "*Decompressed tms.biz.aurelius_production_*" "tms uncompress"
Test-CommandOk { tms uncompress ./Downloads/CurrentVersions/tms.biz.bcl_production_*.zip ./Products -test-offline } "*Decompressed tms.biz.bcl_production_*" "tms uncompress"

$products = tms list -json -test-offline | ConvertFrom-Json -AsHashtable
if ($products.Count -ne 2) {
    throw "There should be 2 products installed, but there are $($products.Count)."
}

Test-CommandFails {tms list-remote -test-offline} "*Offline mode, no connection to internet.*" "tms list-remote"
Test-CommandFails {tms versions-remote a.b.c -test-offline} "*Offline mode, no connection to internet.*" "tms versions-remote"

$info = tms info -json -test-offline | ConvertFrom-Json -AsHashtable
if ($info["has credentials"] -ne $true) {
    throw "The info command should show that we have credentials."
}

$info = tms server-list -json -test-offline | ConvertFrom-Json -AsHashtable
if ($info.Count -ne 2) {
    throw "The server-list command should show 2 servers."
}    
if ($info["tms"].enabled -ne $true) {
    throw "The tms server should be enabled."
}
if ($info["community"].enabled -ne $false) {
    throw "The community server should be disabled."
}

$logPlace = tms log-view -print -test-offline
if (-not (Test-Path $logPlace)) {
    throw "The log-view command should show a valid log file path."
}

$configPlace = tms config -print -test-offline
if (-not (Test-Path $configPlace)) {
    throw "The config-view command should show a valid config file path."
}

tms config-write -p:"tms smart setup options:build cores=1" -test-offline

$cores = tms config-read "tms smart setup options:build cores" -test-offline
if ($cores -ne "1") {
    throw "The config-write or config-read command did not work as expected."
}

tms server-enable community -test-offline
Test-CommandFails { tms install tms.biz.aurelius -test-offline } "*Offline mode, no connection to internet.*" "tms install"
tms server-enable tms false -test-offline
Test-CommandFails { tms install tms.biz.aurelius -test-offline } "*Offline mode, no connection to internet.*" "tms install"

tms server-add testserver zipfile https://example.com -test-offline
$info = tms server-list -json -test-offline | ConvertFrom-Json -AsHashtable
if ($info.Count -ne 3) {
    throw "The server-list command should show 3 servers."
}    
tms server-remove testserver -test-offline
$info = tms server-list -json -test-offline | ConvertFrom-Json -AsHashtable
if ($info.Count -ne 2) {
    throw "The server-list command should show 2 servers."
}    

Test-CommandOk { tms doctor -test-offline} "*Checking Windows User Path*" "tms doctor"



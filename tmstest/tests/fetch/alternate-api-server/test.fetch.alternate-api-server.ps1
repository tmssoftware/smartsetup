#test that we can use an alternate api server

. test.setup

tms server-enable community false
tms server-enable tms false

tms server-add alternate-api-server api https://api.landgraf.dev/tms

tms credentials -server:alternate-api-server -code:" " -email:" "
Test-CommandFails {tms fetch tms.biz.aurelius} -Message "*Credentials not provided*"

tms credentials -server:alternate-api-server -code:$env:TMSTEST_CODE -email:$env:TMSTEST_EMAIL
$build = tms fetch tms.biz.aurelius

$result = tms list -json | ConvertFrom-Json -AsHashtable
if (-not $result.ContainsKey("tms.biz.aurelius")) {
    throw "The product tms.biz.aurelius should be installed, but it is not found in the list."
}

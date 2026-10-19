#Check we can cancel and nothing remains hang
. test.setup
tms server-enable community true

Test-tmsgui

$installed = tms list -json -detailed | ConvertFrom-Json -AsHashtable
$installedIdes = $installed["tms.webcore"].Contains("ides")
if ($installedIdes) {
    throw "there should be products installed, but there are not: $installed"
}   
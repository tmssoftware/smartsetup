#Check we can cancel and nothing remains hang
. test.setup
tms server-enable community true

Test-tmsgui

$installed = tms list -json -detailed | ConvertFrom-Json -AsHashtable
$installedIdes = $installed["sglienke.spring4d"].Contains("ides")
if (-not $installedIdes) {
    throw "there should be products installed, but there are not: $installed"
}   
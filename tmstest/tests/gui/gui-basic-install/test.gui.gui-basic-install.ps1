#Check a simple install of community and api products
. test.setup
tms server-enable community true

Test-tmsgui

$installed = tms list -json -detailed | ConvertFrom-Json -AsHashtable
$installedIdes = $installed["jedi.jcl"].Contains("ides")
if (-not $installedIdes) {
    throw "there should be products installed, but there are not: $installed"
}   
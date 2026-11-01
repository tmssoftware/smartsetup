#Check we can cancel and nothing remains hang
. test.setup
tms server-enable community true

#install a single product so when we cancel, we are sure it didn't install for some platforms or ides
tms config-write -p:configuration-for-all-products:replace-delphi-versions=[delphi13] -p:configuration-for-all-products:platforms=[win32intel] -p:configuration-for-all-products:options:skip-register=false

Test-tmsgui

$installed = tms list -json -detailed | ConvertFrom-Json -AsHashtable
$installedIdes = $installed["tms.flexcel.vcl"].Contains("ides")
if ($installedIdes) {
    throw "there should be no products installed, but there are: $installed"
}
# Makes sure we can't add a TMS server different from tms.
# See https://github.com/tmssoftware/tms-smartsetup/issues/263
. test.setup

tms config --print #add a config file so we can run tms

tms server-remove TMS
tms server-add TMS zipfile file://asdfasdfda 
$Servers = tms server-list -json | ConvertFrom-Json
if ($Servers.TMS.name -cne "tms") {
    Write-Error "It shouldn't let you add a TMS (uppercase) server."
}
if ($Servers.tms.protocol -ne "api") {
    Write-Error "tms server shouldn't have a protocol."
}




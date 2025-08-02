# Makes sure we can't add a TMS server different from tms.
# See https://github.com/tmssoftware/tms-smartsetup/issues/263

. test.setup

tms config --print #add a config file so we can run tms
try {
  tms server-add TMS zipfile file://asdfasdfda   
  Write-Output "It shouldn't let you add a TMS server."
  exit -1
}
catch {
}



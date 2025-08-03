# Makes sure we can't add a TMS server different from tms.
# See https://github.com/tmssoftware/tms-smartsetup/issues/263
. test.setup

tms config --print
tms server-add TMSTest zipfile file://tomato 
try {
  tms server-add TMSTest zipfile file://potato 
  Write-Output "It shouldn't let you add another TMSTest server."
  exit(-1);    
}
catch {
    
}
$Servers = tms server-list -json | ConvertFrom-Json
if ($Servers.TMSTest.url -cne "file://tomato") {
   
  Write-Error "The server added should have the url file://tomato."
}

try {
tms build -config:tms.config.duplicated.yaml
Write-Output "It shouldn't let you build with a duplicated server."
exit(-1);
}
catch {
}
exit 0





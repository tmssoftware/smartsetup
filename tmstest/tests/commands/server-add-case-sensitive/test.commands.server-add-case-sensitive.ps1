# Makes sure we can't add a TMS server different from tms.
# See https://github.com/tmssoftware/tms-smartsetup/issues/263

. test.setup
$ErrorActionPreference = "SilentlyContinue"
 $err_message = tms server-add TMS zipfile file://asdfasdfda

if (-not "$($err_message)".Contains('Error: Server "TMS" is already added in tms.config.yaml.')) {
  Write-Output "It shouldn't let you add a TMS server."
  exit -1
}

$err_message = tms server-add community zipfile file://asdfasdfda

if (-not "$($err_message)".Contains('Error: Server "community" is already added in tms.config.yaml.')) {
  Write-Output "It shouldn't let you add a community server."
  exit -1
}

exit 0



# Makes sure we can't add a TMS server different from tms.
# See https://github.com/tmssoftware/tms-smartsetup/issues/263

. test.setup

tms credentials -code:"test" -email:"test@example.com"
$Credentials = tms credentials -print -json | ConvertFrom-Json
if ($Credentials.code -ne "test") {
    Write-Error "The credentials code should be 'test'."
}
if ($Credentials.email -ne "test@example.com") {
    Write-Error "The credentials email should be 'test@example.com'."
}

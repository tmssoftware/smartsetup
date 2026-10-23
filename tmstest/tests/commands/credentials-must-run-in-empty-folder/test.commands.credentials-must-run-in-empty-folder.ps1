# Checks that credentials can be set in an empty folder, or you wouldn't be able to set them in a new setup.

. test.setup

Remove-Item -Path (Join-Path -Path (Get-Location) -ChildPath "tms.config.yaml")

# delete existing credentials
tms credentials -code:" " -email:" " -test-credentials-profile:tmstest.emptycreds
tms credentials -code:" " -email:" " -test-credentials-profile:tmstest.emptycreds  #check deleting non-existing creds doesn't crash

$existing = tms credentials -print -json -test-credentials-profile:tmstest.emptycreds | ConvertFrom-Json -AsHashtable
$existing.Count | Assert-ValueIs 0

tms credentials -code:"test" -email:"test@example.com" -test-credentials-profile:tmstest.emptycreds
$Credentials = tms credentials -print -json -test-credentials-profile:tmstest.emptycreds| ConvertFrom-Json
if ($Credentials.code -ne "test") {
    Write-Error "The credentials code should be 'test'."
}
if ($Credentials.email -ne "test@example.com") {
    Write-Error "The credentials email should be 'test@example.com'."
}

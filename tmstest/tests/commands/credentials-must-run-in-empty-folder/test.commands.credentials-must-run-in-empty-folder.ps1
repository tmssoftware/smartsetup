# Checks that credentials can be set in an empty folder, or you wouldn't be able to set them in a new setup.

. test.setup

Remove-Item -Path (Join-Path -Path (Get-Location) -ChildPath "tms.config.yaml")

tms credentials -code:"test" -email:"test@example.com"
$Credentials = tms credentials -print -json | ConvertFrom-Json
if ($Credentials.code -ne "test") {
    Write-Error "The credentials code should be 'test'."
}
if ($Credentials.email -ne "test@example.com") {
    Write-Error "The credentials email should be 'test@example.com'."
}

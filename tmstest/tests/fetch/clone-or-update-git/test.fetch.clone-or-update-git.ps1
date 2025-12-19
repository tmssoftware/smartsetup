# Check cases where the product has to be cloned new, and when it can be updated.

. test.setup
tms server-enable community
tms server-enable tms false

$fetchOutput = tms fetch taurustls_developers.taurustls -verbose
$testOk = Test-Result -CommandResult $fetchOutput -Message "*Cloning into*taurustls_developers.taurustls*"
if (-not ($testOk)) {
    Write-Error "The fetch output should indicate that the product is being cloned. Actual message: $($fetchOutput)"
}

$fetchOutput = tms fetch taurustls_developers.taurustls:1.0.0.36 -verbose
$testOk = -not (Test-Result -CommandResult $fetchOutput -Message "*Cloning*")
if (-not ($testOk)) {
    Write-Error "It should only update, not clone. Actual message: $($fetchOutput)"
}
$testOk = Test-Result -CommandResult $fetchOutput -Message "*pull --all*"
if (-not ($testOk)) {   
    Write-Error "It should pull. Actual message: $($fetchOutput)"
}

Remove-Item -Recurse -Force "Products/taurustls_developers.taurustls/src"
mkdir "Products/taurustls_developers.taurustls/src"
$fetchOutput = tms fetch taurustls_developers.taurustls -verbose
$testOk = Test-Result -CommandResult $fetchOutput -Message "*Cloning into*taurustls_developers.taurustls*"
if (-not ($testOk)) {
    Write-Error "Since the src folder was missing, it should re-clone. Actual message: $($fetchOutput)"
}

$fetchOutput = tms fetch taurustls_developers.taurustls -verbose
$testOk = -not (Test-Result -CommandResult $fetchOutput -Message "*Cloning*")
if (-not ($testOk)) {
    Write-Error "It should only update, not clone. Actual message: $($fetchOutput)"
}

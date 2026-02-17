# Check cases where the product has to be cloned new, and when it can be updated.

. test.setup
tms server-enable community
tms server-enable tms false

$fetchOutput = tms fetch zeos.zeoslib -verbose
$testOk = Test-Result -CommandResult $fetchOutput -Message "*--non-interactive checkout*"
if (-not ($testOk)) {
    Write-Error "The fetch output should indicate that the product is being cloned. Actual message: $($fetchOutput)"
}

$fetchOutput = tms fetch zeos.zeoslib -verbose
$testOk = -not (Test-Result -CommandResult $fetchOutput -Message "*--non-interactive checkout*")
if (-not ($testOk)) {
    Write-Error "It should only update, not clone. Actual message: $($fetchOutput)"
}
$testOk = Test-Result -CommandResult $fetchOutput -Message "*--non-interactive update*"
if (-not ($testOk)) {   
    Write-Error "It should pull. Actual message: $($fetchOutput)"
}

Remove-Item -Recurse -Force "Products/zeos.zeoslib/src"
mkdir "Products/zeos.zeoslib/src"
$fetchOutput = tms fetch zeos.zeoslib -verbose
$testOk = Test-Result -CommandResult $fetchOutput -Message "*--non-interactive checkout*"
if (-not ($testOk)) {
    Write-Error "Since the src folder was missing, it should re-clone. Actual message: $($fetchOutput)"
}

$fetchOutput = tms fetch zeos.zeoslib -verbose
$testOk = -not (Test-Result -CommandResult $fetchOutput -Message "*--non-interactive checkout*")
if (-not ($testOk)) {
    Write-Error "It should only update, not clone. Actual message: $($fetchOutput)"
}

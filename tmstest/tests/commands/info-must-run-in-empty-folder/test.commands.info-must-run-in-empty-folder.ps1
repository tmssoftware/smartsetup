# Makes sure we can't run tms info in an empty folder.

. test.setup

Remove-Item ".\tms.config.yaml" -Force
tms info
$info = tms info -json | ConvertFrom-Json

if ($info."folder initialized") {
    Write-Error "Test failed: Folder should not be initialized."
}

if ($info."has credentials") {
    Write-Error "Test failed: Folder should not have credentials."
}

tmscredentials

tms info
$info = tms info -json | ConvertFrom-Json

if (-not $info."folder initialized") {
    Write-Error "Test failed: Folder should be initialized."
}

if (-not $info."has credentials") {
    Write-Error "Test failed: Folder should have credentials."
}

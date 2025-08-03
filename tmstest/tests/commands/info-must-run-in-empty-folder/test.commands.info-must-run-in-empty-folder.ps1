# Makes sure we can't run tms info in an empty folder.

. test.setup

tms info
$info = tms info -json | ConvertFrom-Json

if ($info."folder initialized") {
    Write-Error "Test failed: Folder should not be initialized."
}

if ($info."has credentials") {
    Write-Error "Test failed: Folder should not have credentials."
}
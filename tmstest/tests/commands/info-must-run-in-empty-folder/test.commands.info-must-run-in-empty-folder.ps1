# Makes sure we can't run tms info in an empty folder.

. test.setup

tms info
$info = tms info -json | ConvertFrom-Json

if ($info."folder initialized") {
    Write-Output "Test failed: Folder should not be initialized."
    exit 1
}

if ($info."has credentials") {
    Write-Output "Test failed: Folder should not have credentials."
    exit 1
}
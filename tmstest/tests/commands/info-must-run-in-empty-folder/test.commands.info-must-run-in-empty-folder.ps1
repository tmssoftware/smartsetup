# Makes sure we can't run tms info in an empty folder.

. test.setup

Remove-Item ".\tms.config.yaml" -Force

#Folder is not initialized, it has the logs. 
tms info
$info = tms info -json -test-credentials-profile:temstest.info-test | ConvertFrom-Json

if ($info."folder initialized") {
    Write-Error "Test failed: Folder should not be initialized."
}

if ($info."has credentials") {
    Write-Error "Test failed: Folder should not have credentials."
}

# if there is a tms.exe file, the folder is considered initialized.
New-Item -Path ".\tms.exe" -ItemType File -Force | Out-Null

tms info
$info = tms info -json | ConvertFrom-Json

if (-not $info."folder initialized") {
    Write-Error "Test failed: Folder should be initialized."
}

if (-not $info."has credentials") {
    Write-Error "Test failed: Folder should have credentials."
}

mkdir ".\subfolder" | Out-Null
Set-Location ".\subfolder"  
tms info
$info = tms info -json | ConvertFrom-Json

if (-not $info."folder initialized") {
    Write-Error "Test failed: Folder should be initialized."
}

if (-not $info."has credentials") {
    Write-Error "Test failed: Folder should have credentials."
}

Set-Location ".."

#check that a folder with tms.config.yaml is considered initialized
mkdir ".\subfolder2" | Out-Null
Set-Location ".\subfolder2"
tms config -print

$info = tms info -json | ConvertFrom-Json

if (-not $info."folder initialized") {
    Write-Error "Test failed: Folder should be initialized."
}

if (-not $info."has credentials") {
    Write-Error "Test failed: Folder should have credentials."
}
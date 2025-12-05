# When a fetch fails but others don't we should stop, not build. both community and tms servers.

. test.setup
tmscredentials
$buildResult = Invoke-WithExitCodeIgnored{ tms install tms.vcl.crypto tms.this.product.doesnotexist }
$testOk = Test-Result -CommandResult $buildResult -Message "*Error: Could not find any products matching tms.this.product.doesnotexist*"

if (-not ($testOk)) {
    Write-Error "The error message should mention 'Error: Could not find any products matching tms.this.product.doesnotexist'. Actual message: $($buildResult)"
}
$installed = tms list -json | ConvertFrom-Json -AsHashtable
if ($installed.Count -ne 0) {
    Write-Error "No products should have been installed, but $($installed.Count) were."
}

# Create a download folder
tms install tms.vcl.crypto

# Change the Download folder to be readonly so fetch fails.
$downloadFolder = "./Downloads"
#get the full path
$downloadFolder = (Get-Item -Path $downloadFolder).FullName

if (-not (Test-Path -Path $downloadFolder)) {
    Write-Error "Download folder $downloadFolder does not exist."
}

# replace all files in the download folder by zero-byte files to simulate a corrupted download
Get-ChildItem -Path $downloadFolder -Recurse | ForEach-Object {
    if (-not ($_.PSIsContainer)) {
        Set-Content -Path $_.FullName -Value $null
        #make it readonly
        Set-ItemProperty -Path $_.FullName -Name IsReadOnly -Value $true
    }
}

$buildResult2 = Invoke-WithExitCodeIgnored{ tms install tms.vcl.crypto }

# make the files writable again for cleanup
Get-ChildItem -Path $downloadFolder -Recurse | ForEach-Object {
    if (-not ($_.PSIsContainer)) {
        Set-ItemProperty -Path $_.FullName -Name IsReadOnly -Value $false
    }
}

$testOk2 = Test-Result -CommandResult $buildResult2 -Message "*Error: Could not fetch tms.vcl.crypto*"  
if (-not ($testOk2)) {
    Write-Error "The error message should mention 'Error: Could not fetch tms.vcl.crypto'. Actual message: $($buildResult2)"
}
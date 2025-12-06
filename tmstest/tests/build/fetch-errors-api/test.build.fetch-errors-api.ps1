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

# Change the Download folder to be readonly so fetch fails.
$downloadFolder = "./Downloads"
#get the full path

# Create the download folder if it does not exist
if (-not (Test-Path -Path $downloadFolder)) {
    New-Item -Path $downloadFolder -ItemType Directory | Out-Null
}
$downloadFolder = (Get-Item -Path $downloadFolder).FullName

#Change the ACL permissions for Authenticated Users to deny write
$acl = Get-Acl -Path $downloadFolder
$accessRule = New-Object System.Security.AccessControl.FileSystemAccessRule("Authenticated Users", "Write", "Deny")
$acl.AddAccessRule($accessRule)
Set-Acl -Path $downloadFolder -AclObject $acl

$buildResult2 = ""
try {
    $buildResult2 = Invoke-WithExitCodeIgnored { tms install tms.vcl.crypto }
}
finally {    
    # make the files writable again for cleanup
    $acl.RemoveAccessRule($accessRule)
    Set-Acl -Path $downloadFolder -AclObject $acl
}

Test-FetchResultCounts -FetchResult $buildResult2 -ExpectedLines @(
    "*- tms.vcl.crypto -> FAILED*"
)
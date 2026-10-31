# restoring a manual backup made by zipping stuff. no snapshot is present in the zip, only files.

. test.setup

tms fetch tms.flexcel.vcl tms.webcore
uninstall_and_check(0)


$zipFile = Join-Path -Path $PWD -ChildPath "repo.zip"
if (Test-Path -Path $zipFile) {
    Remove-Item -Path $zipFile
}
Add-Type -AssemblyName System.IO.Compression.FileSystem
$downloads =  Join-Path -Path $PWD -ChildPath "Downloads"
[System.IO.Compression.ZipFile]::CreateFromDirectory($downloads, $zipFile);

$result = tms restore "$zipFile" -skip-register -test-offline
Test-BuildResultCounts -BuildResult $result -expectedNotModifiedCount 0 -expectedIgnoreCount 0 -expectedOkCount 2

$result = tms list -json | ConvertFrom-Json -AsHashtable
if ($result.Count -ne 2) {
    throw "There should be 2 products in the list, but there are $($result.Count)."
}

tms install tms.biz.aurelius

tms snapshot tms.snapshot.yaml

$snapshotFile = Join-Path -Path $PWD -ChildPath "tms.snapshot.yaml"
$zip = [System.IO.Compression.ZipFile]::Open($zipFile, [System.IO.Compression.ZipArchiveMode]::Update)
try {
    [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip, $snapshotFile, "tms.snapshot.yaml") | Out-Null
}
finally {
    $zip.Dispose()
}

tms uninstall *

$result = tms restore "$zipFile" -skip-register 
Test-BuildResultCounts -BuildResult $result -expectedNotModifiedCount 0 -expectedIgnoreCount 0 -expectedOkCount 4

$expectedLines = @(
    'Restoring "CurrentVersions/tms.flexcel.vcl_production_',
    'Restoring "CurrentVersions/tms.webcore_production_',
    '- tms.biz.aurelius -> UPDATED',
    '- tms.biz.bcl -> UPDATED'
)
$resultText = $result -join "`n"
foreach ($expected in $expectedLines) {
    if (-not $resultText.Contains($expected)) {
        throw "Expected the restore output to contain '$expected', but it doesn't."
    }
}

#check to install over existing files.
$result = tms restore "$zipFile" -skip-register 
#backups always delete the folder and get it back, removing all dcus and requiring a rebuild
#the products fetched won't install if they are already installed.
Test-BuildResultCounts -BuildResult $result -expectedNotModifiedCount 2 -expectedIgnoreCount 0 -expectedOkCount 2

$expectedLines = @(
    'Restoring "CurrentVersions/tms.flexcel.vcl_production_',
    'Restoring "CurrentVersions/tms.webcore_production_',
    '- tms.biz.aurelius -> SKIPPED',
    '- tms.biz.bcl -> SKIPPED'

)
$resultText = $result -join "`n"
foreach ($expected in $expectedLines) {
    if (-not $resultText.Contains($expected)) {
        throw "Expected the restore output to contain '$expected', but it doesn't."
    }
}

$result = tms update
Test-BuildResultCounts -BuildResult $result -expectedNotModifiedCount 2 -expectedIgnoreCount 0 -expectedOkCount 2
$result = tms update
Test-BuildResultCounts -BuildResult $result -expectedNotModifiedCount 4 -expectedIgnoreCount 0 -expectedOkCount 0


#offline should fail here, because of the products in the snapshot, but not in the backup.
$result = Invoke-WithExitCodeIgnored {tms restore "$zipFile" -skip-register -test-offline}
$expectedLines = @(
    'Restoring "CurrentVersions/tms.flexcel.vcl_production_',
    'Restoring "CurrentVersions/tms.webcore_production_',
    'Offline mode, no connection to internet.'
)
$resultText = $result -join "`n"
foreach ($expected in $expectedLines) {
    if (-not $resultText.Contains($expected)) {
        throw "Expected the restore output to contain '$expected', but it doesn't."
    }
}



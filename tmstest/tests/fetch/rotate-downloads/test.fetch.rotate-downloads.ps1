# See that we handle backups of files correctly. We will set "versions to keep" to 4, and fetch 6 different versions,
# verifying that only the 4 most recent versions are kept, and in the correct order. Also only the last downloaded version
# should be in the CurrentVersions folder.

Import-Module -Name $tmsTestRootDir/util/util.fetch-testing.ps1 -Force

. test.setup
tmscredentials

tms config-write -p:"tms smart setup options:versions to keep=4"

tms fetch tms.biz.aurelius:5.22.0.0 
Test-CachedFiles -Product tms.biz.aurelius -WasCached $false
Test-DownloadFolders -Product tms.biz.aurelius -CurrentDownloads @("5.22.0.0") -OldDownloads @()

tms fetch tms.biz.aurelius:5.21
Test-CachedFiles -Product tms.biz.aurelius -WasCached $false
Test-DownloadFolders -Product tms.biz.aurelius -CurrentDownloads @("5.21.0.0") -OldDownloads @("5.22.0.0")

tms fetch tms.biz.aurelius:5.16
Test-CachedFiles -Product tms.biz.aurelius -WasCached $false
Test-DownloadFolders -Product tms.biz.aurelius -CurrentDownloads @("5.16.0.0") -OldDownloads @("5.22.0.0", "5.21.0.0")

tms fetch tms.biz.aurelius:5.20.0.1
Test-CachedFiles -Product tms.biz.aurelius -WasCached $false
Test-DownloadFolders -Product tms.biz.aurelius -CurrentDownloads @("5.20.0.1") -OldDownloads @("5.22.0.0", "5.21.0.0", "5.16.0.0")

$versions = tms versions-remote tms.biz.aurelius -json | ConvertFrom-Json
$sortedVersions = $versions.psobject.Properties.Name | Sort-Object {[version]$_}
$lastVersion = $sortedVersions[-1]

tms update
Test-CachedFiles -Product tms.biz.aurelius -WasCached $false
Test-DownloadFolders -Product tms.biz.aurelius -CurrentDownloads @("$lastVersion") -OldDownloads @("5.22.0.0", "5.21.0.0", "5.20.0.1")

tms fetch tms.biz.aurelius:5.21
Test-CachedFiles -Product tms.biz.aurelius -WasCached $true
Test-DownloadFolders -Product tms.biz.aurelius -CurrentDownloads @("5.21.0.0") -OldDownloads @("$lastVersion", "5.22.0.0", "5.20.0.1")

tms fetch tms.biz.aurelius:5.22
Test-CachedFiles -Product tms.biz.aurelius -WasCached $true
Test-DownloadFolders -Product tms.biz.aurelius -CurrentDownloads @("5.22.0.0") -OldDownloads @("$lastVersion", "5.21.0.0", "5.20.0.1")

tms fetch tms.biz.aurelius:5.17
Test-CachedFiles -Product tms.biz.aurelius -WasCached $false
Test-DownloadFolders -Product tms.biz.aurelius -CurrentDownloads @("5.17.0.0") -OldDownloads @("$lastVersion", "5.22.0.0", "5.21.0.0")

tms fetch tms.biz.aurelius:5.22
Test-CachedFiles -Product tms.biz.aurelius -WasCached $true
Test-DownloadFolders -Product tms.biz.aurelius -CurrentDownloads @("5.22.0.0") -OldDownloads @("$lastVersion", "5.21.0.0", "5.17.0.0")

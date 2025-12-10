# Test that having "versions to keep" = 0 doesn't crash, and it removes all downloads
Import-Module -Name $tmsTestRootDir/util/util.fetch-testing.ps1 -Force
. test.setup
tmscredentials

tms config-write -p:"tms smart setup options:versions to keep=0"
tms fetch tms.biz.bcl
Test-CachedFiles -Product tms.biz.bcl -WasCached $false
Test-DownloadFolders -Product tms.biz.bcl -CurrentDownloads @() -OldDownloads @()

tms fetch tms.biz.bcl:1.43.0.1
Test-CachedFiles -Product tms.biz.bcl -WasCached $false
Test-DownloadFolders -Product tms.biz.bcl -CurrentDownloads @() -OldDownloads @()

tms fetch tms.biz.bcl:1.43.0.3
Test-CachedFiles -Product tms.biz.bcl -WasCached $false
Test-DownloadFolders -Product tms.biz.bcl -CurrentDownloads @() -OldDownloads @()

tms fetch tms.biz.bcl:1.43.0.2
Test-CachedFiles -Product tms.biz.bcl -WasCached $false
Test-DownloadFolders -Product tms.biz.bcl -CurrentDownloads @() -OldDownloads @()

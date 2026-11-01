# Test that symlinks inside zip and tar.zst bundles are never extracted as real links,
# and that a link to ..\evil doesn't make us delete or write anything outside the product folder.
# We run tms 4 folders deep inside the test folder, so if something follows the link, it can't escape the test.
# The bundles in .zips are created by .zips/make-archives.py

. test.setup
Import-Module -Name $tmsTestRootDir/util/util.repo.psm1 -Force
Import-Module -Name $tmsTestRootDir/util/util.symlinks.psm1 -Force

function Publish-Bundle {
    param([string]$Bundle)
    Copy-Item -Path "$zips/tmsbuild-template.yaml" -Destination "$zips/tmsbuild.yaml" -Force
    Update-TmsbuildYamlWithLocalUrl -TmsbuildFilePath "$zips/tmsbuild.yaml" -fileName "/$Bundle"
    if (Test-Path -Path $serverZip) {
        Remove-Item -Path $serverZip -Force
    }
    $zip = [System.IO.Compression.ZipFile]::Open($serverZip, [System.IO.Compression.ZipArchiveMode]::Create)
    try {
        [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip, "$zips/tmsbuild.yaml", "tmsbuild.yaml") | Out-Null
    } finally {
        $zip.Dispose()
    }
}

$testRoot = (Get-Location).Path
$zips = Join-Path -Path $testRoot -ChildPath ".zips"
$serverZip = Join-Path -Path $zips -ChildPath "repo.zip"

$workFolder = Initialize-DeepWorkFolder -Root $testRoot
Set-Location -Path $workFolder
$productFolder = Join-Path -Path $workFolder -ChildPath "Products/tmstest.evil"

tms server-enable tms false
Publish-Bundle -Bundle "evil_symlink.zip"
tms server-add tmstest.evil zipfile "file:///$($serverZip -replace '\\', '/')"

foreach ($bundle in @("evil_symlink.zip", "evil_symlink.tar.zst")) {
    Write-Output "=== Testing $bundle ==="
    Publish-Bundle -Bundle $bundle

    $install = Invoke-TmsAllowingFailure install tmstest.evil
    Write-Output $install.Output
    Write-Output "Install exit code: $($install.ExitCode)"
    Assert-CanariesIntact -Root $testRoot -Stage "Install from $bundle"
    if (Test-Path -Path $productFolder) {
        Assert-NoReparsePoints -Path $productFolder
    }

    $uninstall = Invoke-TmsAllowingFailure uninstall tmstest.evil
    Write-Output $uninstall.Output
    Write-Output "Uninstall exit code: $($uninstall.ExitCode)"
    Assert-CanariesIntact -Root $testRoot -Stage "Uninstall after $bundle"
    Assert-FolderRemoved -Path $productFolder -Stage "Uninstall after $bundle"
}

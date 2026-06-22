#If we clean up the output folder before making a git pull, we will also erase dcus.
#So it will have to rebuild the package, even if there are no updates. This test checks that we don't do that.

. test.setup
Import-Module -Name $tmsTestRootDir/util/util.repo.psm1 -Force

function Verify-Version {
    param (
        [array]$buildResult,
        [string]$expectedVersion,
        [int] $notModifiedCount,
        [int] $okCount,
        [int] $ignoreCount  
    )
    Test-BuildResultCounts -buildResult $buildResult -expectedNotModifiedCount:$notModifiedCount -expectedIgnoreCount:$ignoreCount -expectedOkCount:$okCount

    $version = ./app/Win32/Release/versioner.exe

    if ($version -ne $expectedVersion) {
        throw "Version should be $expectedVersion, but was $version"
    }
}

function Modify-Pas {
    param (
        [string]$pasFilePath,
        [string]$version
    )
    if ($version -eq "1.0") {
        return
    }
    $content = Get-Content -Path $pasFilePath
    $newContent = $content -replace "Result := .*$", "Result := '$version'"
    Set-Content -Path $pasFilePath -Value $newContent
}

function Create-GitRepos {
    Push-Location "__updated-version"

    $tmsbuildFiles = Get-ChildItem -Path $PWD -Filter 'tmsbuild.yaml' -Recurse -File
    foreach ($tmsbuildFile in $tmsbuildFiles) {
        Update-TmsbuildYamlWithLocalUrl -TmsbuildFilePath $tmsbuildFile.FullName
    }

    # Create the initial repo
    git init | Out-Null
    foreach ($version in "1.0", "2.0", "3.0") {
        Set-Content -Path (Join-Path -Path $PWD -ChildPath "version.txt") -Value $version
        Modify-Pas (Join-Path -Path $PWD -ChildPath "src/IFace.pas") -Version $version
        git add -A | Out-Null
        git commit -m "Add version $version" | Out-Null
        git tag $version | Out-Null
    }

    # Zip the updated repo
    Pop-Location
    $zipFile = Join-Path -Path $PWD -ChildPath "repo.zip"
    if (Test-Path -Path $zipFile) {
        Remove-Item -Path $zipFile
    }
    Add-Type -AssemblyName System.IO.Compression.FileSystem

    $zip = [System.IO.Compression.ZipFile]::Open($zipFile, [System.IO.Compression.ZipArchiveMode]::Create)
    try {
        [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip, (Join-Path -Path $PWD -ChildPath "__updated-version\tmsbuild.yaml"), "tmsbuild.yaml") | Out-Null
    } finally {
        $zip.Dispose()
    }   
}

Create-GitRepos
tms config-write -p:"tms smart setup options:error if skipped=false"

tms server-enable tms false
tms server-enable community false
$PWDFWD = $PWD.Path.Replace('\', '/')
tms server-add testserver zipfile "file:///$($PWDFWD)/repo.zip"

$result = tms install test.data.updater:1.0
Verify-Version $result "1.0" 0 2 0
$result = tms update
Verify-Version $result "3.0" 0 2 0
$result = tms update
Verify-Version $result "3.0" 2 0 0
$result = tms update
Verify-Version $result "3.0" 2 0 0
$result = tms install test.data.updater:2.0
Verify-Version $result "2.0" 0 2 0
$result = tms update
Verify-Version $result "3.0" 0 2 0
$result = tms update
Verify-Version $result "3.0" 2 0 0

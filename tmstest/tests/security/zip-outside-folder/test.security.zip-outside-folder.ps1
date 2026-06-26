#Test that we can't extract files outside the target folder

. test.setup
Import-Module -Name $tmsTestRootDir/util/util.repo.psm1 -Force


tms server-enable tms false
Copy-Item -Path "$PWD/.zips/tmsbuild-template.yaml" -Destination "$PWD/.zips/tmsbuild.yaml" -Force
Update-TmsbuildYamlWithLocalUrl -TmsbuildFilePath ./.zips/tmsbuild.yaml -fileName "/evil_ok.zip"
$zip = [System.IO.Compression.ZipFile]::Open("$PWD/.zips/repo_ok.zip", [System.IO.Compression.ZipArchiveMode]::Create)
try {
    [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip, (Join-Path -Path $PWD -ChildPath ".zips/tmsbuild.yaml"), "tmsbuild.yaml") | Out-Null
} finally {
    $zip.Dispose()
}   


tms server-add tmstest.evil zipfile "file:///$($PWD -replace '\\', '/')/.zips/repo_ok.zip"

tms install tmstest.evil
tms uninstall tmstest.evil

Copy-Item -Path "$PWD/.zips/tmsbuild-template.yaml" -Destination "$PWD/.zips/tmsbuild.yaml" -Force
Update-TmsbuildYamlWithLocalUrl -TmsbuildFilePath ./.zips/tmsbuild.yaml -fileName "/evil_ko.zip"

Remove-Item -Path "$PWD/.zips/repo_ok.zip" -Force
$zip = [System.IO.Compression.ZipFile]::Open("$PWD/.zips/repo_ok.zip", [System.IO.Compression.ZipArchiveMode]::Create)
try {
    [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip, (Join-Path -Path $PWD -ChildPath ".zips/tmsbuild.yaml"), "tmsbuild.yaml") | Out-Null
} finally {
    $zip.Dispose()
} 

Test-CommandFails { tms install tmstest.evil} "*is outside the extract folder*"

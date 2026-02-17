# Tests that when we don't have a tmsbuild.yaml in the local repo, we can still fetch it from the server.
# Also checks that the tmsbuild.yaml from the server will be updated when we update the server.
# Because as the local tmsbuild.yaml has higher priority than the server one, if we have a local one,
# we will never fetch the newer one from the server. Once we fetch the first tmsbuild.yaml from the server,
# we will always use that one, even if there is a newer one in the server.


function Update-ServerZip {
    param (
        [string]$CurrentFolder
    )
    
    if (Test-Path -Path "$CurrentFolder/tmsbuild_test_repos.zip") {
        Remove-Item -Path "$CurrentFolder/tmsbuild_test_repos.zip" -Force
    }
    Compress-Archive -Path "$CurrentFolder/_server/*" -DestinationPath "$CurrentFolder/tmsbuild_test_repos.zip" -Force
}

. test.setup
tms server-enable tms false

Copy-Item -Path (Join-Path -Path $tmsTestRootDir -ChildPath "tmp-run/test-repos/tmsbuild_test_repos.zip") -Destination "." -Force
# extract the zip above to the "server" folder
Expand-Archive -Path "tmsbuild_test_repos.zip" -DestinationPath "_server" -Force
$currentFolder = Get-Location
foreach ($file in Get-ChildItem -Path "$currentFolder\_server" -Recurse -Filter "tmsbuild.yaml") {
    $content = (Get-Content -Path $file.FullName -Raw) 
    $content = $content + "`npackage options:`n  ignore dproj platforms: true"
    $content | Set-Content -Path $file.FullName
}
Update-ServerZip -CurrentFolder $currentFolder


mkdir .\sources
Set-Location -Path .\sources
Move-Item -Path "../tms.config.yaml" -Destination "." -Force
tms server-add testserver zipfile "file:///$($currentFolder.ToString().Replace('\', '/'))/tmsbuild_test_repos.zip"
tms config-write -p:"configuration for all products:platforms=[win32intel, win64intel]"

# B has tmsbuild.yaml in the local repo. F and E do not have it locally, so they will be fetched from the server.
tms install tmstest.b
compare-files_diff "./Products/tmstest.b/src/tmsbuild.yaml" (Join-Path -Path "../_server" -ChildPath "B-CDE/tmsbuild.yaml")  
compare-files "./Products/tmstest.e/src/tmsbuild.yaml" (Join-Path -Path "../_server" -ChildPath "E-F/tmsbuild.yaml")  
compare-files "./Products/tmstest.f/src/tmsbuild.yaml" (Join-Path -Path "../_server" -ChildPath "F/tmsbuild.yaml")

# change all the tmsbuild.yaml files in the server folder so the line platform: and its child "all" are changed to win32
foreach ($file in Get-ChildItem -Path "$currentFolder\_server" -Recurse -Filter "tmsbuild.yaml") {
    $content = (Get-Content -Path $file.FullName -Raw) 
    $content = $content -replace "platforms:\s*- all", "platforms: `n      - win32intel"
    $content | Set-Content -Path $file.FullName
}

Update-ServerZip -CurrentFolder $currentFolder

$result = tms install tmstest.b
$summary = $result | tmsutil summary-to-json | ConvertFrom-Json -AsHashtable
$summary["B"] | Assert-ValueIs "OK."
$summary["E"] | Assert-ValueIs "OK."
$summary["F"] | Assert-ValueIs "OK."
$summary["C"] | Assert-ValueIs "NOT MODIFIED."
$summary["D"] | Assert-ValueIs "NOT MODIFIED."
if (-not (Test-Result -CommandResult $result -Message "*- win64intel -> UNINSTALLED.*")) {
    throw "win64intel should have been uninstalled for E and F."
}


compare-files_diff "./Products/tmstest.b/src/tmsbuild.yaml" (Join-Path -Path "../_server" -ChildPath "B-CDE/tmsbuild.yaml")  
compare-files "./Products/tmstest.e/src/tmsbuild.yaml" (Join-Path -Path "../_server" -ChildPath "E-F/tmsbuild.yaml")  
compare-files "./Products/tmstest.f/src/tmsbuild.yaml" (Join-Path -Path "../_server" -ChildPath "F/tmsbuild.yaml")
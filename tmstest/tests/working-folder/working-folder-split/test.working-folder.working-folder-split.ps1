# If the working folder is different from the folder where tms.config.yaml is,
# we need to search for tmsbuild.yaml to build in both the working folder and
# the folder where tms.config.yaml is.

. test.setup
$workingFolder = Join-Path -Path $env:TEMP -ChildPath "tmstest-tmp" -AdditionalChildPath "tms-working-folder-split-test"
if (Test-Path -Path $workingFolder) {
  Remove-Item -Path $workingFolder -Recurse -Force
}
tms config-write -p:"tms smart setup options:working folder=$workingFolder"
tms server-enable tms false
tms server-enable community

tms install jam.virtualtreeview
tms build

$versions = tms config-read "configuration for all products:delphi versions" -json | ConvertFrom-Json 

$allInstalled = tms list -json -detailed | ConvertFrom-Json -AsHashtable
    
foreach ($version in $versions) {
    foreach ($installed in $allInstalled) {
        if (-not $installed.values.ides.$version.platforms.win32intel.built) {
            Write-Error "Product $($installed.name) was not built correctly for Delphi $version."
        }
    }
}

# There should have been 2 products installed
if ($allInstalled.Count -ne 2) {
    Write-Error "Expected 2 products to be installed, but found $($allInstalled.Count)."
}

Remove-Item -Path $workingFolder -Recurse -Force

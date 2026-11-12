# Testing what happens when tms.config.yaml is not in the current folder.

. test.setup
tms server-enable tms false
tms server-enable community

New-Item -ItemType Directory -Path "cfg" -Force | Out-Null
Move-Item -Path "tms.config.yaml" -Destination (Join-Path -Path ./cfg -ChildPath "tms.config.yaml")

tms install jam.virtualtreeview -config:"cfg/tms.config.yaml"
tms build -config:"cfg/tms.config.yaml"

$versions = tms config-read "configuration for all products:delphi versions" -json -config:"cfg/tms.config.yaml" | ConvertFrom-Json 

$allInstalled = tms list -json -detailed -config:"cfg/tms.config.yaml" | ConvertFrom-Json -AsHashtable
    
foreach ($version in $versions) {
    foreach ($installed in $allInstalled) {
        if (-not $installed.values.ides.$version.platforms.win32intel.built) {
            Write-Error "Product $($installed.name) was not built correctly for Delphi $version."
        }
    }
}

# There should have been 1 products installed
if ($allInstalled.Count -ne 1) {
    Write-Error "Expected 1 products to be installed, but found $($allInstalled.Count)."
}

# The cfg folder should only contain tms.config.yaml
$cfgItems = @(Get-ChildItem -Path ./cfg -Force)
if ($cfgItems.Count -ne 5 -or ($cfgItems.Where({$_.Name -eq "tms.config.yaml"}).Count -ne 1) -or ($cfgItems.Where({$_.Name -eq ".tmssetup"}).Count -ne 1)) {
    Write-Error "Expected cfg folder to contain tms.config.yaml and working folders for tms, but found: $(($cfgItems | ForEach-Object { $_.Name }) -join ', ')"
}



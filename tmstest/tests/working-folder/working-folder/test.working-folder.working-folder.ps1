# This is a basic test of working folder.
# As we can't assume a fixed disk like r:\ existing, we will build in %temp%
# that hopefully is in a different drive. To do a more thorough test, run tmstest -working-folder

. test.setup
$workingFolder = Join-Path -Path $env:TEMP -ChildPath "tmstest-tmp" -AdditionalChildPath "tms-working-folder-test"
if (Test-Path -Path $workingFolder) {
  Remove-Item -Path $workingFolder -Recurse -Force
}
tms config-write -p:"tms smart setup options:working folder=$workingFolder"
tmscredentials
tms server-enable community

$info = tms info -json | ConvertFrom-Json
if ($info."working folder" -ne $workingFolder) {
    Write-Error "The working folder is not set correctly. Expected: '$workingFolder', Found: '$($info.workingFolder)'."
}

tms install tms.vcl.crypto vsoft.commandline tms.biz.aurelius
$installed = tms list -json | ConvertFrom-Json -AsHashtable
$expectedCount = 4
if ($installed.Count -ne $expectedCount) {
    Write-Error "Expected $expectedCount products to be installed, but found $($installed.Count)."
}

# Check if the .tmssetup folder was created in the working folder
$setupFolder = Join-Path -Path $workingFolder -ChildPath ".tmssetup"
if (-not (Test-Path -Path $setupFolder)) {
    Write-Error "The .tmssetup folder was not created in the working folder '$workingFolder'."
}
# Check if the Logs folder was created in the working folder
$logsFolder = Join-Path -Path $workingFolder -ChildPath "Logs"
if (-not (Test-Path -Path $logsFolder)) {
    Write-Error "The Logs folder was not created in the working folder '$workingFolder'."
}

# Check if the Products folder was created in the working folder
$productsFolder = Join-Path -Path $workingFolder -ChildPath "Products"
if (-not (Test-Path -Path $productsFolder)) {
    Write-Error "The Products folder was not created in the working folder '$workingFolder'."
}

# Check if the Temp folder was created in the working folder
$tempFolder = Join-Path -Path $workingFolder -ChildPath "Temp"
if (-not (Test-Path -Path $tempFolder)) {
    Write-Error "The Temp folder was not created in the working folder '$workingFolder'."
}

# Check that 0 folders were created in the our current folder
$currentFolder = Get-Location
$foldersInCurrent = @(Get-ChildItem -Path $currentFolder -Directory)
if ($foldersInCurrent.Count -ne 0) {
    Write-Error "Expected 0 folders to be created in the current folder '$currentFolder', but found $($foldersInCurrent.Count)."
}

# Check that only 3 files were created in our current folder (.tmsconfig.yaml, output.log, output2.log)
$filesInCurrent = @(Get-ChildItem -Path $currentFolder -File)
$expectedFileCount = 3  
if ($filesInCurrent.Count -ne $expectedFileCount) {
    Write-Error "Expected $expectedFileCount files to be created in the current folder '$currentFolder', but found $($filesInCurrent.Count)."
}

tms uninstall tms.vcl.crypto vsoft.commandline tms.biz.aurelius -cascade
$installedAfterUninstall = tms list -json | ConvertFrom-Json -AsHashtable
if ($installedAfterUninstall.Count -ne 0) {
    Write-Error "Expected 0 products to be installed after uninstall, but found $($installedAfterUninstall.Count)."
}

Remove-Item -Path $workingFolder -Recurse -Force

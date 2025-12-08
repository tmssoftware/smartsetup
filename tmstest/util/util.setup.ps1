# This script sets up the test environment for TMS tests.
[CmdletBinding()] param()

. $PSScriptRoot/util.errors.ps1

if (-not $tmsTestRootDir) {
    Write-Error "The variable `tmsTestRootDir` is not set. You need to setup the environment, see ../README.md."
}
$callerPath = $PSCmdlet.MyInvocation.PSScriptRoot
if (-not $callerPath) { 
    Write-Error "The script is not being called from a valid path."
}

# get current folder
$currentFolder = Get-Location
$resolvedItemPath = (Resolve-Path $currentFolder).Path
$callingFolderName = Split-Path -Path $callerPath -Leaf
$resolvedTestsFolder = Join-Path -Path (Resolve-Path $tmsTestRootDir\tmp-run).Path -ChildPath $callingFolderName

if ($resolvedItemPath -ne $resolvedTestsFolder) {
    Write-Error "The test script must be called from $($resolvedTestsFolder) and was called from $($resolvedItemPath). Try running it from tmstest.ps1"
}

. $PSScriptRoot/util.set_starting_config_yaml.ps1

Import-Module -Name $PSScriptRoot/util.modules.psm1 -Force

# set variable to the current folder name
$location = Get-Location
$folderName = Split-Path -Path $location.Path -Leaf


$Global:tmsWorkingFolder = ""
if ($Global:tmsUseWorkingFolder) {
    # if tms.config.yaml doesn't exist, do not create it
    if (Test-Path -Path (Join-Path -Path (Get-Location) -ChildPath "tms.config.yaml")) {
        $Global:tmsWorkingFolder = Join-Path -Path $env:TEMP -ChildPath "tmstest-tmp" -AdditionalChildPath "$($folderName)"
        tms config-write -p:"tms smart setup options:working folder=$Global:tmsWorkingFolder"
        Write-Host "Set working folder to: $Global:tmsWorkingFolder"
    }
}


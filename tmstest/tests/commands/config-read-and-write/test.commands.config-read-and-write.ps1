. test.setup
tms config -reset -print

$buildCores = 0
$altRegistryKey = ""
$workingFolder = ""
$preventSleep = "true"
$versionsToKeep = "-1"
$errorIfSkipped = "false"
$excludedProducts = "[]"
$includedProducts = "[]"
$additionalProductsFolders = "[]"
$tmsServerEnabled = "true"
$communityServerEnabled = "false"
$gitLocation = ""
$gitCloneCommand = ""
$gitPullCommand = ""
$svnLocation = ""
$svnCheckoutCommand = ""
$svnUpdateCommand = ""
$autoSnapshotFilenames = "[]"
$dcuMegafolders = "[]"

$productArray = @("all products", "test1", "test2")

$productOptionsVerbosity = @("info", "", "")
$productOptionsSkipRegister = @("false", "", "")
$productOptionsDryRun = @("false", "", "")

$productDelphiVersions = @("[]", "", "")
$productPlatforms = @("[]", "", "")

$productCompilationOptionsDebugDcus = @("true", "", "")
$productCompilationOptionsDefines = @("[]", "", "")


$productAdvancedOptionsUseSymlinks = @("false", "", "")
$productAdvancedOptionsKeepParallelFolders = @("false", "", "")
$productAdvancedOptionsModifySources = @("false", "", "")
$productAdvancedOptionsPartialBuilds = @("false", "", "")
$productAdvancedOptionsAddSourceCodeToLibraryPath = @("false", "", "")

$productCompilerPathsD11 = @("", "", "")

function Test-JSON() {
    param(
        [Parameter(Mandatory, Position = 0)] [string] $fileName
    )
    tms config-read -json | Out-File -FilePath "$fileName.json" -Encoding utf8
    $schemaPath = Join-Path $PSScriptRoot "..\..\..\..\tms\example-config\tms.config.schema.json"
    ajv test -s $schemaPath -d "$fileName.json" --valid
    compare-files "$fileName.json" "$fileName.expected.json"
}
function Test-ValuesOk() {
    tms config-read "tms smart setup options:build cores" | Assert-ValueIs $buildCores
    tms config-read "tms smart setup options:alternate registry key" | Assert-ValueIs $altRegistryKey
    tms config-read "tms smart setup options:working folder" | Assert-ValueIs $workingFolder
    tms config-read "tms smart setup options:prevent sleep" | Assert-ValueIs $preventSleep
    tms config-read "tms smart setup options:versions to keep" | Assert-ValueIs $versionsToKeep
    tms config-read "tms smart setup options:error if skipped" | Assert-ValueIs $errorIfSkipped
    tms config-read "tms smart setup options:excluded products" | Assert-ValueIs $excludedProducts
    tms config-read "tms smart setup options:included products" | Assert-ValueIs $includedProducts
    tms config-read "tms smart setup options:additional products folders" | Assert-ValueIs $additionalProductsFolders
    tms config-read "tms smart setup options:servers:tms:enabled" | Assert-ValueIs $tmsServerEnabled
    tms config-read "tms smart setup options:servers:community:enabled" | Assert-ValueIs $communityServerEnabled
    tms config-read "tms smart setup options:git:git location" | Assert-ValueIs $gitLocation
    tms config-read "tms smart setup options:git:clone command" | Assert-ValueIs $gitCloneCommand
    tms config-read "tms smart setup options:git:pull command" | Assert-ValueIs $gitPullCommand
    tms config-read "tms smart setup options:svn:svn location" | Assert-ValueIs $svnLocation
    tms config-read "tms smart setup options:svn:checkout command" | Assert-ValueIs $svnCheckoutCommand
    tms config-read "tms smart setup options:svn:update command" | Assert-ValueIs $svnUpdateCommand
    tms config-read "tms smart setup options:dcu megafolders" | Assert-ValueIs $dcuMegafolders
    tms config-read "tms smart setup options:auto snapshot filenames" | Assert-ValueIs $autoSnapshotFilenames

    for ($i = 0; $i -lt $productArray.Count; $i++) {
        tms config-read "configuration for $($productArray[$i]):options:verbosity" | Assert-ValueIs $productOptionsVerbosity[$i]
        tms config-read "configuration for $($productArray[$i]):options:skip register" | Assert-ValueIs $productOptionsSkipRegister[$i]
        tms config-read "configuration for $($productArray[$i]):options:dry run" | Assert-ValueIs $productOptionsDryRun[$i]
        tms config-read "configuration for $($productArray[$i]):delphi versions" | Assert-ValueIs $productDelphiVersions[$i]
        tms config-read "configuration for $($productArray[$i]):platforms" | Assert-ValueIs $productPlatforms[$i]
        tms config-read "configuration for $($productArray[$i]):compilation options:debug dcus" | Assert-ValueIs $productCompilationOptionsDebugDcus[$i]
        tms config-read "configuration for $($productArray[$i]):compilation options:defines" | Assert-ValueIs $productCompilationOptionsDefines[$i]
        tms config-read "configuration for $($productArray[$i]):advanced options:use symlinks" | Assert-ValueIs $productAdvancedOptionsUseSymlinks[$i]
        tms config-read "configuration for $($productArray[$i]):advanced options:keep parallel folders" | Assert-ValueIs $productAdvancedOptionsKeepParallelFolders[$i]
        tms config-read "configuration for $($productArray[$i]):advanced options:modify sources" | Assert-ValueIs $productAdvancedOptionsModifySources[$i]
        tms config-read "configuration for $($productArray[$i]):advanced options:partial builds" | Assert-ValueIs $productAdvancedOptionsPartialBuilds[$i]
        tms config-read "configuration for $($productArray[$i]):advanced options:add source code to library path" | Assert-ValueIs $productAdvancedOptionsAddSourceCodeToLibraryPath[$i]
        tms config-read "configuration for $($productArray[$i]):compiler paths:delphi11" | Assert-ValueIs $productCompilerPathsD11[$i]
    }
}

Test-ValuesOk

$buildCores = 4
tms config-write -p:"tms smart setup options:build cores=$buildCores"
Test-ValuesOk

$altRegistryKey = "myProduct"
tms config-write -p:"tms smart setup options:alternate registry key=$altRegistryKey"
Test-ValuesOk

$workingFolder = "WorkingFolder"
tms config-write -p:"tms smart setup options:working folder=$workingFolder"
Test-ValuesOk

$preventSleep = "false"
tms config-write -p:"tms smart setup options:prevent sleep=$preventSleep"
Test-ValuesOk

$versionsToKeep = "3"
tms config-write -p:"tms smart setup options:versions to keep=$versionsToKeep"
Test-ValuesOk

$errorIfSkipped = "true"
tms config-write -p:"tms smart setup options:error if skipped=$errorIfSkipped"
Test-ValuesOk

$excludedProducts = "[tms-fnc,tms-fnc-cloud]"
tms config-write -p:"tms smart setup options:excluded products=$excludedProducts"
Test-ValuesOk

$includedProducts = "[tms-fnc-cloud]"
tms config-write -p:"tms smart setup options:included products=$includedProducts"
Test-ValuesOk

$additionalProductsFolders = "[..\landgraf\aws-sdk-delphi,..\landgraf\tms-biz,..\tms\delphi-graphql]"
tms config-write -p:"tms smart setup options:additional products folders=$additionalProductsFolders"
Test-ValuesOk

$tmsServerEnabled = "false"
tms config-write -p:"tms smart setup options:servers:tms:enabled=$tmsServerEnabled"
Test-ValuesOk

$communityServerEnabled = "true"
tms config-write -p:"tms smart setup options:servers:community:enabled=$communityServerEnabled"
Test-ValuesOk

$gitLocation = "C:\Program Files\Git\cmd\git.exe"
tms config-write -p:"tms smart setup options:git:git location=$gitLocation"
Test-ValuesOk

$gitCloneCommand = "clone --depth 1"
tms config-write -p:"tms smart setup options:git:clone command=$gitCloneCommand"
Test-ValuesOk

$gitPullCommand = "pull"
tms config-write -p:"tms smart setup options:git:pull command=$gitPullCommand"
Test-ValuesOk

$svnLocation = "C:\Program Files\TortoiseSVN\bin\svn.exe"
tms config-write -p:"tms smart setup options:svn:svn location=$svnLocation"
Test-ValuesOk

$svnCheckoutCommand = "checkout --depth immediates"
tms config-write -p:"tms smart setup options:svn:checkout command=$svnCheckoutCommand"
Test-ValuesOk

$svnUpdateCommand = "update"
tms config-write -p:"tms smart setup options:svn:update command=$svnUpdateCommand"
Test-ValuesOk

$dcuMegafolders = "[tms: '*.tms',none: test,other: '*']"
tms config-write -p:"tms smart setup options:dcu megafolders=$dcuMegafolders"
Test-ValuesOk

tms config-write -p:"tms smart setup options:dcu megafolders=[tms:*.tms, none: test,other:*, all: p]"
$dcuMegafolders = "[tms: '*.tms',none: test,other: '*',all: p]"
Test-ValuesOk

tms config-write -p:"tms smart setup options:auto snapshot filenames=[snapshot1,snapshot2,snapshot3]"
$autoSnapshotFilenames = "[snapshot1,snapshot2,snapshot3]"
Test-ValuesOk

tms config-write -p:"tms smart setup options:add auto snapshot filenames=[snapshot4,snapshot5]"
$autoSnapshotFilenames = "[snapshot1,snapshot2,snapshot3,snapshot4,snapshot5]"
Test-ValuesOk

tms config-write -p:"tms smart setup options:replace auto snapshot filenames=[snapshot1,snapshot2,snapshot3]"
$autoSnapshotFilenames = "[snapshot1,snapshot2,snapshot3]"
Test-ValuesOk


$result = 
Invoke-WithExitCodeIgnored {
    tms config-read "tms smart setup options:build cores2" 
}
$result -join " " | Assert-ValueContains 'Error: "build cores2" is an invalid child section for "root:tms smart setup options"'

$result = 
Invoke-WithExitCodeIgnored {
    tms config-read "configuration for test.product:delphi versions2" 
}
$result -join " " | Assert-ValueContains 'Error: "delphi versions2" is an invalid child section for "root:test.product"'

Test-Json -fileName "tms.config1"

$productOptionsVerbosity = $("info", "trace", "error")
for ($i = 0; $i -lt $productArray.Count; $i++) {
    tms config-write -p:"configuration for $($productArray[$i]):options:verbosity=$($productOptionsVerbosity[$i])"
}
Test-ValuesOk

$productOptionsSkipRegister = $("false", "true", "true")
for ($i = 0; $i -lt $productArray.Count; $i++) {
    tms config-write -p:"configuration for $($productArray[$i]):options:skip register=$($productOptionsSkipRegister[$i])"
}
Test-ValuesOk

$productOptionsDryRun = $("false", "true", "false")
for ($i = 0; $i -lt $productArray.Count; $i++) {
    tms config-write -p:"configuration for $($productArray[$i]):options:dry run=$($productOptionsDryRun[$i])"
}
Test-ValuesOk

$productDelphiVersions = $("[]", "[delphi11]", "[delphixe,delphi12]")
for ($i = 0; $i -lt $productArray.Count; $i++) {
    tms config-write -p:"configuration for $($productArray[$i]):delphi versions=$($productDelphiVersions[$i])"
}
Test-ValuesOk

$productDelphiVersions = $("[delphi13]", "", "[delphixe,delphi12]")
for ($i = 0; $i -lt $productArray.Count; $i++) {
    tms config-write -p:"configuration for $($productArray[$i]):delphi versions=$($productDelphiVersions[$i])"
}
Test-ValuesOk

$productPlatforms = $("[win64xintel]", "[]", "[win32intel,win64intel,android64]")
for ($i = 0; $i -lt $productArray.Count; $i++) {
    tms config-write -p:"configuration for $($productArray[$i]):platforms=$($productPlatforms[$i])"
}
$productPlatforms = $("[win64xintel]", "", "[win32intel,win64intel,android64]")
Test-ValuesOk

$productCompilationOptionsDebugDcus = $("true", "false", "true")
for ($i = 0; $i -lt $productArray.Count; $i++) {
    tms config-write -p:"configuration for $($productArray[$i]):compilation options:debug dcus=$($productCompilationOptionsDebugDcus[$i])"
}
Test-ValuesOk

$productCompilationOptionsDefines = $("[DEBUG]", "[]", "[DEBUG,TEST]")
for ($i = 0; $i -lt $productArray.Count; $i++) {
    tms config-write -p:"configuration for $($productArray[$i]):compilation options:defines=$($productCompilationOptionsDefines[$i])"
}
$productCompilationOptionsDefines = $("[DEBUG]", "", "[DEBUG,TEST]")
Test-ValuesOk

$productAdvancedOptionsUseSymlinks = $("false", "true", "true")
for ($i = 0; $i -lt $productArray.Count; $i++) {
    tms config-write -p:"configuration for $($productArray[$i]):advanced options:use symlinks=$($productAdvancedOptionsUseSymlinks[$i])"
}
Test-ValuesOk

$productAdvancedOptionsUseSymlinks = $("false", "true", "false")
for ($i = 0; $i -lt $productArray.Count; $i++) {
    tms config-write -p:"configuration for $($productArray[$i]):advanced options:use symlinks=$($productAdvancedOptionsUseSymlinks[$i])"
}
Test-ValuesOk

$productAdvancedOptionsKeepParallelFolders = $("false", "true", "true")
for ($i = 0; $i -lt $productArray.Count; $i++) {
    tms config-write -p:"configuration for $($productArray[$i]):advanced options:keep parallel folders=$($productAdvancedOptionsKeepParallelFolders[$i])"
}
Test-ValuesOk

$productAdvancedOptionsModifySources = $("false", "true", "false")
for ($i = 0; $i -lt $productArray.Count; $i++) {
    tms config-write -p:"configuration for $($productArray[$i]):advanced options:modify sources=$($productAdvancedOptionsModifySources[$i])"
}
Test-ValuesOk

$productAdvancedOptionsPartialBuilds = $("true", "true", "false")
for ($i = 0; $i -lt $productArray.Count; $i++) {
    tms config-write -p:"configuration for $($productArray[$i]):advanced options:partial builds=$($productAdvancedOptionsPartialBuilds[$i])"
}
Test-ValuesOk

$productAdvancedOptionsAddSourceCodeToLibraryPath = $("true", "false", "false")
for ($i = 0; $i -lt $productArray.Count; $i++) {
    tms config-write -p:"configuration for $($productArray[$i]):advanced options:add source code to library path=$($productAdvancedOptionsAddSourceCodeToLibraryPath[$i])"
}
Test-ValuesOk

$productCompilerPathsD11 = $("C:\Program Files (x86)\Embarcadero\Studio\22.0\bin", "C:\Delphi11", "")
for ($i = 0; $i -lt $productArray.Count; $i++) {
    tms config-write -p:"configuration for $($productArray[$i]):compiler paths:delphi11=$($productCompilerPathsD11[$i])"
}
Test-ValuesOk

Test-Json -fileName "tms.config2"

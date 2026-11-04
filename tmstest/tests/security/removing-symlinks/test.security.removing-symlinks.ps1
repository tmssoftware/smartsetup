# Check that removing a symlinked folder removes the symlink, not the linked folder.

. test.setup
Import-Module -Name $tmsTestRootDir/util/util.symlinks.psm1 -Force

$testRoot = (Get-Location).Path
$workFolder = Initialize-DeepWorkFolder -Root $testRoot
Set-Location -Path $workFolder

$repo = Join-Path -Path $workFolder -ChildPath "data, repo"
$evilsource = Join-Path -Path $repo -ChildPath "evil"
$eviltarget = Join-Path -Path $workFolder -ChildPath "evil"

    
foreach ($deleteMode in @("folder-deleter", "delete-folder-moving-to-locked", "delete-folder-ignore-locked")) {
    mkdir "$repo"
    # Create a symlink to evil
    New-Item -Path $evilsource -ItemType SymbolicLink -Value $eviltarget

    tms test-delete-folder -mode:$deleteMode -folder:$evilsource
    if (Test-Path -Path $evilsource) {
        throw "$deleteMode should have deleted $evilsource, but it still exists."
    }
    Assert-CanariesIntact -Root $testRoot -Stage "Deleting directly with $deleteMode"

    New-Item -Path "$evilsource" -ItemType SymbolicLink -Value "$eviltarget"
    tms test-delete-folder -mode:$deleteMode -folder:"$repo"
    if (Test-Path -Path "$repo") {
        throw "$deleteMode should have deleted $repo, but it still exists."
    }

        
    Assert-CanariesIntact -Root $testRoot -Stage "Deleting nested with $deleteMode"
}
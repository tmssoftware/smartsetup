# Check that skip submodules property in tmsbuild.yaml works as expected.
. test.setup

tms server-enable tms false
tms server-add testserver zipfile "file:///$($tmsTestRootDir.Replace('\', '/'))/tmp-run/test-repos/tmsbuild_test_repos.zip"

tms config-write -p:"tms smart setup options:git:clone command =  -c protocol.file.allow=always clone"

tms fetch tmstest.submodules_main_no_fetch tmstest.submodules_main_fetch

if (-not (Test-Path -Path ".\Products\tmstest.submodules_main_fetch\src\Submodules_Child")) {
    throw "Expected submodule folder '$(PWD)\Products\tmstest.submodules_main_fetch\src\Submodules_Child' to be created, but it was not found"
}
# folder should not be empty
$isEmpty = $null -eq (Get-ChildItem -Path ".\Products\tmstest.submodules_main_fetch\src\Submodules_Child" -Force | Select-Object -First 1)
if ($isEmpty) {
    throw "Expected submodule folder '$(PWD)\Products\tmstest.submodules_main_fetch\src\Submodules_Child' to contain files, but it is empty"
}
#check that .\Products\tmstest.submodules_main_no_fetch\src\Submodules_Child folder is empty
$isEmpty = $null -eq (Get-ChildItem -Path ".\Products\tmstest.submodules_main_no_fetch\src\Submodules_Child" -Force | Select-Object -First 1)
if (-not $isEmpty) {
    throw "Expected submodule folder '$(PWD)\Products\tmstest.submodules_main_no_fetch\src\Submodules_Child' to be empty, but it contains files"
}
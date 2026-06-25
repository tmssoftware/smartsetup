# check detached head states, also cleaning up changes.
. test.setup

tms server-enable tms false
tms server-add testserver zipfile "file:///$($tmsTestRootDir.Replace('\', '/'))/tmp-run/test-repos/tmsbuild_test_repos.zip"
tms config-write -p:"tms smart setup options:error if skipped=false"

Test-CommandFails {
    tms install test.tag.tag.and.branch.git:pain
} "*Can't find the version ""pain"" in product test.tag.tag.and.branch.git*"

tms install test.tag.tag.and.branch.git:master
#check head is not in detached state
$head = git -C "Products\test.tag.tag.and.branch.git\src" rev-parse --abbrev-ref HEAD
if ($head -ne "master") {
    throw "Expected to be on branch 'master', but got '$head'"
}

tms update
#check head is not in detached state
$head = git -C "Products\test.tag.tag.and.branch.git\src" rev-parse --abbrev-ref HEAD
if ($head -ne "master") {
    throw "Expected to be on branch 'master', but got '$head'"
}

tms install test.tag.tag.and.branch.git:main
#check head is not in detached state
$head = git -C "Products\test.tag.tag.and.branch.git\src" rev-parse --abbrev-ref HEAD
if ($head -ne "main") {
    throw "Expected to be on branch 'main', but got '$head'"
}

#check the the html returned by `tms log-view -print` doesn't contain a box-error css class
$logViewFileName = tms log-view -print
$logViewHtml = Get-Content -Path $logViewFileName | Out-String
if ($logViewHtml -like "*box-error""*") {
    throw "Expected log view html to not contain 'box-error' class, but it does"
}

tms update
#check head is not in detached state
$head = git -C "Products\test.tag.tag.and.branch.git\src" rev-parse --abbrev-ref HEAD
if ($head -ne "master") {
    throw "Expected to be on branch 'master', but got '$head'"
}

tms install test.tag.tag.and.branch.git:2.1.0
#check head is not in detached state, 2.1.0 is a branch besides a tag, and branches have priority
$head = git -C "Products\test.tag.tag.and.branch.git\src" rev-parse --abbrev-ref HEAD
if ($head -ne "heads/2.1.0") {
    throw "Expected to be on branch 'heads/2.1.0', but got '$head'"
}

tms install test.tag.tag.and.branch.git:2.0.0-beta.2
#check head is in detached state
$head = git -C "Products\test.tag.tag.and.branch.git\src" rev-parse --abbrev-ref HEAD
if ($head -ne "HEAD") {
    throw "Expected to be in detached head state, but got '$head'"
}

#create a new file so the repo is dirty, and also modify a file
New-Item -Path "Products\test.tag.tag.and.branch.git\src\newfile.txt" -ItemType File -Value "test" | Out-Null
Set-Content -Path "Products\test.tag.tag.and.branch.git\src\src\uTagTagAndBranch.pas" -Value "test"
#check that utagtagandbranch.pas is modified and newfile.txt exists
$content = Get-Content -Path "Products\test.tag.tag.and.branch.git\src\src\uTagTagAndBranch.pas"
if ($content -ne "test") {
    throw "Expected uTagTagAndBranch.pas to be modified, but got '$content'"
}
if (-not (Test-Path -Path "Products\test.tag.tag.and.branch.git\src\newfile.txt")) {
    throw "Expected newfile.txt to exist, but it does not"
}

tms install test.tag.tag.and.branch.git:2.1.0
#check head is not in detached state, 2.1.0 is a branch besides a tag, and branches have priority
$head = git -C "Products\test.tag.tag.and.branch.git\src" rev-parse --abbrev-ref HEAD
if ($head -ne "heads/2.1.0") {
    throw "Expected to be on branch 'heads/2.1.0', but got '$head'"
}
#check changes are discarded
if (Test-Path -Path "Products\test.tag.tag.and.branch.git\src\newfile.txt") {
    throw "Expected newfile.txt to be deleted, but it exists"
}
#while we are at branch 2.1.0, the tag is actually 2.0.0-beta.1 and that's what is inside the file.
$content = Get-Content -Path "Products\test.tag.tag.and.branch.git\src\src\uTagTagAndBranch.pas" | Out-String
if ($content -notlike "*Result := '2.0.0-beta.1'*") {
    throw "Expected uTagTagAndBranch.pas to be reverted, but got '$content'"
}

tms install test.tag.tag.and.branch.git:2.1.0_beta+1
tms install test.tag.tag.and.branch.git:2.1.0
tms install test.tag.tag.and.branch.git:2.1.0_beta+1

New-Item -Path "Products\test.tag.tag.and.branch.git\src\newfile.txt" -ItemType File -Value "test" | Out-Null
Set-Content -Path "Products\test.tag.tag.and.branch.git\src\src\uTagTagAndBranch.pas" -Value "test"
tms update
#check head is not in detached state, update should switch back to master branch
$head = git -C "Products\test.tag.tag.and.branch.git\src" rev-parse --abbrev-ref HEAD
if ($head -ne "master") {
    throw "Expected to be on branch 'master' after update, but got '$head'"
}
#check changes are discarded
if (Test-Path -Path "Products\test.tag.tag.and.branch.git\src\newfile.txt") {
    throw "Expected newfile.txt to be deleted after update, but it exists"
}   
$content = Get-Content -Path "Products\test.tag.tag.and.branch.git\src\src\uTagTagAndBranch.pas" | Out-String
if ($content -notlike "*Result := '2.1.0'*") {
    throw "Expected uTagTagAndBranch.pas to be reverted after update, but got '$content'"
}

Set-Content -Path "Products\test.tag.tag.and.branch.git\src\src\uTagTagAndBranch.pas" -Value "test"
Invoke-WithExitCodeIgnored{ tms build }

$logViewFileName = tms log-view -print
$logViewHtml = Get-Content -Path $logViewFileName | Out-String
if ($logViewHtml -notlike "*box-error""*") {
    throw "Expected log view html to contain 'box-error' class, but it does not"
}

tms install test.tag.tag.and.branch.git:2.1.0_beta+1
tms update
uninstall_and_check(0)
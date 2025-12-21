# verify the auto snapshot functionality works as expected

. test.setup
tmscredentials
tms server-enable community

$snapshotFile = Join-Path "snapshots" "auto-snapshot-test.yaml"

Test-CommandFails {tms restore} "*Please specify the filename of the snapshot to restore.*"

tms config-write -p:"tms smart setup options:auto snapshot filenames=[$snapshotFile]"

tms fetch tms.vcl.query:1.18.0.0 pbear.htmlviewer:11.7 pyscripter.python4delphi tms.fnc.uipack

if (-Not (Test-Path $snapshotFile)) {
    Write-Error "Snapshot file $snapshotFile was not created."
}

Copy-Item $snapshotFile "./snapshot1.yaml" -Force

tms pin tms.vcl.query

Copy-Item $snapshotFile "./snapshot2.yaml" -Force

compare-files_diff $snapshotFile "./snapshot1.yaml"

$Diff = Compare-Object (Get-Content $snapshotFile) (Get-Content "./snapshot1.yaml")

if ($Diff.Count -ne 2) {
    Write-Error "Snapshot file $snapshotFile should have only two differences after pinning tms.vcl.query, but it has $($Diff.Count) differences."
}
if ($Diff[0].SideIndicator -ne "=>" -or $Diff[1].SideIndicator -ne "<=") {
    Write-Error "Snapshot file $snapshotFile differences after pinning tms.vcl.query are not as expected."
}

if ($Diff[0].InputObject -notmatch "pinned: false" -or $Diff[1].InputObject -notmatch "pinned: true") {
    Write-Error "Snapshot file $snapshotFile differences after pinning tms.vcl.query do not show the expected pinned value changes."
}

Set-Location "Products/pyscripter.python4delphi/src"
$hash = git rev-parse HEAD
Set-Location "../../.."

uninstall_and_check 0

$Contents = Get-Content $snapshotFile
if ($Contents.Count -ne 4) {
    Write-Error "Snapshot file $snapshotFile should have 4 lines after uninstalling all products, but it has $($Contents.Count) lines."
}

tms restore "./snapshot2.yaml" -full -nobuild  # do not build, since newer delphi versions might not support the versions in this test, and break it. This test only checks fetching, never building.
compare-files $snapshotFile "./snapshot2.yaml"

$results = tms list -json | ConvertFrom-Json -AsHashtable

if ($results.Count -ne 5) {
    Write-Error "After restoring from snapshot, there should be 5 products installed, but there are $($results.Count)."
}

if ($results["tms.vcl.query"].version -ne "1.18.0.0") {
    Write-Error "After restoring from snapshot, tms.vcl.query should be version 1.18.0.0."
}

if ($results["tms.vcl.query"].pinned -ne $true) {
    Write-Error "After restoring from snapshot, tms.vcl.query should be pinned."
}

if ($results["pbear.htmlviewer"].version -ne "11.7") {
    Write-Error "After restoring from snapshot, pbear.htmlviewer should be version 11.7."
}
if ($results["pbear.htmlviewer"].pinned -ne $false) {
    Write-Error "After restoring from snapshot, pbear.htmlviewer should not be pinned."
}

if ($results["pyscripter.python4delphi"].version -ne $hash) {
    Write-Error "After restoring from snapshot, pyscripter.python4delphi should be at the saved version."
}

if ($results["pyscripter.python4delphi"].pinned -ne $false) {
    Write-Error "After restoring from snapshot, pyscripter.python4delphi should not be pinned."
}

tms pin *
# get time when $snapshotFile was last modified
$LastModified = (Get-Item $snapshotFile).LastWriteTime

#delay for 2 seconds to ensure file timestamp changes
Start-Sleep -Seconds 2
tms update
$NewLastModified = (Get-Item $snapshotFile).LastWriteTime

# If there are no changes, we don't want the snapshot to be recreated, even with the same contents.
if ($NewLastModified -ne $LastModified) {
    Write-Error "Snapshot file $snapshotFile should not change after updating, as all was pinned."
}

tms unpin tms.vcl.query
Copy-Item $snapshotFile "./snapshot3.yaml" -Force
tms restore -nobuild
compare-files_diff $snapshotFile "./snapshot3.yaml"

Copy-Item "./snapshot3.yaml" $snapshotFile -Force

tms restore -full -nobuild
compare-files $snapshotFile "./snapshot3.yaml"


# tests generating multiple snapshots automatically

. test.setup
tmscredentials
tms server-enable community

$snapshotFile = @( 
    Join-Path "snapshots" "snapshots1.yaml"
    Join-Path "snapshots" "snapshots2.yaml"
    Join-Path "snapshots3" "snapshots3.yaml"
)

tms config-write -p:"tms smart setup options:auto snapshot filenames=[$($snapshotFile -join ',')]"
tms fetch tms.vcl.query

foreach ($file in $snapshotFile) {
    if (-Not (Test-Path $file)) {
        Write-Error "Snapshot file $file was not created."
    }
    compare-files $file $snapshotFile[0]
}

tms fetch tms.fnc.uipack
if (-Not (Test-Path $snapshotFile[1])) {
    Write-Error "Snapshot file $($snapshotFile[1]) was not created after fetching second product."
}

foreach ($file in $snapshotFile) {
    if (-Not (Test-Path $file)) {
        Write-Error "Snapshot file $file was not created."
    }
    compare-files $file $snapshotFile[0]
}

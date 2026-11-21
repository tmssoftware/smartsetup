# check a simple backup of tms and community projects with versions

. test.setup

tms server-enable community

$berosnapshot = "15f4b5982f6e5c0ad8e0eeb92ab540739e50d623"
$aistudioVersion = "2.0.2.2"

tms backup .\tms-backup-today.snapshot.zip -include:tms.dev.aistudio:$aistudioVersion -include:bero.boldfordelphi:$berosnapshot

$result = tms backup-info .\tms-backup-today.snapshot.zip -json | ConvertFrom-Json -AsHashtable

if ($result.products.count -ne 2) {
    throw "Expected 2 products in backup, but got $($result.products.count)"
}


if ($result.products."bero.boldfordelphi".version -ne $berosnapshot) {
    throw "Expected bero.boldfordelphi version to be $berosnapshot, but got $($result.products."bero.boldfordelphi".version)"
}

if ($result.products."tms.dev.aistudio".version -ne $aistudioVersion) {
    throw "Expected tms.dev.aistudio version to be $aistudioVersion, but got $($result.products."tms.dev.aistudio".version)"
}

tms restore .\tms-backup-today.snapshot.zip -skip-register

$installed = tms list -json | ConvertFrom-Json -AsHashtable

if ($installed."bero.boldfordelphi".version -ne $berosnapshot) {
    throw "Expected installed bero.boldfordelphi version to be $berosnapshot, but got $($installed."bero.boldfordelphi".version)"
}
if ($installed."tms.dev.aistudio".version -ne $aistudioVersion) {
    throw "Expected installed tms.dev.aistudio version to be $aistudioVersion, but got $($installed."tms.dev.aistudio".version)"
}
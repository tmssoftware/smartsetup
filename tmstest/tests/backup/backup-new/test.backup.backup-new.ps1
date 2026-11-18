# check backups from repo

. test.setup
tms server-enable community

tms config-write -p:"tms smart setup options:versions to keep=0"

$previousScripterVersion = (tms versions-remote tms.biz.scripter -json | ConvertFrom-Json).psobject.Properties.Name | Select-Object -First 1 -Skip 1
$previousJclVersion = (tms versions-remote jedi.jcl -json | ConvertFrom-Json).psobject.Properties.Name | Select-Object -First 1 -Skip 1

tms install tms.biz.scripter:$previousScripterVersion
tms pin *
tms backup ./all.snapshot.zip -include:tms.biz.sparkle -include:c4d.conn -include:jedi.jcl:$previousJclVersion -include:xor-el.crypto-lib-4-pascal -include:tms.biz.scripter
tms uninstall *
tms restore ./all.snapshot.zip -test-offline -skip-register

$installed = tms list -json | ConvertFrom-Json -AsHashtable
if ($installed.Count -ne 8) {
    Write-Error "After restoring from snapshot, there should be 8 products installed, but there are $($installed.Count)."
}

if ($installed["tms.biz.scripter"].version -eq $previousScripterVersion) {
    Write-Error "After restoring from snapshot, tms.biz.scripter should not be version $previousScripterVersion."
}

if ($installed["jedi.jcl"].version -ne $previousJclVersion) {
    Write-Error "After restoring from snapshot, jedi.jcl should be version $previousJclVersion, but it is $($installed["jedi.jcl"].version)."
}
 
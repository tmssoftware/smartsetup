# Tests that we can restore from a snapshot correctly

. test.setup
tmscredentials
tms server-enable community

tms install tms.biz.echo vsoft.*

$vsoftCount = ((tms list -json | ConvertFrom-Json -AsHashtable).Keys | Where-Object { $_ -like "vsoft.*" }).Count

$previousAureliusVersion = (tms versions-remote tms.biz.aurelius -json | ConvertFrom-Json).psobject.Properties.Name | Select-Object -First 1 -Skip 1
$previousYamlVersion = (tms versions-remote vsoft.yaml -json | ConvertFrom-Json).psobject.Properties.Name | Select-Object -First 1 -Skip 1

tms update tms.biz.aurelius:$previousAureliusVersion
tms update vsoft.yaml:$previousYamlVersion
tms pin tms.biz.bcl

#not fetched products
Remove-Item -Path ("Products/vsoft.cancellationtoken/tmsfetch.info.txt") -Force
Remove-Item -Path ("Products/tms.biz.echo/tmsfetch.info.txt") -Force

tms snapshot "snapshot1.yaml"

# uninstall everything. we removed tmsfetch for 2 products, so those will stay 
tms uninstall *
$installed = tms list -json | ConvertFrom-Json -AsHashtable
if ($installed.Count -ne 2) {
    Write-Error "All products except 2 should have been uninstalled, but $($installed.Count) remain."
}

# delete the full Products folder
Remove-Item -Path "Products" -Recurse -Force
tms build

tms restore "snapshot1.yaml" -nobuild

$installed = tms list -json | ConvertFrom-Json -AsHashtable
if ($installed.Count -ne 9) {
    Write-Error "After restoring from snapshot, there should be 9 products installed, but there are $($installed.Count)."
}

uninstall_and_check 0

tms restore "snapshot1.yaml" -full -exclude:tms.biz.* -exclude:vsoft.commandline -nobuild
$installed = tms list -json | ConvertFrom-Json -AsHashtable
if ($installed.Count -ne $vsoftCount - 1) {
    Write-Error "After restoring from snapshot, there should be $($vsoftCount - 1) products installed, but there are $($installed.Count)."
}

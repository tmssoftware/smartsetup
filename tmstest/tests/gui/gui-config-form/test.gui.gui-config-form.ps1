#check basic config form functionality

. test.setup
Test-tmsgui basic

$cfg = tms config-read
$refCfg = Get-Content -Path "./reference/basic-cfg.txt" -Encoding UTF8
DiffText -ReferenceObject $refCfg -DifferenceObject $cfg

Test-tmsgui remove-server

$cfg = tms config-read
$refCfg = Get-Content -Path "./reference/remove-server-cfg.txt" -Encoding UTF8
DiffText -ReferenceObject $refCfg -DifferenceObject $cfg

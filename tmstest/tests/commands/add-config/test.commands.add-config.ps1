# Checks inherited configuration files.

. test.setup
tmscredentials

$baseConfig = tms config-read
$addConfig = tms config-read -add-config:tms.config.extra1.yaml

$diff = Compare-Object -ReferenceObject $baseConfig -DifferenceObject $addConfig
if ($diff -ne $null -and $diff.Count -ne 0) {
    throw "The configuration with add-config extra1 should be the same, it is empty."
}

$addConfig2 = tms config-read -add-config:tms.config.extra2.yaml
$diff2 = Compare-Object -ReferenceObject $baseConfig -DifferenceObject $addConfig2
if ($diff2.Count -ne 15) {
    throw "The configuration with add-config extra2 should have 15 differences, but it has $($diff2.Count)."
}

$addConfig3 = tms config-read  -add-config:tms.config.extra1.yaml -add-config:tms.config.extra2.yaml -add-config:tms.config.extra3.yaml -json | ConvertFrom-Json -AsHashtable

$addConfig3['tms smart setup options']['build cores'] | Assert-ValueIs 9
$addConfig3['tms smart setup options']['alternate registry key'] | Assert-ValueIs ""
$addConfig3['tms smart setup options']['excluded products'] | Assert-ValueIs @()
@(Compare-Object $addConfig3['tms smart setup options']['auto snapshot filenames'] @('potato.snapshot.yaml')).Count | Assert-ValueIs 0
$addConfig3['tms smart setup options']['servers']['tms']['enabled'] | Assert-ValueIs $false
$addConfig3['tms smart setup options']['servers']['community']['enabled'] | Assert-ValueIs $false
$addConfig3['configuration for my product']['options']['verbosity'] | Assert-ValueIs 'trace'
$addConfig3['configuration for my product']['options']['skip register'] | Assert-ValueIs $true
$addConfig3['configuration for my product']['options']['dry run'] | Assert-ValueIs $false
@(Compare-Object $addConfig3['configuration for my product']['delphi versions'] @('delphi11')).Count | Assert-ValueIs 0
@(Compare-Object $addConfig3['configuration for my product']['platforms'] @('win32intel','win64intel')).Count | Assert-ValueIs 0

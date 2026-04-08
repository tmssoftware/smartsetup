# test if a local config file is loaded automatically.

. test.setup

$cfg = tms config-read -json | ConvertFrom-Json -AsHashtable
foreach ($entry in $cfg.GetEnumerator()) {
     Write-Host "Checking configuration section: '$($entry.Key)'" -ForegroundColor Green
    if ($entry.Key -eq "tms smart setup options") {
        $entry.Value['build cores'] | Assert-ValueIs 7
        $entry.Value['alternate registry key'] | Assert-ValueIs "local-key"
    }
    elseif ($entry.Key -eq "configuration for all products") {
        $entry.Value['options']['verbosity'] | Assert-ValueIs 'info'
        $entry.Value['options']['skip register'] | Assert-ValueIs $true
        $entry.Value['options']['dry run'] | Assert-ValueIs $true
        $entry.Value['delphi versions'] | Assert-ValueIs @('delphi2005')
    }
    else {
        throw "Unexpected configuration section: '$($entry.Key)'"
    }
}

$cfg = tms config-read -config:config1.yaml -json | ConvertFrom-Json -AsHashtable
foreach ($entry in $cfg.GetEnumerator()) {
     Write-Host "Checking configuration section: '$($entry.Key)'" -ForegroundColor Green
    if ($entry.Key -eq "tms smart setup options") {
        $entry.Value['build cores'] | Assert-ValueIs 17
        $entry.Value['alternate registry key'] | Assert-ValueIs "key1"
    }
    elseif ($entry.Key -eq "configuration for all products") {
        $entry.Value['options']['verbosity'] | Assert-ValueIs 'trace'
        $entry.Value['options']['skip register'] | Assert-ValueIs $false
        $entry.Value['options']['dry run'] | Assert-ValueIs $false
        $entry.Value['delphi versions'] | Assert-ValueIs @('delphi11')
    }
    else {
        throw "Unexpected configuration section: '$($entry.Key)'"
    }
}

$cfg = tms config-read -config:config2.yaml -json | ConvertFrom-Json -AsHashtable
foreach ($entry in $cfg.GetEnumerator()) {
     Write-Host "Checking configuration section: '$($entry.Key)'" -ForegroundColor Green
    if ($entry.Key -eq "tms smart setup options") {
        $entry.Value['build cores'] | Assert-ValueIs 22
        $entry.Value['alternate registry key'] | Assert-ValueIs "key1"
    }
    elseif ($entry.Key -eq "configuration for all products") {
        $entry.Value['options']['verbosity'] | Assert-ValueIs 'trace'
        $entry.Value['options']['skip register'] | Assert-ValueIs $false
        $entry.Value['options']['dry run'] | Assert-ValueIs $false
        $entry.Value['delphi versions'] | Assert-ValueIs @('delphi13')
    }
    else {
        throw "Unexpected configuration section: '$($entry.Key)'"
    }
}
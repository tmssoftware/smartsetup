# Tests the output from tms config-read -cmd

. test.setup
tmscredentials

#recursively loop over all entries in $json and check that they exist in $cmd output
function Test-AllJsonEntriesAreInCmd {
    param(
        [Parameter(Mandatory, Position=0)] $cmd,
        [Parameter(Mandatory, Position=1)] [hashtable] $json,
        [Parameter(Position=2)] [string] $parentKey = ""
    )

    foreach ($key in $json.Keys) {
        $fullKey = if ($parentKey -ne "") { "$($parentKey):$key" } else { $key }
        $value = $json[$key]

        if ($value -is [hashtable]) {
            # Recursive call for nested hashtables
            Test-AllJsonEntriesAreInCmd -cmd $cmd -json $value -parentKey $fullKey
        }
        else {
            # Check if the key exists in the cmd output
            $pattern = "^-p:""$($fullKey) =.*$"
            if (-not ($cmd -match $pattern)) {
                throw "Key '$fullKey' with value '$value' not found in tms config-read -cmd output."
            }
        }
    }
}

function Test-AllCmdEntriesAreInJson {
    param(
        [Parameter(Mandatory, Position=0)] $cmd,
        [Parameter(Mandatory, Position=1)] [hashtable] $json,
        [Parameter(Position=2)] [int] $skipJson = 0
    )

    foreach ($line in $cmd) {
        if ($line -match "^(.*) =(.*)$") {
            $fullKey = $matches[1]
            $value = $matches[2]

            $fullKey = $fullKey.Trim().Substring(4) # remove -p:"

            # Navigate through the hashtable using the fullKey
            $keys = $fullKey -split ":" 
            $current = $json
            
            if ($keys.Count - 2 -ge $skipJson) {
                foreach ($key in $keys[$skipJson..($keys.Count - 2)]) {
                    if ($current.ContainsKey($key)) {
                        $current = $current[$key]
                    }
                    else {
                        throw "Key '$fullKey' not found in JSON configuration."
                    }
                }
            }

            $finalKey = $keys[-1]
            if (-not $current.ContainsKey($finalKey)) {
                throw "Key '$fullKey' not found in JSON configuration."
            }
        }
        else {
            throw "Line '$line' in cmd output is not in the expected 'key = value' format."
        }
    }
}

$cmd = tms config-read -cmd
$json = tms config-read -json | ConvertFrom-Json -AsHashtable
Test-AllJsonEntriesAreInCmd -cmd $cmd -json $json
Test-AllCmdEntriesAreInJson -cmd $cmd -json $json

$cmd2 = tms config-read -cmd "tms smart setup options"
$json2 = tms config-read -json "tms smart setup options" | ConvertFrom-Json -AsHashtable
$cmd2.count -eq $cmd.count | Assert-ValueIs $false
Test-AllJsonEntriesAreInCmd -cmd $cmd2 -json $json2 -parentKey "tms smart setup options"
Test-AllCmdEntriesAreInJson -cmd $cmd2 -json $json2 -skipJson 1

$allCommands = tms config-read -cmd -config:random.config.yaml
foreach ($line in $allCommands) {
    tms config-write $line.Replace('"', "'")
}
$reReadCommands = tms config-read -cmd

Compare-Object -ReferenceObject $allCommands -DifferenceObject $reReadCommands | Measure-Object | Select-Object -ExpandProperty Count | Assert-ValueIs 0
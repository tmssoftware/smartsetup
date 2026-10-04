# Check that comments inside strings are not treated as comments.

. test.setup


function Check-Write {
    param (
        [string]$value,
        [boolean]$quoted = $false,
        [string]$expectedValue = $value
    )
    tms config-write -p:tms-smart-setup-options:git:clone-command=$value
    $prodfolder = tms config-read tms-smart-setup-options:git:clone-command
    
    if ($quoted) {
        $expectedValue = '''' + $expectedValue + ''''
    }
    if ($prodfolder -ne  $expectedValue) {
        throw "Expected git:clone-command to be '$expectedValue', but got $prodfolder"
    }
}

function Check-Write-Array {
    param (
        [string]$value,
        [string[]]$items
    )
      tms spec -non-interactive -template:.\tmsbuild-0.yaml -s:"paths:extra-cpp-include-paths=$value" -json
    
      #read tmsbuild.json
      $tmsbuildJson = Get-Content -Path .\tmsbuild.json -Raw | ConvertFrom-Json -AsHashtable
      $cpppaths = $tmsbuildJson['paths']['extra cpp include paths']
    if ($items.Count -ne $cpppaths.Count) {
        throw "Expected extra cpp include paths to have $($items.Count) items, but got $($cpppaths.Count)"
    }
    for ($i = 0; $i -lt $items.Count; $i++) {
        $expectedItem = $items[$i]
        if ($cpppaths[$i] -ne $expectedItem) {
            throw "Expected extra cpp include path item $($i) to be '$expectedItem', but got '$($cpppaths[$i])'"
        }
    }

    $expectedPackageFoldersNames = @('delphirio', 'delphisydney+', 'delphi12+', 'delphi13+')
    $expectedPackageFoldersValues = @('Rad Studio 10.3 Rio', 'number', 'number #1 and more', 'number #1 and more #another comment')
    $packageFolders = $tmsbuildJson['package options']['package folders']
    if ($packageFolders.Count -ne $expectedPackageFoldersNames.Count) {
        throw "Expected package folders to have $($expectedPackageFoldersNames.Count) items, but got $($packageFolders.Count)"
    }
    for ($i = 0; $i -lt $expectedPackageFoldersNames.Count; $i++) {
        if ($packageFolders[$expectedPackageFoldersNames[$i]] -ne $expectedPackageFoldersValues[$i]) {
            throw "Expected package folder item $($i) to be '$expectedPackageFoldersValues[$i]', but got '$($packageFolders[$expectedPackageFoldersNames[$i]])'"
        }
    }

}

function Check-WriteJson {
    param (
        [string]$value
    )
    tms config-write -p:tms-smart-setup-options:git:clone-command=$value
    $prodfolder = tms config-read tms-smart-setup-options:git:clone-command -json | ConvertFrom-Json -AsHashtable
    
    if ($prodfolder -ne $value) {
        throw "Expected git:clone-command to be '$value', but got $prodfolder"
    }
}


Check-Write '#potato' $true
Check-Write 'tomato #potato' $true
Check-Write 'tomato#potato' $false
Check-Write '#tomatopotato' $true
Check-Write 'tomato # potato #tomato#' $true
Check-Write "doesn't exist" $false
Check-Write "doesn ' t exist" $false
Check-Write "rock-'n-roll" $false
Check-Write "rock,'n,'roll" $false
Check-Write "rock,'n, #,'roll" $false "'rock,''n, #,''roll'"
Check-Write "rock # roll" $true
Check-Write "' rock roll '" $false

#copy the file tms.config-0.yaml to tms.config.yaml, and check that the values are read correctly.
Copy-Item -Path "./tms.config-0.yaml" -Destination "./tms.config.yaml" -Force
$prodfolder = tms config-read tms-smart-setup-options:git:clone-command
if ($prodfolder -ne "rock,'n,") {
    throw "Expected git:clone-command to be 'rock,'n, ', but got $prodfolder"
}


Check-WriteJson '#potato'
Check-WriteJson 'tomato #potato'
Check-WriteJson 'tomato#potato'
Check-WriteJson 'tomato # potato #tomato#'
Check-WriteJson "doesn't exist" 
Check-WriteJson "rock-'n-roll" 
Check-WriteJson "rock,'n-roll" 

Check-Write-Array '[a,b,c]' @('a','b','c') 
Check-Write-Array '[#potato,tomato #potato, tomato#potato]' @('#potato','tomato #potato','tomato#potato') 
Check-Write-Array "[a-'b,,'don''t', 'tomato #potato']" @('a-''b', "", "don't", "tomato #potato")  
Check-Write-Array "[a-'b,'don''t', m #i- #ne]" @('a-''b', "don't", "m #i- #ne")  

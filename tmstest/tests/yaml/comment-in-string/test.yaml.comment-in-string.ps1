# Check that comments inside strings are not treated as comments.

. test.setup


function Test-Write {
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

function Test-Write-Array {
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

function Test-WriteJson {
    param (
        [string]$value
    )
    tms config-write -p:tms-smart-setup-options:git:clone-command=$value
    $prodfolder = tms config-read tms-smart-setup-options:git:clone-command -json | ConvertFrom-Json -AsHashtable
    
    if ($prodfolder -ne $value) {
        throw "Expected git:clone-command to be '$value', but got $prodfolder"
    }
}


Test-Write '#potato' $true
Test-Write 'tomato #potato' $true
Test-Write 'tomato#potato' $false
Test-Write '#tomatopotato' $true
Test-Write 'tomato # potato #tomato#' $true
Test-Write "doesn't exist" $false
Test-Write "doesn ' t exist" $false
Test-Write "rock-'n-roll" $false
Test-Write "rock,'n,'roll" $false
Test-Write "rock,'n, #,'roll" $false "'rock,''n, #,''roll'"
Test-Write "rock # roll" $true
Test-Write "' rock roll '" $false

#copy the file tms.config-0.yaml to tms.config.yaml, and check that the values are read correctly.
Copy-Item -Path "./tms.config-0.yaml" -Destination "./tms.config.yaml" -Force
$prodfolder = tms config-read tms-smart-setup-options:git:clone-command
if ($prodfolder -ne "rock,'n,") {
    throw "Expected git:clone-command to be 'rock,'n, ', but got $prodfolder"
}


Test-WriteJson '#potato'
Test-WriteJson 'tomato #potato'
Test-WriteJson 'tomato#potato'
Test-WriteJson 'tomato # potato #tomato#'
Test-WriteJson "doesn't exist" 
Test-WriteJson "rock-'n-roll" 
Test-WriteJson "rock,'n-roll" 

Test-Write-Array '[a,b,c]' @('a','b','c') 
Test-Write-Array '[#potato,tomato #potato, tomato#potato]' @('#potato','tomato #potato','tomato#potato') 
Test-Write-Array "[a-'b,,'don''t', 'tomato #potato']" @('a-''b', "", "don't", "tomato #potato")  
Test-Write-Array "[a-'b,'don''t', m #i- #ne]" @('a-''b', "don't", "m #i- #ne")  

tms config-write -p:tms-smart-setup-options:git:clone-command="none" -add-config:tms.config-1.yaml -add-config:tms.config-2.yaml

$result = tms config-read "tms smart setup options:excluded products"

if ($result -ne "['tms.e # xample1',' #tms.  example2']")
{
    throw "error reading excluded products: got '" + $result + "'"  
}

$result = tms config-read "configuration for all products:options:skip register"
if ($result -ne '[startmenu]')
{
    throw "error reading skip register: got '" + $result + "'"  
}

$result = Invoke-WithExitCodeIgnored {tms list -config:tms.config-3.yaml}

if (-not ($result -join "`n").Contains('Invalid value: "x" for tag "a:b"') )
{
    throw "Invalid parse: The key should be 'a:b' and was: " + $result
}

# tmsbuild-1.yaml has:
#  - a flow array with # and : inside quoted strings, and a comment after it.
#  - array items ending with a lone #.
#  - a quoted key containing a colon and a #.
tms spec -non-interactive -template:.\tmsbuild-1.yaml -json
$tmsbuildJson = Get-Content -Path .\tmsbuild.json -Raw | ConvertFrom-Json -AsHashtable

$expectedCppIncludePaths = @('a #b', 'c:d', 'e', 'f ] #g')
$cppIncludePaths = $tmsbuildJson['paths']['extra cpp include paths']
if (($cppIncludePaths -join '|') -ne ($expectedCppIncludePaths -join '|')) {
    throw "Expected extra cpp include paths to be '$($expectedCppIncludePaths -join '|')', but got '$($cppIncludePaths -join '|')'"
}

$expectedDelphiLibraryPaths = @('path1', 'path2\a\b', "@linux64,win64intel: p'ath3''")
$delphiLibraryPaths = $tmsbuildJson['paths']['extra delphi library paths']
if (($delphiLibraryPaths -join '|') -ne ($expectedDelphiLibraryPaths -join '|')) {
    throw "Expected extra delphi library paths to be '$($expectedDelphiLibraryPaths -join '|')', but got '$($delphiLibraryPaths -join '|')'"
}

$dependency = $tmsbuildJson['dependencies'] | Where-Object { $_.ContainsKey('tms.example:4 #x') }
if ($null -eq $dependency) {
    throw "Expected a dependency named 'tms.example:4 #x', but got: $($tmsbuildJson['dependencies'] | ConvertTo-Json -Compress)"
}
if ($dependency['tms.example:4 #x'] -ne 'TMS # Example 4') {
    throw "Expected dependency 'tms.example:4 #x' to be 'TMS # Example 4', but got '$($dependency['tms.example:4 #x'])'"
}
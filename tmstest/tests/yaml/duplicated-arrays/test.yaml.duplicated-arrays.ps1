# Test to see if we can enter duplicated items in arrays

. test.setup

function Get-ExpectedError($file) {
    $content = Get-Content $file
    
    return $content[0].Substring(1).Trim()
}

$result = Invoke-WithExitCodeIgnored { tms config-write -p:tms-smart-setup-options:excluded-products=[potato.salad,potato.salad] }
if (-not ($result -join "`n").Contains('Error: Duplicated item in section excluded products: "potato.salad" is already defined. In parameter: "tms-smart-setup-options:excluded-products=[potato.salad,potato.salad]".'))
{
    throw "Duplicated items in array configuration for   all products :compilation options:add-defines=[MYDEF,MYDEF] failed"
}

tms config-write -p:tms-smart-setup-options:excluded-products=[potato.salad]

$result = Invoke-WithExitCodeIgnored { tms config-write -p:tms-smart-setup-options:add_excluded-products=[potato.salad] }
if (-not ($result -join "`n").Contains('Error: Duplicated item in section excluded products: "potato.salad" is already defined. In parameter: "tms-smart-setup-options:add_excluded-products=[potato.salad]".'))
{
    throw "Duplicated items in array configuration for   all products :compilation options:add-defines=[MYDEF,MYDEF] failed"
}

tms config-write -p:tms-smart-setup-options:replace_excluded-products=[potato.salad]
$result = Invoke-WithExitCodeIgnored { tms config-write -p:tms-smart-setup-options:add_excluded-products=[potato.salad,potato.salad] }
if (-not ($result -join "`n").Contains('Error: Duplicated item in section excluded products: "potato.salad" is already defined. In parameter: "tms-smart-setup-options:add_excluded-products=[potato.salad,potato.salad]".'))
{
    throw "Duplicated items in array configuration for   all products :compilation options:add-defines=[MYDEF,MYDEF] failed"
}


$result = Invoke-WithExitCodeIgnored{ tms config-write -p:"configuration for   all products :compilation options:add-defines=[MYDEF,MYDEF]" }
if (-not ($result -join "`n").Contains('Error: Duplicated item in section defines: "MYDEF" is already defined. In parameter: "configuration for   all products :compilation options:add-defines=[MYDEF,MYDEF]".'))
{
    throw "Duplicated items in array configuration for   all products :compilation options:add-defines=[MYDEF,MYDEF] failed"
}

tms config-write -p:"configuration for   all products :compilation options:add-defines=[MYDEF]"

$result = Invoke-WithExitCodeIgnored{ tms config-write -p:"configuration for   all products :compilation options:add-defines=[MYDEF]" }
if (-not ($result -join "`n").Contains('Error: Duplicated item in section defines: "MYDEF" is already defined. In parameter: "configuration for   all products :compilation options:add-defines=[MYDEF]".'))
{
    throw "Duplicated items in array configuration for   all products :compilation options:add-defines=[MYDEF] failed"
}

$result = Invoke-WithExitCodeIgnored{ tms spec -non-interactive -s:"paths:add-extra debug dcu path=[temp\debug]" -s:"paths:add-extra debug dcu path=[temp\debug]"}
if (-not ($result -join "`n").Contains('Error: Duplicated item in section extra debug dcu path: "temp\debug" is already defined. In parameter: "paths:add-extra debug dcu path=[temp\debug]".'))
{
    throw 'Error: Duplicated item in section extra debug dcu path: "temp\debug" is already defined. In parameter: "paths:add-extra debug dcu path=[temp\debug]".'
}

$result = Invoke-WithExitCodeIgnored{ tms spec -non-interactive -s:"paths:extra debug dcu path=[temp\debug,temp\debug]"}
if (-not ($result -join "`n").Contains('Error: Duplicated item in section extra debug dcu path: "temp\debug" is already defined. In parameter: "paths:extra debug dcu path=[temp\debug,temp\debug]".'))
{
    throw "Duplicated items in array paths:extra debug dcu path=[temp\debug] failed"
}

tms spec -non-interactive -s:"paths:extra debug dcu path=[temp]"
tms spec -non-interactive -template:tmsbuild.yaml -s:"paths:extra debug dcu path=[temp\debug]"
$result = Invoke-WithExitCodeIgnored{ tms spec -non-interactive -template:tmsbuild.yaml -s:"paths:add-extra debug dcu path=[temp\debug]"}
if (-not ($result -join "`n").Contains('Error: Duplicated item in section extra debug dcu path: "temp\debug" is already defined. In parameter: "paths:add-extra debug dcu path=[temp\debug]".'))
{    
    throw "Duplicated items in array paths:extra debug dcu path=[temp\debug] failed"
}

$result = Invoke-WithExitCodeIgnored{ tms spec -non-interactive -s:"packages=[one:[runtime],one:[design]]"}
if (-not ($result -join "`n").Contains('Error: Duplicated item in section packages: "one" is already defined. In parameter: "packages=[one:[runtime],one:[design]]".'))
{
    throw "Invalid package specification"
}

$result = Invoke-WithExitCodeIgnored{ tms spec -non-interactive -s:"add-packages=[one:[runtime]]" -s:"add-packages=[one:[design]]"}
if (-not ($result -join "`n").Contains('Error: Duplicated item in section packages: "one" is already defined. In parameter: "add-packages=[one:[design]]".'))
{
    throw "Invalid package specification 'one:[runtime],one:[design]' for packages=[one:[runtime],one:[design]] failed"
}

tms spec -non-interactive -s:"packages=[one:[runtime]]"
tms spec -non-interactive -template:tmsbuild.yaml -s:"packages=[one:[design]]"
$result = Invoke-WithExitCodeIgnored{ tms spec -non-interactive -template:tmsbuild.yaml -s:"add-packages=[one:[runtime]]"}
if (-not ($result -join "`n").Contains('Error: Duplicated item in section packages: "one" is already defined. In parameter: "add-packages=[one:[runtime]]".'))
{
    throw "Invalid package specification"
}


# loop over all the files at the ./configs folder:
foreach ($file in Get-ChildItem -Path "./configs" -Filter "*.yaml" -Recurse)
{
    Write-Host "Testing $file.FullName"
    $expectedError = Get-ExpectedError $file.FullName
    if ($expectedError -eq "OK") {
      tms list -config:$file.FullName
    }
    else {
        $result = Invoke-WithExitCodeIgnored { tms list -config:$file.FullName }
        if (-not ($result -join "`n").Contains($expectedError)) {
            throw "Error in $file.FullName: Expected error $expectedError but got $($result -join "`n")"
        }
    }
}


# packages, which have a different implementation than the rest.
tms spec -non-interactive -template:builds/tmsbuild.11.yaml -s:"replace_packages=[l_b:[runtime]]" -json
$result = Get-Content tmsbuild.json | ConvertFrom-Json -AsHashtable
if ($result.packages.l_b -ne "runtime") {
    throw "Duplicated items in array packages=[l_b:[runtime]] failed"
}
if ($result.packages.Count -ne 1) {
    throw "Duplicated items in array packages=[l_b:[runtime]] failed"
}

$result = Invoke-WithExitCodeIgnored{ tms spec -non-interactive -template:builds/tmsbuild.11.yaml -s:"add_packages=[l_b:[runtime]]" -json}
if (-not ($result -join "`n").Contains('Duplicated item in section packages: "l_b" is already defined')) {
    throw "Duplicated items in array packages=[l_b:[runtime]] failed"
}   

tms spec -non-interactive -template:builds/tmsbuild.11.yaml -s:"add_packages=[l_c:[design]]" -json
$result = Get-Content tmsbuild.json | ConvertFrom-Json -AsHashtable
if ($result.packages[1].l_c -ne "design") {
    throw "Duplicated items in array packages=[l_c:[design]] failed"
}
if ($result.packages.Count -ne 2) {
    throw "Duplicated items in array packages=[l_c:[design]] failed"
}

# dependencies, which use the generic duplicates in TListOfActions
tms spec -non-interactive -template:builds/tmsbuild.11.yaml -s:"replace_dependencies=[tmslaztest.a:example]" -json
$result = Get-Content tmsbuild.json | ConvertFrom-Json -AsHashtable
if ($result.dependencies."tmslaztest.a" -ne "example") {
    throw "Duplicated items in array dependencies=[tms.example:example] failed"
}
if ($result.dependencies.Count -ne 1) {
    throw "Duplicated items in array dependencies=[tms.example:example] failed"
}


$result = Invoke-WithExitCodeIgnored{ tms spec -non-interactive -template:builds/tmsbuild.11.yaml -s:"add_dependencies=[tmslaztest.a: new example]" -json}
if (-not ($result -join "`n").Contains('Duplicated item in section dependencies: "tmslaztest.a" is already defined')) {
    throw "Duplicated items in array dependencies=[tmslaztest.a: new example] failed"
}
 
tms spec -non-interactive -template:builds/tmsbuild.11.yaml -s:"add_dependencies=[tms.potato:a potato]" -json
$result = Get-Content tmsbuild.json | ConvertFrom-Json -AsHashtable
if ($result.dependencies[1]."tms.potato" -ne "a potato") {
    throw "Duplicated items in array dependencies=[tms.potato:a potato] failed"
}
if ($result.dependencies.Count -ne 2) {
    throw "Duplicated items in array dependencies=[tms.potato:a potato] failed"
}


foreach ($file in Get-ChildItem -Path "./builds" -Filter "*.yaml" -Recurse)
{
    Write-Host "Testing $file.FullName"
    $expectedError = Get-ExpectedError $file.FullName
    if ($expectedError -eq "OK") {
      tms spec -non-interactive -template:$file.FullName
    }
    else {
        $result = Invoke-WithExitCodeIgnored { tms spec -non-interactive -template:$file.FullName }
        if (-not ($result -join "`n").Contains($expectedError)) {
            throw "Error in $file.FullName: Expected error $expectedError but got $($result -join "`n")"
        }
    }
}



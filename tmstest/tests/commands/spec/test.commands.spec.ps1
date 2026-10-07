# test the tms spec command

. test.setup

remove-item -path ".\tms.config.yaml"  #make sure we can run without config.
tms spec -non-interactive -template:".\product\tmsbuild.yaml" -s:"application:id=potato.salad" -cmd -test-fixed-version
Compare-Object -ReferenceObject (Get-Content -Path ".\tmsbuild.cmd") -DifferenceObject (Get-Content -Path ".\product-ref\tmsbuild.cmd") | Measure-Object | Select-Object -ExpandProperty Count | Assert-ValueIs 0
tms spec -non-interactive -template:".\product\tmsbuild.yaml" -s:"application:id=potato.salad" -json  -test-fixed-version
Compare-Object -ReferenceObject (Get-Content -Path ".\tmsbuild.json") -DifferenceObject (Get-Content -Path ".\product-ref\tmsbuild.json") | Measure-Object | Select-Object -ExpandProperty Count | Assert-ValueIs 0
tms spec -non-interactive -template:".\product\tmsbuild.yaml" -s:"application:id=potato.salad"  -test-fixed-version
Compare-Object -ReferenceObject (Get-Content -Path ".\tmsbuild.yaml") -DifferenceObject (Get-Content -Path ".\product-ref\tmsbuild.yaml") | Measure-Object | Select-Object -ExpandProperty Count | Assert-ValueIs 0

Move-Item -Path ".\tmsbuild.yaml" -Destination ".\tmsbuild.target.yaml" -Force  
Copy-Item -Path ".\tmsbuild.json" -Destination ".\tmsbuild.target.json" -Force  

#check we can read the json file generated
$Spec = Get-Content -Path ".\tmsbuild.json" | ConvertFrom-Json
if ($Spec.application.id -ne "potato.salad") {
    throw "Spec command did not generate expected application id."
}


tms spec -non-interactive
#create a version.txt file with one line
Set-Content -Path ".\version.txt" -Value "test: 1.2.3"


#read tmsbuild.cmd and for each line, execute tms spec -s with that line
$CmdLines = Get-Content -Path ".\tmsbuild.cmd"

Copy-Item -Path ".\tmsbuild.yaml" -Destination ".\tmsbuild1.yaml" -Force

$i = 1
foreach ($line in $CmdLines) {
    $i++
    $escapedLine = $line.Substring(4, $line.Length - 5)  #remove the leading -s:
    write-host "Testing with spec line: $escapedLine" -ForegroundColor Green

    tms spec -non-interactive -template:".\tmsbuild$($i-1).yaml" -s:"$escapedLine"
    Copy-Item -Path ".\tmsbuild.yaml" -Destination ".\tmsbuild$i.yaml" -Force
   
    Compare-Object -ReferenceObject (Get-Content -Path ".\tmsbuild$i.yaml") -DifferenceObject (Get-Content -Path ".\product-ref\tmsbuild$i.yaml") | Measure-Object | Select-Object -ExpandProperty Count | Assert-ValueIs 0

}
$last = "tmsbuild$($CmdLines.Count + 1)"

tms spec -non-interactive -template:.\$($last).yaml -json

# diff should be 0, but there is a bug in delphi's paramstr that doesn't allow us to pass double quotes in parameters. No way to escape them:
# https://stackoverflow.com/questions/52525969/get-parameter-with-double-quotes-using-paramstr
# We could remove those double-quoted examples from our test (it is very unlikely anyway that a text needs double quotes for yaml,
# it would need to have a \n or a literal " in the middle), but we leave them to remember this could be improved. (even if that would
# mean creating our own command line parser to replace paramstr)
Compare-Object -ReferenceObject (Get-Content -Path ".\tmsbuild.json") -DifferenceObject (Get-Content -Path ".\tmsbuild.target.json") |
   Measure-Object | Select-Object -ExpandProperty Count | Assert-ValueIs 2

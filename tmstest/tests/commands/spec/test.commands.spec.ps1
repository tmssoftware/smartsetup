# test the tms spec command

. test.setup

remove-item -path ".\tms.config.yaml"  #make sure we can run without config.
tms spec -non-interactive -template:".\product\tmsbuild.yaml" -s:"application:id=potato.salad" -cmd -test-fixed-version
Compare-Object -ReferenceObject (Get-Content -Path ".\tmsbuild.cmd") -DifferenceObject (Get-Content -Path ".\product-ref\tmsbuild.cmd") | Measure-Object | Select-Object -ExpandProperty Count | Assert-ValueIs 0
tms spec -non-interactive -template:".\product\tmsbuild.yaml" -s:"application:id=potato.salad" -json  -test-fixed-version
Compare-Object -ReferenceObject (Get-Content -Path ".\tmsbuild.json") -DifferenceObject (Get-Content -Path ".\product-ref\tmsbuild.json") | Measure-Object | Select-Object -ExpandProperty Count | Assert-ValueIs 0
tms spec -non-interactive -template:".\product\tmsbuild.yaml" -s:"application:id=potato.salad"  -test-fixed-version
Compare-Object -ReferenceObject (Get-Content -Path ".\tmsbuild.json") -DifferenceObject (Get-Content -Path ".\product-ref\tmsbuild.json") | Measure-Object | Select-Object -ExpandProperty Count | Assert-ValueIs 0

Move-Item -Path ".\tmsbuild.yaml" -Destination ".\tmsbuild.target.yaml" -Force  

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
$i = 0
foreach ($line in $CmdLines) {
    $i++
    $escapedLine = $line.Substring(4, $line.Length - 5)  #remove the leading -s:
    write-host "Testing with spec line: $escapedLine" -ForegroundColor Green
    Copy-Item -Path ".\tmsbuild.yaml" -Destination ".\tmsbuild$i.yaml" -Force
    try
    {
       tms spec -non-interactive -template:".\tmsbuild$i.yaml" -s:"$escapedLine"
    }
    catch
    {
        #check if the reference file exists, if it does, then the spec command failed, otherwise it is expected to fail.
        if (Test-Path -Path ".\product-ref\tmsbuild$i.yaml") {
          throw "tms spec failed at line $i with line: $escapedLine"
        }
        continue
    }
    Compare-Object -ReferenceObject (Get-Content -Path ".\tmsbuild$i.yaml") -DifferenceObject (Get-Content -Path ".\product-ref\tmsbuild$i.yaml") | Measure-Object | Select-Object -ExpandProperty Count | Assert-ValueIs 0

}

#without template
tms spec -non-interactive -s:"registry keys = [Software\tmssoftware\TMS WEB Core = [name = InstallDir,data = '%install-path%',name = 'No',type = dword,data = '2'],Software\tmssoftware\TMS WEB Core\Components = [name = ' on',data = 'off ']]"
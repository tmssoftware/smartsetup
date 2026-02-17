# test the tms spec command

. test.setup

remove-item -path ".\tms.config.yaml"  #make sure we can run without config.
tms spec -non-interactive -template:".\product\tmsbuild.yaml" -s:"application:id=potato.salad" -cmd
tms spec -non-interactive -template:".\product\tmsbuild.yaml" -s:"application:id=potato.salad" -json
tms spec -non-interactive -template:".\product\tmsbuild.yaml" -s:"application:id=potato.salad"

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
foreach ($line in $CmdLines) {
    $escapedLine = $line.Substring(4, $line.Length - 5)  #remove the leading -s:
    tms spec -non-interactive -template:".\tmsbuild.yaml" -s:"$escapedLine"
}